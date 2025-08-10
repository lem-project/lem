#!/usr/bin/env bash -ex
# scripts/macos-deploy.bash

set -o pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

# ===== 1) ビルド =====
qlot install
qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'

# アイコン
cp resources/lem.png bin/lem.app/Contents/Resources/ || true

# ===== 2) OpenSSL dylib を Contents/MacOS に同梱し、参照を @loader_path 化 =====
APP="bin/lem.app"
EXE="$APP/Contents/MacOS/lem"
LIBDIR="$APP/Contents/MacOS"

if [[ ! -x "$EXE" ]]; then
  echo "error: executable not found: $EXE" >&2
  exit 1
fi

if ! command -v brew >/dev/null 2>&1; then
  echo "error: Homebrew not found. Install brew before packaging." >&2
  exit 1
fi
OPENSSL_PREFIX="$(brew --prefix openssl@3)"

# deploy が同名を置いている場合があるので、意図的に上書き
cp -f "$OPENSSL_PREFIX/lib/libssl.3.dylib"    "$LIBDIR/libssl.3.dylib"
cp -f "$OPENSSL_PREFIX/lib/libcrypto.3.dylib" "$LIBDIR/libcrypto.3.dylib"

# dylib 自身の ID は @loader_path/… にしておく（どちらでも動くが一貫性のため）
install_name_tool -id @loader_path/libssl.3.dylib    "$LIBDIR/libssl.3.dylib"
install_name_tool -id @loader_path/libcrypto.3.dylib "$LIBDIR/libcrypto.3.dylib"

# libssl → libcrypto の依存を Cellar 絶対パスから @loader_path に置換
install_name_tool -change \
  "$OPENSSL_PREFIX/lib/libcrypto.3.dylib" \
  @loader_path/libcrypto.3.dylib \
  "$LIBDIR/libssl.3.dylib" || true

# ※ 実行ファイル(lem)は特殊フォーマットのため install_name_tool をかけない

# ===== 3) 署名（dylib に限定、失敗しても処理継続） =====
codesign --force --sign - --timestamp=none "$LIBDIR/libcrypto.3.dylib" || true
codesign --force --sign - --timestamp=none "$LIBDIR/libssl.3.dylib"    || true
# （必要なら）バンドル全体にも試すが失敗は無視
codesign --force --deep --sign - --timestamp=none "$APP" || true

# ===== 4) 確認（Cellar パスが消えていること／@loader_path 解決になっていること） =====
echo "== otool -L libssl.3.dylib =="
otool -L "$LIBDIR/libssl.3.dylib" | sed 's/^/  /'
echo "== otool -L libcrypto.3.dylib =="
otool -L "$LIBDIR/libcrypto.3.dylib" | sed 's/^/  /'

# ===== 5) 配布用 ZIP =====
rm -f lem.zip
( cd bin && zip -r ../lem.zip "lem.app" )
echo "Packaged: $(pwd)/lem.zip"
