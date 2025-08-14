#!/usr/bin/env bash -ex
# scripts/macos-deploy.bash

set -o pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

# ===== 1) ビルド =====
qlot install
qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'

# アイコン（存在しなくても続行）
cp resources/lem.png bin/lem.app/Contents/Resources/ || true

# ===== 2) OpenSSL を同梱し、参照先を @loader_path 化 =====
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
OPENSSL_LIB="$OPENSSL_PREFIX/lib"

# Cellar の実体（バージョン付きディレクトリ）を解決
LIBCRYPTO_REAL="$(realpath "$OPENSSL_LIB/libcrypto.3.dylib")"
LIBSSL_REAL="$(realpath "$OPENSSL_LIB/libssl.3.dylib")"

# deploy が同名を置いている場合もあるので上書き
cp -f "$OPENSSL_LIB/libssl.3.dylib"    "$LIBDIR/libssl.3.dylib"
cp -f "$OPENSSL_LIB/libcrypto.3.dylib" "$LIBDIR/libcrypto.3.dylib"

# dylib 自身の ID を @loader_path に揃える
install_name_tool -id @loader_path/libssl.3.dylib    "$LIBDIR/libssl.3.dylib"
install_name_tool -id @loader_path/libcrypto.3.dylib "$LIBDIR/libcrypto.3.dylib"

# libssl → libcrypto の参照を絶対パスから置換
# 1) Cellar 実体パス
install_name_tool -change "$LIBCRYPTO_REAL" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 2) opt 経由のパス
install_name_tool -change "$OPENSSL_LIB/libcrypto.3.dylib" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 3) @rpath の場合も @loader_path に寄せる（冪等）
install_name_tool -change @rpath/libcrypto.3.dylib @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true

# 互換用シンボリックリンク（無印名で探すケースに備える）
( cd "$LIBDIR" && { ln -sf libssl.3.dylib libssl.dylib; ln -sf libcrypto.3.dylib libcrypto.dylib; } )

# 実行ファイル(lem)は SBCL のダンプ構造のため install_name_tool は当てない

# ===== 3) 署名（必須ではないが念のため。失敗しても続行） =====
codesign --force --sign - --timestamp=none "$LIBDIR/libcrypto.3.dylib" || true
codesign --force --sign - --timestamp=none "$LIBDIR/libssl.3.dylib"    || true
codesign --force --deep --sign - --timestamp=none "$APP" || true

# ===== 4) 検査（絶対パスが残っていないか） =====
echo "== otool -L libssl.3.dylib =="
otool -L "$LIBDIR/libssl.3.dylib" | sed 's/^/  /'
echo "== otool -L libcrypto.3.dylib =="
otool -L "$LIBDIR/libcrypto.3.dylib" | sed 's/^/  /'

if otool -L "$LIBDIR/libssl.3.dylib"    | grep -q '/opt/homebrew' \
|| otool -L "$LIBDIR/libcrypto.3.dylib" | grep -q '/opt/homebrew'; then
  echo "error: absolute Homebrew paths remain in libssl/libcrypto. Fixing failed." >&2
  exit 1
fi

# ===== 5) 配布用 ZIP =====
README_PATH="bin/README.md"
echo "The following command must be executed for lem.app to start.
```
xattr -dr com.apple.quarantine lem.app/
```
" > "$README_PATH"

rm -f lem-macos.zip
(
  cd bin
  zip -r ../lem-macos.zip "lem.app" "README.md"
)
echo "Packaged: $(pwd)/lem-macos.zip"
