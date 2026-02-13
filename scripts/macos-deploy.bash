#!/usr/bin/env bash -eux
# scripts/macos-deploy.bash

set -o pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

# ===== 0) クリーンアップ =====
# ===== 0) Cleanup =====
APP="bin/lem.app"
rm -rf "$APP"

# ===== 1) ビルド =====
# ===== 1) Build =====
qlot install
qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'

# アイコン（存在しなくても続行）
# Icon (Continue even if it does not exist)
cp resources/lem.png bin/lem.app/Contents/Resources/ || true

# ===== 2) OpenSSL を同梱し、参照先を @loader_path 化 =====
# ===== 2) Bundle OpenSSL and convert reference targets to @loader_path =====
EXE="$APP/Contents/MacOS/lem"
LIBDIR="$APP/Contents/MacOS"

if [[ ! -x "$EXE" ]]; then
  echo "error: executable not found: $EXE" >&2
  exit 1
fi

if ! command -v pkg-config >/dev/null 2>&1; then
  echo "error: pkg-config not found. Install pkg-config before packaging." >&2
  exit 1
fi

OPENSSL_LIB="$(pkg-config --variable=libdir openssl)"

# Cellar の実体（バージョン付きディレクトリ）を解決
# Resolve the actual path in the Cellar (versioned directory)
LIBCRYPTO_REAL="$(realpath "$OPENSSL_LIB/libcrypto.3.dylib")"
LIBSSL_REAL="$(realpath "$OPENSSL_LIB/libssl.3.dylib")"

# deploy が同名を置いている場合もあるので上書き
# Overwrite because deploy may have placed a file with the same name
cp -f "$OPENSSL_LIB/libssl.3.dylib"    "$LIBDIR/libssl.3.dylib"
cp -f "$OPENSSL_LIB/libcrypto.3.dylib" "$LIBDIR/libcrypto.3.dylib"

# dylib 自身の ID を @loader_path に揃える
# Align the dylib's own ID to @loader_path
install_name_tool -id @loader_path/libssl.3.dylib    "$LIBDIR/libssl.3.dylib"
install_name_tool -id @loader_path/libcrypto.3.dylib "$LIBDIR/libcrypto.3.dylib"

# libssl → libcrypto の参照を絶対パスから置換
# Replace libssl -> libcrypto references from absolute paths
# 1) Cellar 実体パス
# 1) Cellar resolved path
install_name_tool -change "$LIBCRYPTO_REAL" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 2) opt 経由のパス
# 2) Path via opt
install_name_tool -change "$OPENSSL_LIB/libcrypto.3.dylib" @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true
# 3) @rpath の場合も @loader_path に寄せる（冪等）
# 3) Also shift @rpath cases to @loader_path (idempotent)
install_name_tool -change @rpath/libcrypto.3.dylib @loader_path/libcrypto.3.dylib "$LIBDIR/libssl.3.dylib" || true

# 互換用シンボリックリンク（無印名で探すケースに備える）
# Symbolic links for compatibility (In case it looks for the name without version suffix)
( cd "$LIBDIR" && { ln -sf libssl.3.dylib libssl.dylib; ln -sf libcrypto.3.dylib libcrypto.dylib; } )

# 必要に応じて zlib を同梱
# Bundle zlib if needed
if otool -L "$LIBDIR/libssl.3.dylib" "$LIBDIR/libcrypto.3.dylib" | grep -q "libz"; then
  ZLIB_DIR="$(pkg-config --variable=libdir zlib)"
  install -m 0755 "$ZLIB_DIR/libz.1.dylib" "$LIBDIR/libz.1.dylib"

  # libz 自身の ID を @loader_path に変更
  # Change libz's own ID to @loader_path
  install_name_tool -id @loader_path/libz.1.dylib "$LIBDIR/libz.1.dylib"

  # libssl/libcrypto からの参照を置換
  # Replace references from libssl/libcrypto
  install_name_tool -change "$ZLIB_DIR/libz.1.dylib" @loader_path/libz.1.dylib "$LIBDIR/libssl.3.dylib" || true
  install_name_tool -change "$ZLIB_DIR/libz.1.dylib" @loader_path/libz.1.dylib "$LIBDIR/libcrypto.3.dylib" || true
fi

# 実行ファイル(lem)は SBCL のダンプ構造のため install_name_tool は当てない
# Do not apply install_name_tool to the executable (lem) because of the SBCL dump structure

# ===== 3) 署名（必須ではないが念のため。失敗しても続行） =====
# ===== 3) Signing (Not required, but just in case. Continue even if it fails) =====
codesign --force --sign - --timestamp=none "$LIBDIR/libcrypto.3.dylib" || true
codesign --force --sign - --timestamp=none "$LIBDIR/libssl.3.dylib"    || true
codesign --force --deep --sign - --timestamp=none "$APP" || true

# ===== 4) 検査（絶対パスが残っていないか） =====
# ===== 4) Inspection (Check if any absolute paths remain) =====
echo "== otool -L libssl.3.dylib =="
otool -L "$LIBDIR/libssl.3.dylib" | sed 's/^/  /'
echo "== otool -L libcrypto.3.dylib =="
otool -L "$LIBDIR/libcrypto.3.dylib" | sed 's/^/  /'

if otool -L "$LIBDIR/libssl.3.dylib"    | grep -q '/opt' \
|| otool -L "$LIBDIR/libcrypto.3.dylib" | grep -q '/opt'; then
  echo "error: absolute paths remain in libssl/libcrypto. Fixing failed." >&2
  exit 1
fi

# ===== 5) 配布用 ZIP =====
# ===== 5) ZIP for distribution =====
README_PATH="bin/README.md"
echo 'The following command must be executed for lem.app to start.
```
xattr -dr com.apple.quarantine lem.app/
```
' > "$README_PATH"

rm -f lem-macos.zip
(
  cd bin
  zip -r ../lem-macos.zip "lem.app" "README.md"
)
echo "Packaged: $(pwd)/lem-macos.zip"
