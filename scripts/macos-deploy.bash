#!/usr/bin/env bash
# scripts/macos-deploy.bash

set -euxo pipefail

BUILD_DIR=build
BIN_DIR=bin
APP="$BIN_DIR/lem.app"
EXE="$APP/Contents/MacOS/lem"
LIBDIR="$APP/Contents/MacOS"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

# ===== 0) クリーンアップ =====
# ===== 0) Cleanup =====
rm -rf "$BUILD_DIR"
rm -rf "$BIN_DIR"
mkdir -p "$BUILD_DIR"
mkdir -p "$BIN_DIR"

# ===== 1) ビルド用SBCLの準備 =====
# ===== 1) Prepare SBCL for building =====
# SBCLはlibzstdに依存しており、実行ファイル(lem)はその共有依存関係を継承します。
# SBCLのダンプ構造のため、実行ファイルに対してinstall_name_toolは機能しません。
# そのため、代わりにSBCL実行ファイルのコピーに対してdylib参照を置換する必要があります。
# SBCL depends on libzstd and the app executable (lem) inherits those shared deps
# `install_name_tool` doesn't work on the app executable because of the SBCL dump structure so
# we have to replace dylib references on a copy of the sbcl executable instead
SBCL_BIN=$(realpath "$(which sbcl)")
SBCL_HOME="$(dirname "$SBCL_BIN")/../lib/sbcl"
# Homebrew uses a shell script so we have to find the real executable
if [[ $(file "$SBCL_BIN") == *"shell script"* ]]; then
    SBCL_ROOT="$(dirname "$(dirname "$SBCL_BIN")")"
    SBCL_BIN="$SBCL_ROOT/libexec/bin/sbcl"
    SBCL_HOME="$SBCL_ROOT/lib/sbcl"
fi
install -m 0755 "$SBCL_BIN" "$BUILD_DIR/sbcl"
export SBCL_HOME

LIBZSTD_DIR="$(pkg-config --variable=libdir libzstd)"
install -m 0755 "$LIBZSTD_DIR/libzstd.1.dylib" "$BUILD_DIR/libzstd.1.dylib"
install_name_tool -id @loader_path/libzstd.1.dylib "$BUILD_DIR/libzstd.1.dylib"
install_name_tool -change "$LIBZSTD_DIR/libzstd.1.dylib" @loader_path/libzstd.1.dylib "$BUILD_DIR/sbcl"

codesign --force --sign - "$BUILD_DIR/libzstd.1.dylib"
codesign --force --sign - --preserve-metadata=entitlements,requirements "$BUILD_DIR/sbcl"

# ===== 2) ビルド =====
# ===== 2) Build =====
qlot install
PATH="$PWD/build:$PATH" qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'

# アイコン（存在しなくても続行）
# Icon (Continue even if it does not exist)
install -m 0644 resources/lem.png "$APP/Contents/Resources/" || true

# ===== 3) OpenSSL を同梱し、参照先を @loader_path 化 =====
# ===== 3) Bundle OpenSSL and convert reference targets to @loader_path =====
if [[ ! -x "$EXE" ]]; then
  echo "error: executable not found: $EXE" >&2
  exit 1
fi

if ! command -v pkg-config >/dev/null 2>&1; then
  echo "error: pkg-config not found. Install pkg-config before packaging." >&2
  exit 1
fi

# deploy によって追加されたライブラリを削除
# Remove libs added by deploy
rm "$LIBDIR"/libtree-sitter.*.dylib || true
rm "$LIBDIR"/libzstd.*.dylib || true

OPENSSL_LIB="$(pkg-config --variable=libdir openssl)"

# Cellar の実体（バージョン付きディレクトリ）を解決
# Resolve the actual path in the Cellar (versioned directory)
LIBCRYPTO_REAL="$(realpath "$OPENSSL_LIB/libcrypto.3.dylib")"

# deploy が同名を置いている場合もあるので上書き
# Overwrite because deploy may have placed a file with the same name
install -m 0755 "$OPENSSL_LIB/libssl.3.dylib"    "$LIBDIR/libssl.3.dylib"
install -m 0755 "$OPENSSL_LIB/libcrypto.3.dylib" "$LIBDIR/libcrypto.3.dylib"

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

# 必要に応じて zlib を同梱 (MacPorts の OpenSSL は zlib に依存するため)
# Bundle zlib if needed (MacPorts OpenSSL depends on zlib)
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

# tree-sitter を同梱
# Bundle tree-sitter
LIBTREE_SITTER_DIR="$(pkg-config --variable=libdir tree-sitter)"
install -m 0755 "$LIBTREE_SITTER_DIR/libtree-sitter.0.dylib" "$LIBDIR/libtree-sitter.0.dylib"

# vterm を同梱
# Bundle vterm
VTERM_DIR="$(pkg-config --variable=libdir vterm)"
install -m 0755 "$VTERM_DIR/libvterm.0.dylib" "$LIBDIR/libvterm.0.dylib"

# zstd を同梱
# Bundle zstd
install -m 0755 "$BUILD_DIR/libzstd.1.dylib" "$LIBDIR/libzstd.1.dylib"

# dylib の自認パス (ID) を @loader_path に変更
# Change dylib IDs to @loader_path
install_name_tool -id @loader_path/libtree-sitter.0.dylib "$LIBDIR/libtree-sitter.0.dylib"
install_name_tool -id @loader_path/libts-wrapper.dylib "$LIBDIR/libts-wrapper.dylib"
install_name_tool -id @loader_path/libvterm.0.dylib "$LIBDIR/libvterm.0.dylib"
install_name_tool -id @loader_path/libwebview.0.12.dylib "$LIBDIR/libwebview.dylib"
install_name_tool -id @loader_path/terminal.so "$LIBDIR/terminal.so"

# dylib の参照先を @loader_path に変更
# Change dylib references to @loader_path
install_name_tool -change /opt/homebrew/opt/tree-sitter/lib/libtree-sitter.0.26.dylib @loader_path/libtree-sitter.0.dylib "$LIBDIR/libts-wrapper.dylib"
install_name_tool -change /opt/homebrew/opt/libvterm/lib/libvterm.0.dylib @loader_path/libvterm.0.dylib "$LIBDIR/terminal.so"

# 実行ファイル(lem)は SBCL のダンプ構造のため install_name_tool は当てない
# Do not apply install_name_tool to the executable (lem) because of the SBCL dump structure

# ===== 4) 署名（必須ではないが念のため。失敗しても続行） =====
# ===== 4) Signing (Not required, but just in case. Continue even if it fails) =====
codesign --force --sign - "$LIBDIR/libcrypto.3.dylib" || true
codesign --force --sign - "$LIBDIR/libssl.3.dylib"    || true
codesign --force --deep --sign - "$APP" || true

# ===== 5) 検査（絶対パスが残っていないか） =====
# ===== 5) Inspection (Check if any absolute paths remain) =====
echo "== otool -L * =="
otool -L "$LIBDIR"/* | sed 's/^/  /'

if otool -L "$LIBDIR"/* | grep -q '/opt'; then
  echo "error: absolute paths remain in lem executable or bundled dylibs. Fixing failed." >&2
  exit 1
fi

# ===== 6) 配布用 ZIP =====
# ===== 6) ZIP for distribution =====
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
