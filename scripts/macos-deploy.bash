#!/usr/bin/env bash -ex
# scripts/macos-deploy.bash

set -o pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
parent_dir="$(dirname "$script_dir")"
cd "$parent_dir"

# ===== 1) ビルド =====
qlot install

# Build the lem-terminal native helper with libvterm statically linked so the
# bundled terminal.so has no external libvterm dependency. It must exist before
# `asdf:make :lem` so that lem-terminal/ffi.lisp loads it at build time and the
# deploy library bundles it into Lem.app/Contents/MacOS and reloads it at boot.
LEM_TERMINAL_STATIC=1 make terminal-lib

qlot exec sbcl --eval '(ql:quickload :lem)' --eval '(asdf:make :lem)'

# Confirm the deploy library actually bundled terminal.so into the app; a
# missing helper means the terminal extension would silently disable itself.
if ! find bin/lem.app -name 'terminal.so' -print -quit | grep -q .; then
  echo "error: terminal.so was not bundled into the app by deploy." >&2
  echo "       Ensure libvterm is installed so it loads at build time." >&2
  exit 1
fi

# Rename the deploy output (lem.app) to use the user-facing name (Lem.app).
# The asdf system is named "lem" so the deploy library produces lem.app; we
# rename the bundle, executable, and Info.plist fields here so Finder, the
# Dock, and the menu bar show "Lem".
#
# APFS (and HFS+) are case-insensitive but case-preserving by default, so
# `mv lem.app Lem.app` is a no-op (same path) and `rm -rf bin/Lem.app`
# would delete the bundle we just built. Force a case change by going
# through a uniquely-named intermediate.
mv bin/lem.app bin/__lem_renaming__.app
mv bin/__lem_renaming__.app bin/Lem.app
mv bin/Lem.app/Contents/MacOS/lem bin/Lem.app/Contents/MacOS/__lem_renaming__
mv bin/Lem.app/Contents/MacOS/__lem_renaming__ bin/Lem.app/Contents/MacOS/Lem
PLIST="bin/Lem.app/Contents/Info.plist"
/usr/libexec/PlistBuddy -c "Set :CFBundleName Lem"        "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleDisplayName Lem" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleExecutable Lem"  "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleIconFile Lem"    "$PLIST"

# アイコン（.icns for macOS dock/taskbar icon, .png as fallback）
# Generate the .icns first so a missing icon is a hard failure rather than
# silently shipping an iconless .app. The bundle is now Lem.app, and
# CFBundleIconFile is "Lem", so the icon must be named Lem.icns.
make icns
cp resources/lem.icns bin/Lem.app/Contents/Resources/Lem.icns
cp resources/lem.png  bin/Lem.app/Contents/Resources/Lem.png || true

# ===== 2) OpenSSL を同梱し、参照先を @loader_path 化 =====
APP="bin/Lem.app"
EXE="$APP/Contents/MacOS/Lem"
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
echo "The following command must be executed for Lem.app to start.
```
xattr -dr com.apple.quarantine Lem.app/
```
" > "$README_PATH"

rm -f lem-macos.zip
(
  cd bin
  zip -r ../lem-macos.zip "Lem.app" "README.md"
)
echo "Packaged: $(pwd)/lem-macos.zip"
