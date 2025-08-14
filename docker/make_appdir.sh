#!/bin/sh -ex
export APPIMAGE_EXTRACT_AND_RUN=1

mkdir -p AppDir/usr/bin AppDir/usr/lib AppDir/usr/libexec \
         AppDir/usr/share/applications AppDir/usr/share/icons/hicolor/256x256/apps

# SBCL dump 実体
cp bin/lem AppDir/usr/libexec/lem.real
chmod +x AppDir/usr/libexec/lem.real

# 起動ラッパー（シェル）: 実体を起動するだけ（環境変数の設定は AppRun で行う）
cat > AppDir/usr/bin/run-lem <<'EOF'
#!/usr/bin/env bash
APPDIR="${APPDIR:-$(dirname "$(readlink -f "$0")")/..}"
exec "$APPDIR/usr/libexec/lem.real" "$@"
EOF
chmod +x AppDir/usr/bin/run-lem

# 共有ライブラリは usr/lib に
cp bin/*.so* AppDir/usr/lib/  || true

# .desktop / icon（Exec を run-lem に）
sed -E 's/^Exec=.*/Exec=run-lem %F/' resources/lem.desktop > AppDir/lem.desktop
cp resources/lem.png AppDir/lem.png

# ★ --executable は渡さない！（SBCL 実体を触らせない）
/usr/local/bin/linuxdeploy-x86_64.AppImage \
  --appdir AppDir \
  --desktop-file AppDir/lem.desktop \
  --icon-file   AppDir/lem.png

# linuxdeploy が作った AppRun（symlink）を自前スクリプトで置き換える
rm -f AppDir/AppRun
cat > AppDir/AppRun <<'EOF'
#!/usr/bin/env bash
set -e
APPDIR="${APPDIR:-$(dirname "$(readlink -f "$0")")}"

# AppImage 同梱の .so を優先
export LD_LIBRARY_PATH="$APPDIR/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

# WebKitGTK の helper プロセス (WebKitNetworkProcess, WebKitWebProcess) の場所を検出
for d in \
  /usr/lib/x86_64-linux-gnu/webkit2gtk-4.1 \
  /usr/lib64/webkit2gtk-4.1 \
  /usr/lib/webkit2gtk-4.1 \
  /usr/lib64/webkit2gtk-41 \
  /usr/lib/webkit2gtk-41
do
  if [ -x "$d/WebKitNetworkProcess" ] && [ -x "$d/WebKitWebProcess" ]; then
    export WEBKIT_EXEC_PATH="$d"
    break
  fi
done

# Fedora/openSUSE などで必要になることがある Gio モジュール
mods=""
[ -d /usr/lib64/gio/modules ] && mods="/usr/lib64/gio/modules"
[ -d /usr/lib/x86_64-linux-gnu/gio/modules ] && mods="${mods:+$mods:}/usr/lib/x86_64-linux-gnu/gio/modules"
[ -n "$mods" ] && export GIO_EXTRA_MODULES="${mods}${GIO_EXTRA_MODULES:+:$GIO_EXTRA_MODULES}"

exec "$APPDIR/usr/bin/run-lem" "$@"
EOF
chmod +x AppDir/AppRun

# パッケージ化
/usr/local/bin/appimagetool-x86_64.AppImage AppDir
