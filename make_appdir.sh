#!/bin/sh -ex
export APPIMAGE_EXTRACT_AND_RUN=1

mkdir -p AppDir/usr/bin AppDir/usr/lib AppDir/usr/libexec \
         AppDir/usr/share/applications AppDir/usr/share/icons/hicolor/256x256/apps

# SBCL dump 実体
cp bin/lem AppDir/usr/libexec/lem.real
chmod +x AppDir/usr/libexec/lem.real

# 起動ラッパー（シェル）
cat > AppDir/usr/bin/run-lem <<'EOF'
#!/usr/bin/env bash
APPDIR="${APPDIR:-$(dirname "$(readlink -f "$0")")/..}"
export LD_LIBRARY_PATH="$APPDIR/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
exec "$APPDIR/usr/libexec/lem.real" "$@"
EOF
chmod +x AppDir/usr/bin/run-lem

# 共有ライブラリは usr/lib に
cp bin/*.so* AppDir/usr/lib/  || true

# .desktop / icon（Exec を run-lem に）
sed -E 's/^Exec=.*/Exec=run-lem %F/' resources/lem.desktop > AppDir/lem.desktop
cp resources/lem.png AppDir/lem.png

# ★ --executable は渡さない！
/usr/local/bin/linuxdeploy-x86_64.AppImage \
  --appdir AppDir \
  --desktop-file AppDir/lem.desktop \
  --icon-file   AppDir/lem.png

# 念のため：AppRun が symlink になっているか確認。なければ自前で置く
if [ ! -e AppDir/AppRun ]; then
  ln -sf usr/bin/run-lem AppDir/AppRun
fi

/usr/local/bin/appimagetool-x86_64.AppImage AppDir
