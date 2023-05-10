#!/bin/bash

export ROOT="`dirname "$(readlink -f $0)"`"

APP_DIR=$HOME/.local/share/applications
if [ ! -e $APP_DIR ]; then
    mkdir $APP_DIR
fi

cp lem.desktop $APP_DIR

cd $APP_DIR
sed -i "s,{ICON_PLACEHOLDER},${ROOT}/resources/icon.png,g" lem.desktop
sed -i "s,{EXEC_PLACEHOLDER},${ROOT}/lem,g" lem.desktop
