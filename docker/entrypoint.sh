#!/bin/sh -x

USER_NAME=${USER_NAME:-user}
adduser -D "$USER_NAME"

chmod 755 /work/lem-rpc/lem-rpc
su - $USER_NAME -c '/work/lem-rpc/lem-rpc --mode websocket --port 50000 --host 0.0.0.0'
