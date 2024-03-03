#!/bin/sh -x

USER_NAME=${USER_NAME:-user}
adduser -D "$USER_NAME"

chmod 755 /work/lem-jsonrpc/lem-jsonrpc
su - $USER_NAME -c '/work/lem-jsonrpc/lem-jsonrpc --mode websocket --port 50000 --host 0.0.0.0'
