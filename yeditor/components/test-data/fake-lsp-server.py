#!/usr/bin/env python3
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
#
# Minimal stdio language server for tests: answers initialize and shutdown,
# exits on exit or end of input.
import json, sys

def read_message():
    length = None
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        if line.lower().startswith(b"content-length:"):
            length = int(line.split(b":")[1])
    return json.loads(sys.stdin.buffer.read(length)) if length else {}

def send(obj):
    body = json.dumps(obj).encode()
    sys.stdout.buffer.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    sys.stdout.buffer.flush()

while True:
    msg = read_message()
    if msg is None or msg.get("method") == "exit":
        sys.exit(0)
    if msg.get("method") == "initialize":
        if len(sys.argv) > 1:
            with open(sys.argv[1], "a") as trace:
                trace.write(json.dumps(msg["params"]) + "\n")
        send({"jsonrpc": "2.0", "id": msg["id"], "result": {"capabilities": {}}})
    elif msg.get("method") == "shutdown":
        send({"jsonrpc": "2.0", "id": msg["id"], "result": None})
