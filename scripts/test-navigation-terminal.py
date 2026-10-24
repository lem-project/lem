#!/usr/bin/env python3
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
"""Exercise real ncurses input and MCP together. Requires installed lem-ncurses.
Run from the repository root after loading the environment's dependencies.
"""
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import socket
import struct
import subprocess
import tempfile
import termios
import time
import urllib.request


def exercise(columns, rows):
    with tempfile.TemporaryDirectory(prefix="yeditor-navigation-terminal-") as directory:
        root = Path(directory)
        project = root / "project"
        (project / "src").mkdir(parents=True)
        (project / "src/alpha.lisp").write_text("; alpha\n", encoding="utf-8")
        (project / "src/beta.lisp").write_text("; beta\n", encoding="utf-8")
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
        environment = dict(os.environ, TERM="xterm-256color", YEDITOR_MCP_PORT=str(port),
                           LEM_HOME=str(root / "lem") + "/", XDG_DATA_HOME=str(root / "data"))
        with (root / "stderr.log").open("w+") as errors:
            process = subprocess.Popen(["sbcl", "--noinform", "--load", "scripts/run-yeditor-ncurses.lisp"],
                                       stdin=slave, stdout=slave, stderr=errors, env=environment,
                                       start_new_session=True)
            os.close(slave)
            transcript = bytearray()
            opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
            headers = {"Content-Type": "application/json", "Accept": "application/json, text/event-stream"}
            sequence = 0

            def drain(timeout=0.05):
                if select.select([master], [], [], timeout)[0]:
                    transcript.extend(os.read(master, 65536))

            def rpc(method, params):
                nonlocal sequence
                sequence += 1
                request = urllib.request.Request(f"http://127.0.0.1:{port}/mcp", headers=headers,
                    data=json.dumps(dict(jsonrpc="2.0", id=sequence, method=method, params=params)).encode())
                with opener.open(request, timeout=5) as response:
                    if response.headers.get("Mcp-Session-Id"):
                        headers["Mcp-Session-Id"] = response.headers["Mcp-Session-Id"]
                    result = json.load(response)
                assert "error" not in result, result
                return result["result"]

            def tool(name, arguments=None):
                result = rpc("tools/call", dict(name=name, arguments=arguments or {}))
                assert not result.get("isError"), result
                return json.loads(result["content"][0]["text"])

            def action(name, **arguments):
                return tool("yeditor_navigation_action", dict(action=name, **arguments))

            def status():
                return tool("yeditor_navigation_status")

            def until(predicate, description, seconds=8):
                deadline = time.monotonic() + seconds
                while time.monotonic() < deadline:
                    drain()
                    if predicate():
                        return
                raise AssertionError(description)

            def click(x, y):
                os.write(master, f"\x1b[<0;{x};{y}M\x1b[<0;{x};{y}m".encode())

            try:
                until(lambda: b"[Abrir pasta]" in transcript, "visible startup controls", seconds=90)
                rpc("initialize", dict(protocolVersion="2025-06-18", capabilities={},
                    clientInfo=dict(name="navigation-terminal-test", version="1")))
                click(4, 1)
                until(lambda: bool(status()["browser"]), "real SGR header click opens folder selector")
                os.write(master, b"\x1b")
                until(lambda: not status()["browser"], "Escape cancels without changing project")
                assert status()["project"] == str(Path.cwd()) + "/"
                action("home")
                # The first button is reached without a chord or a command name.
                os.write(master, b"\t\r")
                until(lambda: bool(status()["browser"]), "Tab/Enter activates folder selector")
                action("cancel")
                action("project", path=str(project) + "/")
                until(lambda: status()["index"] == "ready", "asynchronous filename index")
                action("search", query="alpha")
                action("preview", path="src/alpha.lisp")
                assert status()["preview"] == "alpha.lisp"
                click(29, 3)
                until(lambda: not status()["preview"], "native header Fixar pins the file")
                action("preview", path="src/beta.lisp")
                paths = {b["filename"] for b in tool("buffer_list")}
                assert {str(project / "src/alpha.lisp"), str(project / "src/beta.lisp")} <= paths
                action("close")
                assert status()["active_file"].endswith("alpha.lisp")
                os.write(master, b"; human edit")
                until(lambda: any(b["modified"] for b in tool("buffer_list")), "human edit recorded")
                action("close")
                assert status()["pending_close"].endswith("alpha.lisp")
                action("cancel")
                assert status()["active_file"].endswith("alpha.lisp")
                action("close")
                action("discard")
                assert (project / "src/alpha.lisp").read_text() == "; alpha\n"
                action("focus")
                action("tree")
                assert b"Read Only" not in transcript, "mouse input was interpreted as text"
                print(f"PASS real ncurses {columns}x{rows}: click, Tab/Enter, search, preview, pin, dirty close/cancel/discard, MCP JSON")
            except BaseException:
                errors.flush()
                errors.seek(0)
                print(errors.read()[-4000:])
                raise
            finally:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)
                os.close(master)


if __name__ == "__main__":
    for size in [(80, 24), (120, 32)]:
        exercise(*size)
