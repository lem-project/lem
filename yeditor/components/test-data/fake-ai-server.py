#!/usr/bin/env python3
# Copyright (c) 2026 Yuri Flores Albuquerque.
# SPDX-License-Identifier: FSL-1.1-MIT
# Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
# Competing commercial use is strictly prohibited.
#
# Fake AI server for le-models tests. Speaks enough of three protocols:
#   Ollama            /api/version /api/tags /api/generate (NDJSON) /api/pull (NDJSON)
#   OpenAI-compatible /v1/models /v1/chat/completions (SSE)
#   Anthropic         /v1/messages (SSE; validates headers; "OVERLOAD-ONCE" -> one 529)
#   /stats            request counts per path (to check that nothing was sent)
# Prints "READY <port>" once listening on 127.0.0.1 (port 0 = any free port).
import json, sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

STATS = {}
OVERLOADED = set()


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *args):
        pass

    def count(self):
        STATS[self.path] = STATS.get(self.path, 0) + 1

    def body(self):
        length = int(self.headers.get("Content-Length") or 0)
        return json.loads(self.rfile.read(length) or b"{}")

    def send_json(self, code, obj, headers=()):
        data = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        for k, v in headers:
            self.send_header(k, v)
        self.end_headers()
        self.wfile.write(data)

    def stream(self, content_type, lines):
        data = "".join(lines).encode()
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        self.count()
        if self.path == "/api/version":
            self.send_json(200, {"version": "0.0-fake"})
        elif self.path == "/api/tags":
            self.send_json(200, {"models": [{"name": "llama3.2"}, {"name": "qwen2.5-coder"}]})
        elif self.path == "/v1/models":
            self.send_json(200, {"data": [{"id": "local-model"}]})
        elif self.path == "/stats":
            self.send_json(200, STATS)
        else:
            self.send_json(404, {"error": "not found"})

    def do_POST(self):
        self.count()
        req = self.body()
        if self.path == "/api/generate":
            chunks = ["Hello", " from", " " + req.get("model", "?")]
            lines = [json.dumps({"response": c, "done": False}) + "\n" for c in chunks]
            lines.append(json.dumps({"response": "", "done": True}) + "\n")
            self.stream("application/x-ndjson", lines)
        elif self.path == "/api/pull":
            lines = [json.dumps({"status": s}) + "\n" for s in ("pulling manifest", "downloading", "success")]
            self.stream("application/x-ndjson", lines)
        elif self.path == "/v1/chat/completions":
            lines = ["data: " + json.dumps({"choices": [{"delta": {"content": c}}]}) + "\n\n"
                     for c in ("compat", " answer")]
            lines.append("data: [DONE]\n\n")
            self.stream("text/event-stream", lines)
        elif self.path == "/v1/messages":
            if not self.headers.get("x-api-key"):
                return self.send_json(401, {"type": "error", "error": {"type": "authentication_error",
                                                                      "message": "missing x-api-key"}})
            if self.headers.get("anthropic-version") != "2023-06-01":
                return self.send_json(400, {"type": "error", "error": {"type": "invalid_request_error",
                                                                      "message": "bad anthropic-version"}})
            prompt = req["messages"][-1]["content"]
            if "OVERLOAD-ONCE" in prompt and prompt not in OVERLOADED:
                OVERLOADED.add(prompt)
                return self.send_json(529, {"type": "error", "error": {"type": "overloaded_error",
                                                                      "message": "overloaded"}},
                                      headers=[("retry-after", "0")])
            stop = "refusal" if "REFUSE" in prompt else "end_turn"

            def event(name, obj):
                return "event: " + name + "\ndata: " + json.dumps(obj) + "\n\n"
            lines = [event("message_start", {"type": "message_start", "message": {"id": "msg_fake"}}),
                     event("content_block_start", {"type": "content_block_start", "index": 0,
                                                   "content_block": {"type": "thinking", "thinking": ""}}),
                     event("content_block_delta", {"type": "content_block_delta", "index": 0,
                                                   "delta": {"type": "thinking_delta", "thinking": ""}}),
                     event("content_block_start", {"type": "content_block_start", "index": 1,
                                                   "content_block": {"type": "text", "text": ""}})]
            for c in ("Claude", " says", " hi"):
                lines.append(event("content_block_delta", {"type": "content_block_delta", "index": 1,
                                                           "delta": {"type": "text_delta", "text": c}}))
            lines += [event("message_delta", {"type": "message_delta", "delta": {"stop_reason": stop}}),
                      event("message_stop", {"type": "message_stop"})]
            self.stream("text/event-stream", lines)
        else:
            self.send_json(404, {"error": "not found"})


server = ThreadingHTTPServer(("127.0.0.1", int(sys.argv[1]) if len(sys.argv) > 1 else 0), Handler)
print("READY", server.server_address[1], flush=True)
server.serve_forever()
