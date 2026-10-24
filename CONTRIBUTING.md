# Contributing to Yeditor

Read `AGENTS.md`, `STYLEGUIDE.md` and `contract.yml` before changing code.
Use failing acceptance tests first and retain license headers and layer boundaries.

## AI providers (#16)

CI uses `yeditor/components/test-data/fake-ai-server.py`, a local HTTP fixture,
for the Ollama, OpenAI-compatible and Claude protocol tests in
`yeditor-components-tests`. No real Ollama server or cloud credentials are needed.
Run the suite using `sbcl --script scripts/run-yeditor-all-tests.lisp` after
installing dependencies with Qlot.

The network-free privacy/palette checks can run separately with:

```sh
sbcl --script scripts/run-yeditor-model-policy-tests.lisp
```

For an optional real Ollama check, run Ollama locally at
`http://127.0.0.1:11434`, install `le-models` and `le-ollama`, and use the
appearance palette's AI actions to download a model, chat, or explain a selection.
Model downloads require the Ollama server to reach its model registry and may
consume substantial disk space. This manual check is separate from CI fixtures.

Local providers are the default. Remote endpoints require explicit cloud opt-in;
the AI mode-line shows cloud status. This policy covers model discovery and
Ollama downloads as well as generation. Never commit API keys or use real cloud
credentials in tests. Removing `le-models` restores its palette entries and bindings.
