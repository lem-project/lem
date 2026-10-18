# Yeditor — Implementation Handoff

*Living document. Updated after every completed task so another agent can continue the work.
Last update: 2026-10-18.*

Authoritative plan (Portuguese, local only — excluded from git via `.git/info/exclude`):
`.hermes/plans/2026-10-17_192626-yeditor-spacetime-lem-fork.md` (tasks T1–T30, chapters 1–18).
If the plan file is unavailable, this document plus `docs/IP_STRATEGY.md` and `docs/arch.md`
describe the architecture and rules.

## 1. Ground rules (read first)

- **Licensing (open core).** `yeditor/core/`, `yeditor/effects/`, `yeditor/components/` →
  FSL-1.1-MIT; `yeditor/enterprise/` → proprietary; Lem-derived L0 (`src/`, `frontends/`,
  `extensions/`, `contrib/`) → MIT. Every file under `yeditor/` must start with its tier header
  (see `docs/IP_STRATEGY.md` §4); `scripts/check-license-headers.sh` enforces it. Never add
  GPL/AGPL dependencies.
- **Commits.** Author is Yuri only — **no `Co-Authored-By` trailers**. L0 (Lem) changes go in their
  own small commits (`chore(l0)` / `fix(l0)`) explaining why.
- **Lem API.** Use exported `lem:` symbols; CI rejects `lem::`. If something needed is not
  exported, export it in L0 in a separate commit.
- **Dependencies.** Yeditor systems depend on `lem/core` (+ `lem/extensions` when needed), **never on
  the full `lem` system** — it also loads the webview frontend, which then becomes the default UI.
- **Layering.** Core never depends on Enterprise. Agent/journal/forge code (L4–L5) goes in
  `yeditor/enterprise/`.
- **Effects are lazy.** `install-effect` registers a thunk that runs at `tx-commit`; capture
  "previous" state inside the thunk, not at registration time.
- **Keep this file updated** at the end of each task.

## 2. How to build, test and run

From the repository root (dependencies via qlot, already installed in `.qlot/`):

| What | Command |
|---|---|
| All Yeditor test suites (exit 1 on failure) | `sbcl --script scripts/run-yeditor-all-tests.lisp` |
| License/IP gate (+ dependency audit) | `bash scripts/check-license-headers.sh --deps` |
| Benchmarks (+ compare with a base JSON) | `sbcl --script scripts/run-yeditor-bench.lisp out.json [base.json]` |
| Benchmark gate in Docker, base vs head | `bash scripts/bench-gate.sh <base-ref> [out-dir]` |
| Run the editor in a terminal, ephemeral UI on | `sbcl --load scripts/run-yeditor-ncurses.lisp` |

Driving the UI headlessly for verification: start it inside `tmux` (with `remain-on-exit on`),
send keys with `tmux send-keys`, read the screen with `tmux capture-pane -p` (`-e` for colors).
`M-x` must be sent as one escape sequence: `tmux send-keys -l $'\ex'`. The abort key is `C-g`
(`Esc` is the Meta prefix in a terminal).

Test status at last update: 9 test systems, **14,656 assertions, 0 failures** on the host (tree-sitter
tests are skipped there: no libtree-sitter). Inside the CI image (`docker/Dockerfile.test`, which
has libtree-sitter and the Python grammar) the components suite runs everything, tree-sitter included (14,780 assertions at last update).
To run it there with the working tree mounted:
`docker build -f docker/Dockerfile.test -t yeditor-ts .` then
`docker run --rm -v "$PWD/yeditor:/app/yeditor" -v "$PWD/extensions:/app/extensions" -v "$PWD/scripts:/app/scripts" --entrypoint bash yeditor-ts -c "sbcl --script scripts/run-yeditor-all-tests.lisp"`.

## 3. Layout

```
yeditor/
├── core/                 FSL — yeditor-core.asd (umbrella), package.lisp, version.lisp
│   ├── tx/               L1 yeditor-tx: install-effect, tx-commit, tx-rollback, side-transactions
│   ├── context/          L2 yeditor-context: registry (define-component, install/uninstall/toggle),
│   │                     reactivity (coeffects), loader (.yeditor.lisp + HMR), durability (JSONL log)
│   └── bench/            T10 benchmark suite (yeditor-bench)
├── effects/              FSL — standard effects (14 kinds) + tests
├── components/           FSL — T11 theme pack + policy, T12 UI components (zero-chrome, palette, HUD) + tests
└── enterprise/           Proprietary — yeditor-enterprise.asd + placeholder (no code yet)
scripts/                  test runners, launchers, CI gates
```

## 4. Done

| Task | What exists | Where |
|---|---|---|
| T1 | Transaction log, `with-transaction`, commit/rollback, benchmark | `core/tx/tx.lisp` |
| T2 | Inverses, LIFO rollback, all-or-nothing commit, side-transactions (kind 2) | `core/tx/` |
| T3 | `define-component`, install/uninstall/toggle; toggle suspends/resumes the **same** handles; nested installs isolated | `core/context/registry.lisp` |
| T4 | Effects: buffer, mode, hook | `effects/e-buffer,e-mode,e-hook` |
| T5 | Effects: keymap, variable, command, overlay, timer | `effects/` |
| T6 | Effects: process, TCP server, LSP session, fdefinition | `effects/` |
| T7 | Coeffects (`define-coeffects`, `emit-context-event`, `reclassify-all`), work-cycle predicates `:agent-tx-active`, `:eval-overlay-active`, `:diagnostics-critical` | `core/context/reactivity.lisp` |
| T8 | `.yeditor.lisp` loader: `use-component` forms, config validation, reconciliation, mtime-polling HMR thread | `core/context/loader.lisp` |
| T9 | Durable JSONL effects log, lifecycle hook, replay, crash recovery (`:persist t` reinstall) | `core/context/durability.lisp` |
| T10 | Benchmarks (dispatch, keystroke→frame, component toggle, empty commit), budgets, 20% p95 regression gate in CI (base vs head on same runner) | `core/bench/`, `scripts/bench-gate.sh` |
| T12 (first cut) | Zero-chrome basal state, theme palette with live preview + abort rollback, agent HUD mounted by coeffect with inline diffs and C-c C-a / C-c C-k | `components/` |
| T11 | Curated pack of 11 `ergo-*` themes defined by semantic roles; OKLCH/WCAG policy validator (no pure black/white, text ≥ 7:1, syntax chroma ≤ 0.17, no light neon); `le-themes-ergonomic` applies Gruvbox Soft and narrows the palette to the pack; `le-theme-extended` reopens all Lem themes; `e-theme` effect | `components/themes-ergonomic.lisp`, `components/theme-policy.lisp`, `effects/e-theme.lisp` |
| T13 | `le-lsp`: language server sessions started as transactions (one per language), stopped on uninstall; suppresses Lem's untracked auto-start (`lem-lsp-mode:*disable*`) while installed; starts on find-file; modeline `LSP:<lang>` / `LSP:off`; missing server → message with the install command. `e-lsp` rewritten on a new public L0 API (`start-lsp-session`, `stop-lsp-session`, `lsp-session-alive-p`) | `components/lsp.lisp`, `effects/e-lsp.lisp`, `extensions/lsp-mode/lsp-mode.lisp` |
| T14 | Package onboarding: `scan-local-systems` (~/common-lisp, Quicklisp local-projects), `install-package` makes each package a runtime component `PACKAGE/<name>` (source-registry + load effects; inverse deletes created packages and clears created ASDF systems), confirmation notice about partial reversibility, install/uninstall logged by T9 durability; commands `yeditor-install-package` / `yeditor-uninstall-package`. Registry gained `register-component` / `unregister-component` | `components/packages.lisp`, `core/context/registry.lisp` |
| T15 | `le-inherited-entry-points-off` (basal state) removes Lem's load-time global entry points for Git (`C-x g`), projects (`C-x p …`) and the file tree (`C-x d` + filer buffer-sync hook); `le-git`, `le-project`, `le-tree` each re-add theirs as effects, so every feature toggles with no residue. New effects `install-key-removal`, `install-hook-removal` (restores original hook weight). All enabled by default via `enable-ephemeral-ui` | `components/features.lisp`, `effects/e-keymap.lisp`, `effects/e-hook.lisp` |
| T27 | `:treesitter-grammar` effect (kind 16): attaches a tree-sitter parser to a mode's syntax table, undo restores the previous parser and rescans buffers; grammar library loaded once and reused (no reload on toggle). `le-treesitter-packs` with a Python pack (own `highlights.scm` / `indents.scm`), grammars from `~/.local/share/yeditor/parsers/` or the system; `treesitter-install-grammar` (git clone + cc) and `compile-grammar`. L0 fixes: indentation counts only enclosing nodes and line-start outdents; `string.documentation` mapped | `effects/e-treesitter.lisp`, `components/treesitter.lisp`, `components/treesitter/python/`, `extensions/tree-sitter/indent.lisp` |
| T7 wiring | `le-context-bridge` publishes file type, major mode and project root on every file open / buffer switch (Lem hooks → `update-contexts`), so coeffect activation works in the running editor; adds pyproject/package.json/Cargo.toml/go.mod… to Lem's project-root markers. Reactivity gained `:any`, `:project-marker`, `:project-root`, batched `update-contexts`, and non-reentrant reclassification | `components/context-bridge.lisp`, `core/context/reactivity.lisp` |
| T28 | `le-lang-python`, activated by coeffect (.py file or pyproject/requirements/uv.lock): project virtualenv (.venv/venv/env, conda) put on `VIRTUAL_ENV`/`PATH` and followed across projects; best LSP server on PATH (basedpyright → pyright → pylsp); format on save with ruff/black as a reversible side-transaction (failure leaves the buffer untouched); REPL (`C-c C-z`, send region `C-c C-r` / buffer `C-c C-b`) with eval records; pytest under cursor / file (`C-c C-t`) with failing lines marked by overlays; everything undone on deactivation. New `e-environment` effect | `components/lang-python.lisp`, `effects/e-environment.lisp` |
| T16 | `le-models` provider layer (Core): `ollama-provider` (NDJSON streaming, model list, pull), `openai-compatible-provider` (SSE), `claude-provider` (Messages API over raw HTTP — no CL SDK; `x-api-key` from `ANTHROPIC_API_KEY`, `anthropic-version: 2023-06-01`, default `claude-opus-5-5`, server-side refusal fallback on, retries 429/529/5xx honouring `retry-after`, `stop_reason: refusal` reported). Cloud providers refused unless `*allow-cloud*` (privacy default: local). Components `le-models` (status segment `AI:<provider>` with cloud banner; `C-c a e/r/d/c` explain/rewrite/document/chat) and `le-ollama` (local Ollama as default provider, `yeditor-ai-pull-model`). Rewrite and documentation edits are reversible side-transactions (`yeditor-ai-undo-last`) | `components/models.lisp`, `components/ai.lisp` |
| Licensing | Open-core licenses, CLA, IP strategy, header/dependency CI gate | root `LICENSE*`, `docs/IP_STRATEGY.md` |

Extra effects added for T11/T12: `e-editor-variable` (Lem editor variables), `e-window` (side split, kind 15), `e-theme` (apply a color theme without saving it).

## 5. Left to do (in plan order)

- **T12 remainder**: `le-tabs` (off by default), per-segment modeline toggles, palette preview for buffers / LSP symbols / journal entries.
- **T27 remainder**: query packs for TypeScript/Rust/Go/CL (only Python exists), `textobjects.scm`/folding, tree-sitter indentation wired to python-mode's `calc-indent-function` on Enter (belongs with T28).
- **T28 remainder**: packs for TypeScript/Rust/Go/CL following `lang-python.lisp`; eval and test records move to `yeditor-journal` when T20 exists; poetry venv lookup (`poetry env info -p`) not implemented; tree-sitter indentation on Enter for python-mode.
- **T16–T19** `le-models` + `le-ollama`, worker adapters, `le-forge`, `le-mcp` bus.
- **T20–T25** (Enterprise) `yeditor-journal` (+ `eval-event`), agent tool surface, multiplexer with transactional branching (replaces the T12 demo worker), sessions, Change Browser, agentic eval harness.
- **T26** `le-reading` typography/focus mode; **T29–T30** Elisp shim and porter.

## 6. Known gaps and gotchas

- T14 reversal is partial by nature: definitions a package makes inside pre-existing packages remain until restart (the user is told before installing). Uses `ql:quickload` when Quicklisp is loaded, otherwise ASDF.
- T27: grammar libraries stay loaded after uninstall (unloading a shared library that tree-sitter objects awaiting finalization may reference is unsafe); toggling reuses them, so nothing accumulates. Indentation is verified line by line on `components/test-data/sample.py` via `calc-indent-from-query`, not yet through python-mode's Enter key.
- LSP stdio servers are grandchildren of the editor (async-process forks twice); after stop they may linger as zombies until init reaps them — tests treat a zombie as terminated.
- T28 records evals in `*python-evals*` and test runs in `*python-test-results*` (journal pending, T20). REPL output arrives through Lem's event loop, so tests check the process and the record, not the printed result. The LSP choice is re-made whenever the venv changes; it only switches between installed servers (no auto-install).
- Tests under the fake frontend do not run Lem's `process-file` (major mode from extension), so `le-lang-python` also recognises buffers by `.py` file type.
- T16: AI commands run the provider on a worker thread and deliver text through Lem's event loop; tests exercise the providers against `components/test-data/fake-ai-server.py` (Ollama, OpenAI-compatible and Anthropic protocols) and the edit operations with a canned provider. `le-models`/`le-ollama` are not installed by `enable-ephemeral-ui` yet (opt-in). Lem also ships `contrib/ollama` (separate, untouched).
- `e-server` has no test. T13 sessions are keyed by language (as Lem's workspaces are), not by project root; the missing-server case reports the install command but does not run it (no unattended installs); tests use a fake stdio server (`components/test-data/fake-lsp-server.py`) and never complete the async initialize handshake (no event loop under the fake frontend).
- T8: `use-component` options are validated but not passed to the component's install function;
  HMR uses `file-write-date` (1 s resolution).
- T9: components suspended at crash time are not reinstalled on recovery (choice, not specified by plan).
- T5 timer test only checks start/stop without error.
- The T12 HUD agent model (`*agent-tx*` in `components/hud.lisp`) is a demo stand-in until T22.
- Lem's `get-internal-real-time` is millisecond resolution on this SBCL; benchmarks use `sb-ext:get-time-of-day`.
- T11 palettes were written from each theme's public palette from memory; Gruvbox Soft uses Gruvbox Material accents and Tokyo Night Storm / One Dark use brighter foregrounds to meet the policy (documented in `themes-ergonomic.lisp`). The policy does not yet check comment contrast. Lem's other 180+ themes are not run through the policy (plan: mark them `ergonomic: false`, Phase 5).
- L0 dependencies under LLGPL/MPL are recorded exceptions (`scripts/ip-license-exceptions.txt`); resolve before a commercial release.
- `docs/` FSL text (`LICENSE-FSL`) was written from memory — verify against fsl.software before publishing.

## 7. Current work in progress

None. Next in plan order: T17 (worker adapters: Claude Code CLI, Hermes HTTP — Enterprise), T18 (`le-forge` — Enterprise), T19 (`le-mcp` bus).
