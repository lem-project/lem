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

Test status at last update: 10 test systems, **14,731 assertions, 0 failures** on the host (tree-sitter
tests are skipped there: no libtree-sitter). Inside the CI image (`docker/Dockerfile.test`, which
has libtree-sitter and the Python grammar) the components suite runs everything, tree-sitter included (14,855 assertions at last update).
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
└── enterprise/           Proprietary — agent/workers.lisp (T17), forge/forge.lisp (T18) + tests; journal to come
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
| T19 | `le-mcp`: worker bus on Lem's MCP server (`http://127.0.0.1:7890/mcp`), started as an effect (an already-running server is reused, not stopped). Tools: `yeditor_list_components`, `_component_status`, `_toggle_component`, `_transaction_log`, `_worker_register` (protocol `yeditor-worker/1` + capability check), `_worker_unregister`, `_task_next`, `_task_report`; tasks and observations also pushed as JSON-RPC notifications on the SSE stream. Lisp API: `submit-task`, `next-task`, `report-task`, `task-observations`. Handshake + task delivery p95 ≈ 2–3 ms (budget 50 ms). L0: SSE stream, thread-safe tools, `unregister-tool` | `components/mcp.lisp`, `extensions/mcp-server/` |
| T17 (Enterprise) | Worker adapters as components: `le-worker-claude-code` (runs `claude -p … --output-format json --max-turns N --mcp-config <editor MCP>`; CLI keeps its own credentials) and `le-worker-hermes` (provisional HTTP contract: `GET /health`, `POST /tasks` → `{status, observation}`). Installing registers a worker on the le-mcp bus and starts a dispatcher thread (take task → run in agent → report observation); uninstalling stops it and unregisters. Discovery ping, per-task timeout, step budget, extra CLI args configurable | `enterprise/agent/workers.lisp` |
| T18 (Enterprise) | `le-forge` / `forge-component`: request → model → one `define-component` form → static checks (size ≤ 16 KB, `*read-eval*` off, a single `define-component`, only whitelisted operators — safe effects keybinding/variable/editor-variable/hook/theme/timer/buffer plus a harmless CL subset, raw `install-effect` only with `:undo-thunk`, hook/timer functions must be lambdas or `#'` allowed, variable values literals only) → reversibility test (install/uninstall ×5 comparing a fingerprint of buffers, global key bindings, hooks, theme, modeline) → promote to `~/.yeditor/components/<name>.lisp` or discard leaving nothing. Command `yeditor-forge` (`C-c a f` via `le-forge`) | `enterprise/forge/forge.lisp` |
| Licensing | Open-core licenses, CLA, IP strategy, header/dependency CI gate | root `LICENSE*`, `docs/IP_STRATEGY.md` |
| T20 (Enterprise) | `yeditor-journal`: observable workspace memory (file opens, saves with line diffs, evals via `eval-event`), append ~7 µs, query by path ~3 µs, truncation/rotation, watch | `enterprise/journal/journal.lisp` |
| IDE panels (Hermes) | `le-sidebar` (project tree on Lem's filer, `C-c b` toggle, `C-c C-b` focus, `q` back to the editor, `r` refresh), `le-terminal` (`C-c j`, lem-terminal), `le-workspace-layout` (`C-c w`, on demand only), `le-hermes-chat` (Enterprise, `C-c h`, runs the `hermes` CLI; in the chat `C-c C-e` returns to the editor, `C-c C-k` closes) | `components/sidebar,terminal,workspace-layout.lisp`, `enterprise/agent/hermes-chat.lisp` |
| T21 (partial) + T12-B | Director tools on the MCP bus: `yeditor_editor_run_command`, `_open_and_present` (single / side-by-side), `_eval_and_annotate` (Python REPL + virtual-text annotation, buffer untouched), `_propose_checkpoint` (opens the HUD agent transaction). `le-director-cockpit`: F1 intent bar → Hermes worker or `*intent-dispatcher*` (set reversibly by `le-hermes-chat`) | `components/mcp.lisp`, `components/director-cockpit.lisp` |
| T21 (Enterprise) | Tool surface `le-agent-tools-*` (each group a component; MCP tools `yeditor_workspace_read/list/glob/search/edit`, `yeditor_terminal_run`, `yeditor_lsp_diagnostics`, `yeditor_git_status/diff`, `yeditor_journal_query`, `yeditor_agent_transactions`, `yeditor_ai_complete`; Lisp API of the same names). Sandbox: paths outside the project root (also via `..` or symlinks) and binary files refused, each refusal journaled as `audit-event`. Edits go to editor buffers inside the agent transaction; each edit is a kind-2 side-transaction named `<agent-tx-id>/<n>`; another worker cannot edit inside an open transaction; rollback leaves disk byte-identical and closes buffers the agent opened; commit saves through the editor, journaled as `agent:<worker>` with `tx_id`. `le-agent-host` installs workspace/lsp/git/journal groups and the payload injector (tasks on the bus start with recent eval-events + tracebacks, edits, runs/tests and LSP diagnostics). Terminal and AI groups are opt-in. Core support: HUD agent tx rebuilt on side-transactions (`agent-tx-replace`, `agent-tx-open-file`, `*agent-tx-close-functions*`), `side-forget`, bus `*payload-injector*`; L0 `lem-lsp-mode:buffer-diagnostic-list` | `enterprise/agent/tools.lisp`, `components/hud.lisp`, `core/tx/side.lisp` |
| Tutorial | `le-tutorial` (in `enable-ephemeral-ui`) makes Lem's `tutorial` command and the start screen's "Tutorial (t)" open the Yeditor tutorial: quiet screen, files and sidebar (mouse), project files, windows and terminal, themes (palette preview), Python, agent transactions (hands-on with `yeditor-demo-agent`), Director Mode / Hermes chat, Git, components. `M-x yeditor-tutorial`; Lem's editing lessons stay as `M-x yeditor-tutorial-editing`. L0 `lem-tutor`: `*tutorial-file*`, `open-tutorial`, a working copy and progress file per tutorial, leading `;;;;` header lines (license) not copied into the working copy. Exercises were checked in the running editor | `components/tutorial.lisp`, `components/tutorial/yeditor-tutorial.txt`, `extensions/lem-tutor/lem-tutor.lisp` |
| Mouse (terminal) | `lem-mouse-sgr1006` (L0 contrib) now feeds Lem's core mouse events like SDL2: clicks run clickable text (filer/sidebar items, tab bar, buttons), focus windows, place the cursor; double/triple click selects; drag separators; wheel scrolls the window under the pointer. Loaded by the ncurses launcher | `contrib/mouse-sgr1006/main.lisp`, `scripts/run-yeditor-ncurses.lisp` |

Extra effects added for T11/T12: `e-editor-variable` (Lem editor variables), `e-window` (side split, kind 15), `e-theme` (apply a color theme without saving it).

## 5. Left to do (in plan order)

- **T12 remainder**: `le-tabs` (off by default), per-segment modeline toggles, palette preview for buffers / LSP symbols / journal entries.
- **T27 remainder**: query packs for TypeScript/Rust/Go/CL (only Python exists), `textobjects.scm`/folding, tree-sitter indentation wired to python-mode's `calc-indent-function` on Enter (belongs with T28).
- **T28 remainder**: packs for TypeScript/Rust/Go/CL following `lang-python.lisp`; eval and test records move to `yeditor-journal` when T20 exists; poetry venv lookup (`poetry env info -p`) not implemented; tree-sitter indentation on Enter for python-mode.
- **T21 remainder**: `terminal/run` uses a plain subprocess, not the PTY of `le-terminal`; `lsp/symbols` and `lsp/goto-def` not implemented; `_open_and_present` window splits as effects (today not reversible); the Director tools in `components/mcp.lisp` (`yeditor_editor_*`) still bypass the sandbox; verify the `hermes` CLI flags used by `le-hermes-chat` (`chat -q … --oneshot -Q --format stream-json [--resume]`) against the real CLI; wire `le-agent-host` into the launcher once T22 decides routing.
- **T22–T25** (Enterprise) multiplexer with transactional branching (replaces the T12 demo worker), sessions, Change Browser, agentic eval harness.
- **Syntax highlighting by default**: `le-treesitter-packs` is opt-in (needs libtree-sitter + `M-x treesitter-install-grammar`); Python otherwise uses python-mode's regex highlighting. Enable the pack in `enable-ephemeral-ui` when the grammar is present, and add packs for other languages (T27 remainder).
- **Tutorial remainder**: update the text with every user-visible change (keys live in `components/tutorial/yeditor-tutorial.txt`); add chapters for the AI commands (`C-c a …`, once `le-models` is on by default), the agent tools / T22 modes, and the planned top-left menu; a pt-BR translation; the theme palette needs Tab before the list shows — opening it directly would simplify chapter 4.
- **Mouse remainder**: hover (needs SGR 1003 any-motion), a right-click context menu that works in the terminal, clickable HUD buttons (commit / rollback) and modeline segments.
- **T26** `le-reading` typography/focus mode; **T29–T30** Elisp shim and porter.

## 6. Known gaps and gotchas

- T14 reversal is partial by nature: definitions a package makes inside pre-existing packages remain until restart (the user is told before installing). Uses `ql:quickload` when Quicklisp is loaded, otherwise ASDF.
- T27: grammar libraries stay loaded after uninstall (unloading a shared library that tree-sitter objects awaiting finalization may reference is unsafe); toggling reuses them, so nothing accumulates. Indentation is verified line by line on `components/test-data/sample.py` via `calc-indent-from-query`, not yet through python-mode's Enter key.
- LSP stdio servers are grandchildren of the editor (async-process forks twice); after stop they may linger as zombies until init reaps them — tests treat a zombie as terminated.
- T28 records evals in `*python-evals*` and test runs in `*python-test-results*` (journal pending, T20). REPL output arrives through Lem's event loop, so tests check the process and the record, not the printed result. The LSP choice is re-made whenever the venv changes; it only switches between installed servers (no auto-install).
- Tests under the fake frontend do not run Lem's `process-file` (major mode from extension), so `le-lang-python` also recognises buffers by `.py` file type.
- T16: AI commands run the provider on a worker thread and deliver text through Lem's event loop; tests exercise the providers against `components/test-data/fake-ai-server.py` (Ollama, OpenAI-compatible and Anthropic protocols) and the edit operations with a canned provider. `le-models`/`le-ollama` are not installed by `enable-ephemeral-ui` yet (opt-in). Lem also ships `contrib/ollama` (separate, untouched).
- T19 events are broadcast to every session (workers filter by `worker_id`); the toggle tool runs on Lem's main thread and is not exercised over HTTP in tests (no event loop under the fake frontend). Hunchentoot logs each request to stdout during tests.
- T17: tested with a fake `claude` binary and a fake HTTP worker (`enterprise/agent/test-data/`); the real Claude Code CLI flags (`-p`, `--output-format json`, `--max-turns`, `--mcp-config`) were not verifiable on this machine (CLI not installed). The Hermes HTTP contract is provisional — align it with Hermes' real API. Worker edits are not yet confined to `agent-tx-id` side-transactions (T21/T22).
- T18: tests use a canned provider instead of a fake HTTP LLM; promoted components are saved but not auto-loaded at startup (loader integration pending). The operator whitelist is deliberately narrow — widening it needs a security review (process/server/fdefinition/environment effects are excluded on purpose).
- `e-server` has no test. T13 sessions are keyed by language (as Lem's workspaces are), not by project root; the missing-server case reports the install command but does not run it (no unattended installs); tests use a fake stdio server (`components/test-data/fake-lsp-server.py`) and never complete the async initialize handshake (no event loop under the fake frontend).
- T8: `use-component` supports explicit `:config` and legacy flat options; installers
  receive options under `:config` in their context. HMR compares mtime and content
  snapshots off-thread, then reconciles on the editor thread. See
  `docs/t8-config-hmr-validation.md` for ownership, rollback and runtime tests.
- T9: components suspended at crash time are not reinstalled on recovery (choice, not specified by plan).
- T5 timer test only checks start/stop without error.
- The T12 HUD agent model (`*agent-tx*` in `components/hud.lisp`) is a demo stand-in until T22.
- Lem's `get-internal-real-time` is millisecond resolution on this SBCL; benchmarks use `sb-ext:get-time-of-day`.
- T11 palettes were written from each theme's public palette from memory; Gruvbox Soft uses Gruvbox Material accents and Tokyo Night Storm / One Dark use brighter foregrounds to meet the policy (documented in `themes-ergonomic.lisp`). The policy does not yet check comment contrast. Lem's other 180+ themes are not run through the policy (plan: mark them `ergonomic: false`, Phase 5).
- L0 dependencies under LLGPL/MPL are recorded exceptions (`scripts/ip-license-exceptions.txt`); resolve before a commercial release.
- `docs/` FSL text (`LICENSE-FSL`) was written from memory — verify against fsl.software before publishing.

- Clicking the sidebar runs the item (open file / expand folder) but, as in Lem's core, does not move focus into it; `q` only acts when the sidebar is focused (`C-c C-b`). In python buffers `C-c C-b` is shadowed by le-lang-python's send-buffer — click or use `C-x o` there.
- The ncurses launcher takes `YEDITOR_MCP_PORT` (default 7890) so a second editor can run beside one already serving the port; a component that fails to install is reported and the others still install.

- T21: the sandbox is lexical plus a symlink check on existing paths; a symlink created later inside the root is caught only once it exists. `terminal/run` is not sandboxed beyond its working directory — keep `le-agent-tools-terminal` opt-in until T22's `ask` mode gates commands.
- The Python REPL (`python3 -i -u`) carries a watchdog thread that exits when the editor process disappears; before it, REPLs of crashed editors spun at 100% CPU forever (five were found and killed on 2026-10-18).
- Lem's `lem-language-server/micros-tests` SIMPLE-EVAL-TEST fails on this machine (`GIVE-UP-CONNECTION-TO-SERVER`), also on commits before T21 — environment, not Yeditor. `.qlot/bin/rove` needs `ros`; run Lem's suite with `sbcl --load .qlot/setup.lisp --eval '(asdf:load-system :lem-tests)' --eval '(rove:run :lem-tests)'`.
- Tests that start `le-mcp` must use a free port (`free-port`): a running editor holds 7890.

## 7. Current work in progress

None. Next in plan order: T22 (`le-agent-multiplexer`: routing rules, context collection before acting, `dry-run`/`ask`/`auto` modes, parallel branching on shadow buffers with `tx:promote-branch`, "Revert agent session"), then T23–T25. Planned after the engine works (user request, 2026-10-18): a tmux/terminal buffer running Hermes with the project files visible to it, and a discreet top-left menu (open file, theme, terminal, project folder).
