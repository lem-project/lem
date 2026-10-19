[English](README.md) · Português

# Yeditor

[![CI](https://github.com/Yuri-Albuquerque/yeditor/actions/workflows/test.yml/badge.svg)](https://github.com/Yuri-Albuquerque/yeditor/actions/workflows/test.yml)

Yeditor is a **spatiotemporally composable, agentic code editor** built in Common Lisp on top of [Lem](https://github.com/lem-project/lem) (MIT).

Yeditor is **open core**: the transactional engine and SDK are source-available under FSL-1.1-MIT, and the agentic suite (agent host, transactional branching, Eval-to-Journal, forge) is commercial. See [License & Commercial Use](#license--commercial-use).

## Why Yeditor

### Easy as VSCode

- Dashboard; Command Palette fuzzy (Ctrl-Shift-P, unified with M-x); settings in `.yeditor.lisp` with auto-reload (HMR); LSP auto-config per language; clean UI (zero chrome)

### Productive as Emacs

- Emacs-style keybindings; macros; transient menus; integrated REPL; the whole editor is a REPL; everything scriptable in Common Lisp

### Easy Ollama/Claude integration

- `le-ollama` (auto-detect server at 127.0.0.1:11434; model pull inside the editor; inline chat; explain/rewrite/document);
- `le-claude-bridge`;
- **MCP server reused from Lem** (`claude mcp add --transport http lem http://localhost:7890/mcp`)

## The core idea: reversible effects and reactive coeffects

Every editor mutation is a **transaction** carrying its inverse (captured at install time). Component context changes are classified against declared **coeffects** (buffer-mode, project-type…) governing activation. The paradigm follows "A Programming Paradigm for Spatiotemporal Composability" (Shi/Zhang/Cui, 2026, arXiv:2608.25512).

## Status

### What works today (inherited from Lem)

- ncurses/webview/SDL2 frontends, LSP, tree-sitter, client-server, MCP server, Copilot mode, +180 themes (SDL2), 12k commits upstream heritage.

### What we're building (Phase 1)

- The transactional core — `yeditor-tx` (reversible-effect log, undo groups, rollback, durability), `yeditor-context` (unified effect/coeffect context), `yeditor-sdk` (define-component DSL), standard inverses for 15 effect kinds.

## What Yeditor is NOT

- NOT a visual VSCode clone — keyboard-first with VSCode-style onboarding
- DOES NOT run ELisp packages (different language; best-effort compatibility layer is future work)
- NOT a general plugin framework for arbitrary apps — an editor
- NOT OSI open source — Core is source-available (FSL-1.1-MIT), Enterprise is commercial

## Install

Yeditor tracks upstream Lem; installation follows the same paths as Lem. See the Lem upstream documentation (https://lem-project.github.io/) and the nightly builds at https://github.com/lem-project/lem/releases/tag/nightly-latest.

## Roadmap

- Phase 0 — Fork + CI green on first build (Oct 2026)
- Phase 1 — Transactional core M1-M5, T1-T10 (Nov 2026 – Mar 2027)
- Phase 2 — Component ecosystem, T11-T15 (Jun 2027)
- Phase 3 — AI + forge (T16-T19, Sep 2027)
- Phase 4 — Perceived speed + polish (Nov 2027)

## License & Commercial Use

**Build on Yeditor.** The Yeditor Core — the reversible-effect transaction engine
(`yeditor-tx`), the component context and SDK, and the standard effects — is
source-available under the [Functional Source License 1.1 (MIT future)](LICENSE-FSL).
Read it, audit it, build it, write and share components for it, use it at work and in
research. The only thing you can't do is ship a competing commercial product or service
built on it. Every release becomes plain MIT two years after it ships.

**Yeditor Enterprise** — the agent multiplexer and transactional host, parallel
transactional branching, Eval-to-Journal and the generative forge — is commercial software
([LICENSE-ENTERPRISE](LICENSE-ENTERPRISE)), available under a commercial license.

| | License |
|---|---|
| `yeditor/core/`, `yeditor/effects/`, `yeditor/components/` | FSL-1.1-MIT |
| `yeditor/enterprise/` | Proprietary (All Rights Reserved) |
| Code inherited from Lem | MIT — [THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md) |

The full map is in [LICENSE](LICENSE); the IP model is described in
[docs/IP_STRATEGY.md](docs/IP_STRATEGY.md).

**Commercial, enterprise and partnership licensing:** contact Yuri Flores Albuquerque
([@Yuri-Albuquerque](https://github.com/Yuri-Albuquerque)).

### Contributing

Pull requests are welcome after signing the [Contributor License Agreement](CLA.md). You keep
the copyright in your work and grant a license that lets it ship in every edition of Yeditor.

## Citation

```bibtex
@misc{shi2026cordis,
  title         = {A Programming Paradigm for Spatiotemporal Composability},
  author        = {Shi, Yifan and Zhang, Wei and Cui, Tianyi},
  year          = {2026},
  eprint        = {2608.25512},
  archivePrefix = {arXiv},
  primaryClass  = {cs.PL},
  url           = {https://arxiv.org/abs/2608.25512},
}
```

Based on Lem (lem-project/lem), MIT — 12,099 commits of upstream heritage.
