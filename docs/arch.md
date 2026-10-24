```
┌─────────────────────────────────────────────────────────────────────┐
│  L5  FORGE (agentic generative loop)                                │
│      natural language → generated CL component, tested, promoted     │
├─────────────────────────────────────────────────────────────────────┤
│  L4  AGENTIC LOOP + AI (le-models, le-agent, le-chat, le-mcp)        │
│      ollama · claude/fable · openai-compat · mcp · sessions          │
├─────────────────────────────────────────────────────────────────────┤
│  L3  COMPONENT ECOSYSTEM (le-themes, le-mode-line, le-lsp...)        │
│      UI composed of togglable components                             │
├─────────────────────────────────────────────────────────────────────┤
│  L2  UNIFIED CONTEXT (yeditor-context)                               │
│      one effect/coeffect context; dispatch; reactivity               │
├─────────────────────────────────────────────────────────────────────┤
│  L1  TRANSACTIONAL CORE (yeditor-tx)                                 │
│      effect log, inverses, undo groups, rollback, durability         │
├─────────────────────────────────────────────────────────────────────┤
│  L0  LEM FORK (core adapted to the "context paradigm")               │
│      global state mutations routed through the registry;             │
│      async boundaries; frontends ncurses/webview/SDL2                │
└─────────────────────────────────────────────────────────────────────┘
```

## Overview

Yeditor implements a spatiotemporal composable architecture built on top of Lem. It introduces a unified effect/coeffect context (L2), a transactional core with reversible effects (L1), and a component ecosystem (L3) that can be dynamically loaded. Higher layers provide AI-enhanced loops (L4) and a generative forge (L5). Each layer builds on lower layers without direct coupling, ensuring modularity and testability.

## The component model

Components are defined via `define-component` and can provide buffers, modes, keybindings, hooks, and more. They are registered in the component registry and instantiated on demand, respecting layer dependencies.

## Lifecycle

- registered → installed → active ⇄ suspended → uninstalled

## Effect kinds

| Kind | Description |
|------|-------------|
| buffer | Core text buffer |
| buffer-modification | Edits to buffer content |
| mode | Editor mode definition |
| hook | Event hook registration |
| keybinding | Keyboard shortcut mapping |
| dynamic-global | Dynamically scoped global variable |
| command | Interactive command |
| overlay/face | Visual overlay or face |
| timer | Scheduled timer |
| process | External process management |
| server | Language server protocol server |
| lsp-session | LSP session state |
| fdefinition-override | Function definition override |
| file-persist | Persistent file storage |
| ui-state | UI state snapshot |

## Config

Configuration lives in `.yeditor.lisp`. Hot-module-replacement (HMR) watches this file and reloads configuration without restarting the editor.

## Directory map

Inherited from Lem (kept stable to ease upstream merges) — top level:

```
yeditor/
├── src/                     # Lem core (L0) — minimal changes; src/ext/ = core extensions
├── extensions/              # larger inherited modules (Phase 2 turns some into components)
├── frontends/               # ncurses / webview / SDL2 (L0)
├── contrib/                 # inherited, load on demand
├── yeditor/                 # NEW: Yeditor packages (L1-L5)
│   ├── core/                # [FSL-1.1-MIT] L1-L3: yeditor-core.asd, package, version
│   │   ├── tx/              #   yeditor-tx: transactions, inverses, side-transactions
│   │   └── context/         #   yeditor-context: registry, reactivity, loader, durability
│   ├── effects/             # [FSL-1.1-MIT] L1: standard effects (buffer, mode, hook, ...)
│   ├── components/          # [FSL-1.1-MIT] L3: le-* components (Phase 2)
│   └── enterprise/          # [Proprietary] L4-L5: journal, agent/, forge/
├── docs/                    # EN docs (arch.md, component-author.md, ...)
├── tests/                   # inherited
├── lem.asd, yeditor.asd
├── Makefile, qlfile, qlfile.lock
├── README.md + README.pt-BR.md
├── .yeditor.lisp
├── LICENSE (license map), LICENSE-FSL, LICENSE-ENTERPRISE, THIRD_PARTY_NOTICES.md (Lem MIT), CLA.md
└── .github/workflows/       # test.yml (rove tests) + yeditor-ci.yml (Nix builds)
```

Note: the plan's `modes/` directory (language modes) does not exist in the
current upstream layout — language modes live under `src/ext/` and
`extensions/`; the map above reflects the real tree.

## Layer dependency rule

each layer uses only lower layers; no L5/L4/L3 component touches L0 directly - always via L1/L2.

## Dark/light theme component

`yeditor-components:le-dark-theme` mounts the warm Gruvbox Soft dark theme and
adds **Theme: dark** / **Theme: light** to the theme palette, alongside the
previous theme source. The light variant ports Lem's `emacs-light` with warm
paper/ink colors instead of pure black/white. The standard ephemeral UI installs
this component automatically after the ergonomic pack.

Focus a palette item to preview it; accept to save its canonical Lem theme name,
or cancel to restore the previous theme. Removing `le-dark-theme` restores the
mount-time theme, palette source and aliases through transaction effects; remove
it before the underlying ergonomic pack (LIFO). The existing palette command
`yeditor-palette-theme` is also available through the editor's MCP command tool.

The component suite checks 200 dark/light install/preview/accept/uninstall cycles
against color and style snapshots, plus restoration of the palette variables.
