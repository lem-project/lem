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

Yeditor implements a spatiotemporal composable architecture built on top of Lem. It introduces a unified effect/coeffect context (L2), a transactional core with reversible effects (L1), and a component ecosystem (L3) that can be dynamically loaded. Higher layers provide AI‑enhanced loops (L4) and a generative forge (L5). Each layer builds on lower layers without direct coupling, ensuring modularity and testability.

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

Configuration lives in `.yeditor.lisp`. Hot‑module‑replacement (HMR) watches this file and reloads configuration without restarting the editor.

## Directory map

```
.
├─ .hermes
├─ .qwen
├─ contrib
├─ docs
├─ extensions
├─ frontends
├─ resources
├─ scripts
├─ specs
├─ src
├─ tests
└─ yeditor
```

## Layer dependency rule

each layer uses only lower layers; no L5/L4/L3 component touches L0 directly - always via L1/L2.
