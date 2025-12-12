# CLAUDE.md - Lem Editor

Lem is a text editor written in Common Lisp with multiple frontend support (terminal, GUI, browser).

## Quick Reference

```bash
# Build and run
make sdl2          # GUI version
make ncurses       # Terminal version

# Development
qlot install       # Install dependencies
make test          # Run tests
```

## Project Structure

```
src/                    # Core editor
├── lem.lisp            # Entry point
├── buffer/             # Text buffer system
├── window/             # Window management
├── mode.lisp           # Mode system
├── keymap.lisp         # Key bindings
├── defcommand.lisp     # Command definition
└── interface.lisp      # Frontend abstraction

frontends/              # UI implementations
├── sdl2/               # Desktop GUI
├── ncurses/            # Terminal
└── webview/            # Browser-based

extensions/             # Language modes (50+)
├── lisp-mode/          # Common Lisp with REPL
├── lsp-mode/           # LSP client
└── vi-mode/            # Vim emulation
```

## Architecture

Four-layer design: Frontends → Interface (lem-if:*) → Core → Extensions

Key abstractions:
- **Buffer**: Text storage with points, marks, undo history
- **Window**: Buffer display, organized in tree (splits) or floating
- **Mode**: Major (one per buffer) + Minor (multiple, toggleable)
- **Command**: Interactive operation via `define-command`

## Coding Standards

This project uses [code-contractor](https://github.com/rooms-dev/code-contractor) for automated PR review. All Common Lisp coding standards are defined in `contract.yml` at the repository root.

When writing code, refer to `contract.yml` for:
- Package and file structure rules
- Documentation requirements
- Style conventions (loop syntax, error handling, etc.)
- Functional programming guidelines

## Key Files for Common Tasks

| Task | Files |
|------|-------|
| Add command | `src/defcommand.lisp` for macro, `src/commands/` for examples |
| Add mode | `extensions/` for examples, inherit `language-mode` |
| Add frontend | `src/interface.lisp` for protocol, `frontends/` for impl |
| Buffer ops | `src/buffer/internal/buffer.lisp`, `src/buffer/internal/edit.lisp` |
| Window ops | `src/window/window.lisp` |

## Development Notes

- Use `lem-core` package symbols, avoid `lem::` internal access
- Frontends implement `lem-if:*` generic functions
- Modes register via `define-major-mode` / `define-minor-mode`
- Commands register via `define-command` with argument descriptors

## Configuration

User config locations (in order):
1. `$LEM_HOME/init.lisp`
2. `~/.config/lem/init.lisp`
3. `~/.lem/init.lisp`
4. `~/.lemrc`

## Documentation

- Architecture details: `docs/ARCHITECTURE.md`
- Extension development: `docs/extension-development.md`
- Online docs: https://lem-project.github.io/
