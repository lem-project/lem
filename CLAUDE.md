# CLAUDE.md - Lem Editor Project Guide

## Project Overview

Lem is a text editor written in Common Lisp. It provides a live editing environment where users can see program results while editing and customize the editor in real-time. The editor supports multiple frontends (terminal, GUI, browser) through a clean abstraction layer.

**Version**: 2.3.0
**License**: MIT
**Primary Language**: Common Lisp (SBCL recommended)

## Architecture

### Four-Layer Design

```
Layer 4: Frontends      - Platform-specific UI (ncurses, SDL2, webview, server)
Layer 3: Interface      - Protocol abstraction (lem-if:* generic functions)
Layer 2: Core           - Editor kernel (buffer, window, command, mode)
Layer 1: Extensions     - Language modes, LSP, Vi-mode, Git integration
```

### Directory Structure

```
src/                    # Core editor
├── buffer/             # Text buffer system
├── window/             # Window management
├── display/            # Rendering system
├── commands/           # Built-in commands
├── common/             # Utilities (timer, hooks, color, queue)
└── ext/                # Core extensions (completion, grep, etc.)

frontends/              # UI implementations
├── ncurses/            # Terminal frontend
├── sdl2/               # Desktop GUI frontend
├── webview/            # Browser-based frontend
└── server/             # Remote access via JSON-RPC

extensions/             # Language modes and features
├── lisp-mode/          # Common Lisp development
├── lsp-mode/           # Language Server Protocol client
├── vi-mode/            # Vim emulation
├── legit/              # Git integration
└── [language]-mode/    # 50+ language modes

contrib/                # Community contributions
```

## Key Concepts

### Buffer
Text container with line-based storage. Each buffer has:
- A name (unique identifier)
- Optional file association
- Major mode (exactly one)
- Minor modes (zero or more)
- Points (cursor positions)
- Undo history

### Point
Position in a buffer (line + character offset). Three kinds:
- `:temporary` - Short-lived, for calculations
- `:left-inserting` - Stays left when text inserted
- `:right-inserting` - Moves right when text inserted

### Window
Displays a buffer. Organized in:
- Window tree (tiled splits)
- Floating windows (popups)
- Frame (top-level container)

### Mode
Customizes editing behavior:
- **Major mode**: Primary editing paradigm (one per buffer)
- **Minor mode**: Optional features (multiple per buffer)
- **Global mode**: System-wide behavior (emacs-mode, etc.)

### Command
Interactive operation invoked by key or M-x. Defined with `define-command`.

### Keymap
Maps key sequences to commands. Hierarchical lookup through active modes.

## Important Files

| File | Purpose |
|------|---------|
| `lem.asd` | ASDF system definition |
| `src/lem.lisp` | Entry point, initialization |
| `src/interface.lisp` | Frontend abstraction protocol |
| `src/interp.lisp` | Command loop, error handling |
| `src/mode.lisp` | Mode system (major/minor/global) |
| `src/keymap.lisp` | Key binding system |
| `src/defcommand.lisp` | Command definition macro |
| `src/buffer/internal/buffer.lisp` | Buffer implementation |
| `src/window/window.lisp` | Window implementation |
| `src/display/base.lisp` | Display rendering |

## Common Patterns

### Defining a Command

```lisp
(define-command my-command (arg) ((:string "Prompt: "))
  "Documentation string."
  (message "You entered: ~A" arg))
```

Argument descriptors: `:universal`, `:string`, `:number`, `:buffer`, `:file`, `:region`

### Defining a Major Mode

```lisp
(define-major-mode my-mode language-mode
    (:name "My Mode"
     :keymap *my-mode-keymap*
     :syntax-table *my-syntax-table*
     :mode-hook *my-mode-hook*)
  ;; Initialization code
  (setf (variable-value 'enable-syntax-highlight) t))
```

### Defining a Minor Mode

```lisp
(define-minor-mode my-minor-mode
    (:name "My Minor"
     :keymap *my-minor-keymap*
     :global nil  ; or t for global minor mode
     :enable-hook (lambda () ...)
     :disable-hook (lambda () ...))
  ;; Toggle code
  )
```

### Key Binding

```lisp
(define-key *global-keymap* "C-c C-c" 'my-command)
(define-key *my-mode-keymap* "M-." 'find-definition)
```

### Using Hooks

```lisp
(add-hook *after-init-hook* 'my-setup-function)
(add-hook (variable-value 'before-save-hook :buffer buffer) 'format-on-save)
```

### Buffer Operations

```lisp
(with-point ((p (current-point)))
  (insert-string p "text")
  (delete-character p 1)
  (line-start p)
  (line-end p))
```

## Frontend Protocol

Frontends implement the `implementation` class and these key generic functions:

```lisp
(lem-if:invoke implementation function)        ; Start UI loop
(lem-if:display-width implementation)          ; Screen width in chars
(lem-if:display-height implementation)         ; Screen height in chars
(lem-if:make-view implementation window ...)   ; Create window view
(lem-if:render-line implementation view ...)   ; Draw a line
(lem-if:update-display implementation)         ; Flush to screen
```

## Build & Run

### Dependencies
- SBCL (recommended) or other Common Lisp
- Quicklisp + Qlot for dependency management
- Platform-specific: ncurses, SDL2, or GTK4/WebKit

### Quick Start

```bash
# Install dependencies
qlot install

# Run with SDL2 frontend
make sdl2

# Run with terminal frontend
make ncurses

# Run tests
make test
```

### Configuration

User config location (in order of precedence):
1. `$LEM_HOME/init.lisp`
2. `~/.config/lem/init.lisp` (XDG)
3. `~/.lem/init.lisp`
4. `~/.lemrc`

## Development Guidelines

### Code Style
- Follow existing patterns in the codebase
- Use `lem-core` package for extensions
- Avoid `lem::` internal symbol access
- Document public APIs with docstrings

### Adding a Language Mode
1. Create `extensions/[lang]-mode/` directory
2. Define `.asd` system file
3. Create syntax table for highlighting
4. Define major mode inheriting `language-mode`
5. Add to `lem/extensions` dependencies in `lem.asd`

### Adding LSP Support
1. Define language spec with `define-language-spec`
2. Specify language server command and connection mode
3. Add root URI patterns for project detection

### Testing
- Use Rove test framework
- Tests in `lem-tests.asd` and mode-specific `.asd` files
- Run with `make test` or `.qlot/bin/rove lem-tests.asd`

## Debugging

### Logging
```lisp
(log:info "Message: ~A" value)
```
Logs to `~/.lem/debug.log` by default.

### REPL Access
With Lisp mode, connect to Swank for live debugging:
- `M-x slime` or `M-x start-lisp-repl`

### Common Issues
- **Undo not working**: Check `buffer-%enable-undo-p`
- **Keys not binding**: Verify keymap hierarchy and mode activation
- **Display issues**: Check `*implementation*` and view state

## Resources

- Documentation: https://lem-project.github.io/
- Source: https://github.com/lem-project/lem
- DeepWiki Analysis: https://deepwiki.com/lem-project/lem/
