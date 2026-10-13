# Extension Development Guide

This guide explains how to create extensions for the Lem editor.

## Directory Structure

Extensions live in `/extensions/`. A minimal extension requires:

```
extensions/my-mode/
├── lem-my-mode.asd    # ASDF system definition
└── my-mode.lisp       # Main implementation
```

Larger extensions may include:

```
extensions/my-mode/
├── lem-my-mode.asd
├── package.lisp       # Package definition
├── my-mode.lisp       # Mode definition
├── syntax-parser.lisp # Syntax highlighting
├── commands.lisp      # Command definitions
└── lsp-config.lisp    # LSP integration
```

## Minimal Extension Example

### System Definition

`lem-my-mode.asd`:

```lisp
(defsystem "lem-my-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "my-mode")))
```

### Mode Implementation

`my-mode.lisp`:

```lisp
(defpackage :lem-my-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*my-mode-hook*
           :my-mode))
(in-package :lem-my-mode)

(defvar *my-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :paren-pairs '((#\( . #\))
                  (#\{ . #\})
                  (#\[ . #\]))
   :string-quote-chars '(#\")
   :line-comment-string "#"))

(define-major-mode my-mode language-mode
    (:name "MyLang"
     :keymap *my-mode-keymap*
     :syntax-table *my-syntax-table*
     :mode-hook *my-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'line-comment) "#"))

(define-file-type ("my" "myl") my-mode)
```

## Core Macros

### define-major-mode

Creates a major mode with syntax highlighting and keybindings.

```lisp
(define-major-mode mode-name parent-mode
    (:name "Display Name"
     :keymap *mode-keymap*
     :syntax-table *syntax-table*
     :mode-hook *mode-hook*
     :formatter 'formatter-function)
  ;; Initialization body
  (setf (variable-value 'variable-name) value))
```

**Parameters:**
- `mode-name` - Symbol naming the mode
- `parent-mode` - Parent mode to inherit from (typically `language-mode` or `fundamental-mode`)
- `:name` - Human-readable name shown in modeline
- `:keymap` - Variable for mode-specific keybindings
- `:syntax-table` - Syntax table for highlighting
- `:mode-hook` - Hook variable run when mode activates
- `:formatter` - Optional formatting function

### define-minor-mode

Creates a toggleable minor mode.

```lisp
(define-minor-mode mode-name
    (:name "Display Name"
     :description "What this mode does."
     :keymap *mode-keymap*
     :global nil
     :enable-hook 'on-enable
     :disable-hook 'on-disable))
```

**Parameters:**
- `:global` - When `t`, applies to all buffers
- `:enable-hook` - Function called when mode is enabled
- `:disable-hook` - Function called when mode is disabled

### define-command

Defines an interactive command callable via `M-x`.

```lisp
(define-command command-name (args) (arg-descriptors)
  "Documentation string."
  body)
```

**Argument Descriptors:**

| Descriptor | Description |
|------------|-------------|
| `(:universal)` | Prefix argument, defaults to 1 |
| `(:universal-nil)` | Prefix argument, defaults to nil |
| `(:string "Prompt: ")` | Prompts for string input |
| `(:number "Prompt: ")` | Prompts for numeric input |
| `(:buffer)` | Prompts for buffer selection |
| `(:file)` | Prompts for existing file |
| `(:new-file)` | Prompts for new file path |
| `(:region)` | Passes region start and end points |

**Examples:**

```lisp
;; No arguments
(define-command my-hello () ()
  "Display a greeting."
  (message "Hello!"))

;; With prefix argument
(define-command forward-n-lines (&optional (n 1)) (:universal)
  "Move forward N lines."
  (next-line n))

;; With region
(define-command upcase-region (start end) (:region)
  "Convert region to uppercase."
  (uppercase-region start end))

;; With string prompt
(define-command search-word (word) ((:string "Search: "))
  "Search for WORD."
  (search-forward word))
```

### define-key / define-keys

Binds keys to commands.

```lisp
;; Single binding
(define-key *my-mode-keymap* "C-c C-c" 'my-command)

;; Multiple bindings
(define-keys *my-mode-keymap*
  ("C-c C-c" 'compile)
  ("C-c C-r" 'run)
  ("M-." 'find-definition))
```

**Key notation:**
- `C-` = Control
- `M-` = Meta (Alt)
- `S-` = Shift
- `Super-` = Super (Windows/Command key)

### define-file-type

Associates file extensions with a mode.

```lisp
(define-file-type ("ext1" "ext2") mode-name)
```

**Examples:**

```lisp
(define-file-type ("js" "jsx" "mjs") js-mode)
(define-file-type ("rb" "Gemfile") ruby-mode)
```

## Syntax Highlighting

### Basic Syntax Table

```lisp
(defvar *my-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_ #\-)
   :paren-pairs '((#\( . #\))
                  (#\{ . #\})
                  (#\[ . #\]))
   :string-quote-chars '(#\" #\')
   :block-string-pairs '(("\"\"\"" . "\"\"\""))
   :line-comment-string "#"))
```

### TextMate Grammar Patterns

For advanced syntax highlighting, use TextMate-style patterns:

```lisp
(defun make-tmlanguage-my ()
  (make-tmlanguage
   :patterns
   (make-tm-patterns
    ;; Line comment
    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
    ;; Block comment
    (make-tm-region "/\\*" "\\*/" :name 'syntax-comment-attribute)
    ;; Keywords
    (make-tm-match "\\b(if|else|while|for|return)\\b"
                   :name 'syntax-keyword-attribute)
    ;; Built-in functions
    (make-tm-match "\\b(print|input|len)\\b"
                   :name 'syntax-builtin-attribute)
    ;; Strings
    (make-tm-string-region "\""))))

;; Apply to syntax table
(set-syntax-parser *my-syntax-table* (make-tmlanguage-my))
```

**Available attributes:**
- `syntax-comment-attribute`
- `syntax-keyword-attribute`
- `syntax-string-attribute`
- `syntax-builtin-attribute`
- `syntax-constant-attribute`
- `syntax-function-name-attribute`
- `syntax-variable-attribute`
- `syntax-type-attribute`

## Editor Variables

Common variables to set in mode initialization:

```lisp
(define-major-mode my-mode language-mode (...)
  ;; Enable syntax highlighting
  (setf (variable-value 'enable-syntax-highlight) t)
  ;; Use spaces instead of tabs
  (setf (variable-value 'indent-tabs-mode) nil)
  ;; Tab width
  (setf (variable-value 'tab-width) 4)
  ;; Custom indent function
  (setf (variable-value 'calc-indent-function) 'my-calc-indent)
  ;; Line comment prefix
  (setf (variable-value 'line-comment) "//")
  ;; Definition lookup
  (setf (variable-value 'find-definitions-function) 'my-find-def)
  ;; Reference search
  (setf (variable-value 'find-references-function) 'my-find-refs))
```

## Hooks

Add behavior at specific points:

```lisp
(define-major-mode my-mode language-mode (...)
  ;; After save
  (add-hook (variable-value 'after-save-hook :buffer (current-buffer))
            'my-on-save)
  ;; Before buffer is killed
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer))
            'my-cleanup)
  ;; On text change
  (add-hook (variable-value 'after-change-functions :buffer (current-buffer))
            (lambda (start end old-len)
              (my-on-change start end))))
```

## LSP Integration

To add Language Server Protocol support, create `lsp-config.lisp`:

```lisp
(uiop:define-package :lem-my-mode/lsp-config
  (:use :cl))
(in-package :lem-my-mode/lsp-config)

(lem-lsp-mode:define-language-spec (my-spec lem-my-mode:my-mode)
  :language-id "mylang"
  :root-uri-patterns '("my.config" "package.json")
  :command '("my-language-server" "--stdio")
  :install-command "npm install -g my-language-server"
  :connection-mode :stdio)
```

Update the system definition:

```lisp
(defsystem "lem-my-mode"
  :depends-on ("lem/core" "lem-lsp-mode")
  :serial t
  :components ((:file "my-mode")
               (:file "lsp-config")))
```

## Registering the Extension

Add your extension to `lem.asd` in the `lem/extensions` system:

```lisp
(defsystem "lem/extensions"
  :depends-on (
               ;; ... existing extensions
               "lem-my-mode"))
```

## Reference Extensions

Study these extensions for patterns:

| Extension | Complexity | Features |
|-----------|------------|----------|
| `json-mode` | Minimal | Basic syntax, inherits from js-mode |
| `ruby-mode` | Standard | Syntax, indent, LSP |
| `markdown-mode` | Medium | Custom parser, preview |
| `lisp-mode` | Complex | REPL, debugger, inspector |

## Testing

Create a test system:

```lisp
(defsystem "lem-my-mode/tests"
  :depends-on ("lem-my-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
```

Run tests:

```lisp
(asdf:test-system "lem-my-mode")
```
