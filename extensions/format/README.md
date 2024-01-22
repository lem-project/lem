# Formatting for Lem
This package exposes the `format-buffer` command and supports auto-formatting on save.

# Register formatters
Formatting for JavaScript, JSON, C, Go, and Common Lisp are included as defaults, but you can register other formatters with `register-formatter` and `register-formatters`

```lisp
;; register multiple formatters at once:
(register-formatters
  (lem-go-mode:go-mode #'gofmt)
  (lem-c-mode:c-mode   #'clang-format))
    
;; register a single formatter
(register-formatter lem-c-mode #'clang-format)
```

Handlers take a buffer to format as an argument.
    
# Usage
You can either enable auto-formatting with `(setf lem-format:auto-format? t)` or manually use `M-x format-buffer`.  If no formatter is registered for the major mode, an error message is displayed.

# Default formatters
Current formatters and their dependencies:
- JavaScript: prettier
- JSON:       prettier
- C:          clang-format
- Go:         gofmt
- Lisp:       none

The C formatter looks for the nearest `.clang-format` spec file walking up the file tree.  I keep Torvalds' spec file at `~/.clang-format`, but a project-local one would override that.