# Formatting for Lem
This package exposes the `format-buffer` command and supports auto-formatting on save.

# Register formatters
Formatting for C, Go, and Common Lisp are included as defaults, but you can register other formatters with `register-formatter` and `register-formatters`

```lisp
;; register multiple formatters at once:
(register-formatters
  ('lem-go-mode:go-mode #'gofmt)
  ('lem-c-mode:c-mode   #'clang-format)
  ('lem-lisp-mode:lisp-mode #'indent-buffer))
    
;; register a single formatter
(register-formatter 'lem-c-mode #'clang-format)
```

Handlers take a buffer to format as an argument.
    
# Usage
You can either enable auto-formatting with `(setf lem-format:auto-format? t)` or manually use `M-x format-buffer`.  If no formatter is registered for the major mode, an error message is displayed.

# Default formatters
Currently the Go and C formatters depend on having `gofmt` and `clang-format` installed respectively, for C you also need a `.clang-format` spec file somewhere above the working file in your file tree (I keep Torvalds spec file at `~/.clang-format`, but a project-local spec will be found first).