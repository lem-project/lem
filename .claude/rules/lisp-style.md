# Common Lisp Style Guide for Lem

## Naming Conventions

- Functions/variables: `kebab-case` (e.g., `find-buffer`, `current-point`)
- Special variables: `*earmuffs*` (e.g., `*global-keymap*`)
- Constants: `+plus-signs+` (e.g., `+default-tab-size+`)
- Predicates: end with `-p` (e.g., `buffer-modified-p`)
- Conversion: `x-to-y` (e.g., `point-to-lsp-position`)

## Package Structure

```lisp
(defpackage :lem-my-extension
  (:use :cl :lem)              ; Use lem, not lem-core directly
  (:export :my-mode
           :*my-mode-hook*))
(in-package :lem-my-extension)
```

## Conditions and Restarts

Use `editor-error` for user-facing errors:

```lisp
(editor-error "File not found: ~A" filename)
```

## Documentation

- All exported symbols should have docstrings
- Use `@param` and `@return` in complex functions

## Avoid

- `lem::internal-symbol` access - use exported API
- Modifying global state without proper cleanup
- Blocking the main thread - use `bt2:make-thread` for long operations
