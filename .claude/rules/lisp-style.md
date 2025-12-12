# Common Lisp Style Guide

**All coding standards are defined in `contract.yml` at the repository root.**

## Quick Reference

Key rules from contract.yml:

- `defpackage_rule`: First form must be `defpackage` matching filename
- `docstring_rule`: All exported functions/methods/classes require docstrings
- `loop_keywords_rule`: Use `(loop :for ... :do ...)` with colons
- `error_handling_rule`: Use `editor-error` for user-facing errors
- `functional_style_rule`: Prefer explicit arguments over global state
- `trim_whitespace_rule`: No trailing whitespace, end files with newline

## Naming Conventions

- Functions/variables: `kebab-case`
- Special variables: `*earmuffs*`
- Constants: `+plus-signs+`
- Predicates: `-p` suffix (for functions, not user variables)

## Avoid

- `lem::internal-symbol` access - use exported API from `lem-core`
- `uiop:symbol-call` - rethink architecture instead
- `alexandria:curry` - prefer explicit higher-order functions
