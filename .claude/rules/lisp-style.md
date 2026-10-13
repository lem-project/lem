# Common Lisp Style Guide

**All coding standards are defined in `contract.yml` at the repository root.**

## Quick Reference

Key rules from contract.yml:

- `defpackage_rule`: First form must be `defpackage`, package name matches filename
- `file_structure_rule`: defpackage → defvar → keybindings → classes → functions
- `loop_keywords_rule`: Use `(loop :for ... :do ...)` with colons
- `naming_conventions_rule`: kebab-case, *earmuffs*, +constants+, -p suffix
- `docstring_rule`: Exported symbols require docstrings
- `internal_symbol_rule`: Use `lem:` not `lem::` internal symbols
- `error_handling_rule`: Use `editor-error` for user-facing errors
- `frontend_interface_rule`: Use `lem-if:*` protocol for frontend code
- `functional_style_rule`: Prefer explicit arguments over dynamic variables
- `alexandria_usage_rule`: Allowed utilities, avoid `curry`
- `macro_style_rule`: Keep macros small, use `call-with-*` pattern
