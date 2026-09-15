# Odin mode for Lem

## Features

### Syntax Highlighting

Keywords, built-in types, builtin procedures, constants (`true`/`false`/`nil`),
operators (`::`, `:=`, `->`, `---`, `..=`, `..<`, `..`), numeric literals,
strings (`"..."`, `'c'`, backtick raw strings), attributes (`@(...)`) and
directives (`#partial`, ...) are all highlighted, along with `//` line
comments and nestable `/* */` block comments.

`name :: proc` and `name :: struct`/`union`/`enum`/`bit_field`/`bit_set`
declarations highlight `name` as a function or type name respectively.

### Indentation

Indentation is brace-based (`{ }`, `[ ]`, `( )`), tracks `case` labels inside
`switch` statements, and de-dents `where` clauses relative to the
declaration they constrain.

### Formatting

`C-c C-f` (`odin-format-buffer`) formats the current buffer with
[`odinfmt`](https://github.com/DanielGavin/ols) (ships with ols), preserving
the cursor's line.

### Language Server

Odin mode connects to [ols](https://github.com/DanielGavin/ols) (the Odin
Language Server) automatically when editing a `.odin` file inside a project
containing `ols.json` or `odinfmt.json`. Install `ols` and make sure it is on
your `PATH`; see the ols README for installation instructions.

## File Type

Files with the `.odin` extension activate `odin-mode`.
