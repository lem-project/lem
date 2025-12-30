# Quickstart: TOML Mode Extension

**Date**: 2025-12-30
**Feature**: 001-toml-mode

## Prerequisites

- Lem editor built with tree-sitter support
- tree-sitter-toml grammar installed (via `ts:load-language-from-system`)

## Installation

### Option 1: Manual Load

```lisp
(ql:quickload :lem-toml-mode)
```

### Option 2: Init File

Add to `~/.config/lem/init.lisp`:

```lisp
(ql:quickload :lem-toml-mode)
```

## Usage

### Automatic Activation

Open any `.toml` file and TOML mode activates automatically:

```
M-x find-file RET Cargo.toml RET
```

### Manual Activation

Switch to TOML mode in any buffer:

```
M-x toml-mode RET
```

### Comment/Uncomment

Use standard Lem comment commands:

| Command | Default Binding | Description |
|---------|-----------------|-------------|
| `comment-or-uncomment-region` | `M-;` | Toggle comments |
| `comment-line` | N/A | Comment current line |

## Syntax Highlighting

TOML elements are highlighted as follows:

| Element | Example | Color (default theme) |
|---------|---------|----------------------|
| Table headers | `[package]` | Type color |
| Keys | `name =` | Variable color |
| Strings | `"value"` | String color |
| Numbers | `42`, `3.14` | Constant color |
| Booleans | `true`, `false` | Keyword color |
| Comments | `# comment` | Comment color |
| Dates | `2024-01-15` | Constant color |

## Verification

Check mode is working:

1. Open a TOML file
2. Verify mode line shows "Toml"
3. Verify syntax highlighting is applied
4. Test commenting with `M-;`

## Troubleshooting

### No Syntax Highlighting

Check tree-sitter is available:

```lisp
(lem-tree-sitter:tree-sitter-available-p)
;; Should return T
```

### Tree-sitter TOML Not Found

Ensure the TOML grammar is installed:

```lisp
(ts:get-language "toml")
;; Should not error

;; If not found, try loading:
(ts:load-language-from-system "toml")
```

### Mode Not Activating

Verify file extension registration:

```lisp
;; In Lem, file-type should return toml-mode for .toml files
(lem:file-type "test.toml")
```
