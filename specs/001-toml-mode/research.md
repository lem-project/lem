# Research: TOML Mode Extension

**Date**: 2025-12-30
**Feature**: 001-toml-mode

## Research Questions

### 1. Tree-sitter TOML Grammar Availability

**Decision**: Use the official `tree-sitter-toml` grammar from the tree-sitter organization.

**Rationale**:
- Official grammar maintained by tree-sitter team at [github.com/tree-sitter/tree-sitter-toml](https://github.com/tree-sitter/tree-sitter-toml)
- Implements TOML Spec v1.0.0-rc.1 (compatible with v1.0.0)
- Already used by major editors (Helix, Neovim, Emacs tree-sitter modes)
- Lem's `lem-tree-sitter` uses `ts:load-language-from-system` which can load this grammar

**Alternatives Considered**:
- Custom TextMate grammar (tmlanguage): Rejected - Less accurate, already moving to tree-sitter
- Other TOML parsers: None with tree-sitter integration needed

### 2. TOML Grammar Node Types

**Decision**: Map the following node types to Lem syntax attributes:

| Node Type | Lem Attribute | Notes |
|-----------|---------------|-------|
| `bare_key` | `syntax-variable-attribute` | Keys in key-value pairs |
| `quoted_key` | `syntax-string-attribute` | Quoted key names |
| `string` | `syntax-string-attribute` | All string values |
| `integer` | `syntax-constant-attribute` | Integer literals |
| `float` | `syntax-constant-attribute` | Float literals |
| `boolean` | `syntax-keyword-attribute` | true/false |
| `comment` | `syntax-comment-attribute` | # comments |
| `table` | `syntax-type-attribute` | [table] headers |
| `table_array_element` | `syntax-type-attribute` | [[array]] headers |
| `local_date`, `local_time`, `local_date_time`, `offset_date_time` | `syntax-constant-attribute` | Date/time values |
| `escape_sequence` | `syntax-string-attribute` | Escape sequences in strings |

**Rationale**:
- Follows the attribute mapping used by yaml-mode and json-mode for consistency
- Uses Lem's built-in syntax attribute names from `lem/language-mode`

### 3. highlights.scm Query Structure

**Decision**: Create a highlights.scm file following the official tree-sitter-toml query structure with Lem-compatible capture names.

**Rationale**:
- Lem's `lem-tree-sitter/highlight.lisp` maps capture names to attributes
- The official highlights.scm provides a complete base to adapt

**Key Captures**:
```scheme
; Keys
(bare_key) @property
(quoted_key) @string

; Values
(boolean) @constant.builtin
(comment) @comment
(string) @string
(integer) @number
(float) @number
(offset_date_time) @string.special
(local_date_time) @string.special
(local_date) @string.special
(local_time) @string.special

; Tables
(table (bare_key) @type)
(table (quoted_key) @type)
(table_array_element (bare_key) @type)
(table_array_element (quoted_key) @type)

; Punctuation
"." @punctuation.delimiter
"," @punctuation.delimiter
"=" @operator
"[" @punctuation.bracket
"]" @punctuation.bracket
"[[" @punctuation.bracket
"]]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
```

### 4. Existing Mode Pattern Analysis

**Decision**: Follow yaml-mode as the primary template.

**Rationale**:
- yaml-mode is the simplest tree-sitter enabled mode (~70 LOC)
- json-mode has additional complexity (js-mode dependency for formatting/indent)
- Both use the same `enable-tree-sitter-for-mode` function

**Pattern from yaml-mode**:
1. Define package with `:use :cl :lem :lem/language-mode :lem/language-mode-tools`
2. Create fallback tmlanguage patterns
3. Create syntax table with appropriate chars
4. Use `enable-tree-sitter-for-mode` in mode definition
5. Set buffer-local variables: `enable-syntax-highlight`, `indent-tabs-mode`, `tab-width`, `line-comment`
6. Register file extension via `define-file-type`

### 5. Testing Strategy

**Decision**: Create basic tests using Rove framework.

**Rationale**:
- Follows Lem's testing conventions
- Tests mode activation and basic highlighting

**Test Cases**:
1. Mode activates for `.toml` files
2. Line comment character is `#`
3. Syntax highlighting is enabled
4. Mode inherits from `language-mode`

## Implementation Notes

### File Structure

```
extensions/toml-mode/
├── lem-toml-mode.asd
├── toml-mode.lisp
├── tree-sitter/
│   └── highlights.scm
└── tests/
    └── main.lisp
```

### Dependencies

- `lem/core` - Core editor functionality
- `lem-tree-sitter` - Tree-sitter integration
- `lem/language-mode` - Base mode functionality (via `:use`)
- `lem/language-mode-tools` - TM language helpers (via `:use`)

### Registration

The mode must be registered in Lem's extension loading. Options:
1. Add to `extensions/extensions.lisp` (if it exists)
2. Rely on ASDF system autoload
3. User loads via `(ql:quickload :lem-toml-mode)`

**Decision**: Follow existing pattern - define system, user loads as needed. Document in README.

## References

- [tree-sitter-toml](https://github.com/tree-sitter/tree-sitter-toml) - Official TOML grammar
- [Tree-sitter Syntax Highlighting](https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html) - Highlighting documentation
- `extensions/yaml-mode/` - Reference implementation in Lem
- `extensions/json-mode/` - Additional reference
