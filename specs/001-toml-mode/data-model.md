# Data Model: TOML Mode Extension

**Date**: 2025-12-30
**Feature**: 001-toml-mode

## Overview

TOML mode is an editor extension with minimal data model requirements. It operates on existing Lem abstractions (buffers, syntax tables, modes) without introducing new persistent entities.

## Entities

### 1. TOML Mode

**Type**: Major Mode (runtime object, not persisted)

| Attribute | Type | Description |
|-----------|------|-------------|
| name | string | "Toml" |
| keymap | keymap | Mode-specific key bindings (inherited from language-mode) |
| syntax-table | syntax-table | TOML syntax configuration |
| mode-hook | hook | `*toml-mode-hook*` |

**Relationships**:
- Inherits from `language-mode`
- Associated with `*toml-syntax-table*`
- Registered for `.toml` file extension

### 2. TOML Syntax Table

**Type**: Syntax Table (runtime object)

| Attribute | Type | Description |
|-----------|------|-------------|
| symbol-chars | list | Characters valid in symbols (e.g., `-`, `_`) |
| string-quote-chars | list | `"` and `'` |
| paren-pairs | alist | `[`/`]`, `{`/`}` |
| parser | treesitter-parser | Tree-sitter parser instance |

**State**: Configured once at mode load, shared across all TOML buffers.

### 3. Tree-sitter Parser Instance

**Type**: Runtime object (per-buffer, managed by lem-tree-sitter)

| Attribute | Type | Description |
|-----------|------|-------------|
| language-name | string | "toml" |
| highlight-query | query | Compiled highlights.scm |
| tree | tree | Current parse tree |
| cached-tick | integer | Buffer modification tick for cache |

**Lifecycle**:
- Created when first TOML buffer uses tree-sitter
- Tree updated incrementally on buffer changes
- Garbage collected when buffer closed

## Configuration

### Buffer-Local Variables

| Variable | Value | Description |
|----------|-------|-------------|
| `enable-syntax-highlight` | `t` | Enable highlighting |
| `indent-tabs-mode` | `nil` | Use spaces |
| `tab-width` | 2 | Standard TOML indentation |
| `line-comment` | `"#"` | Comment character |

### File Type Registration

| Extension | Mode |
|-----------|------|
| `.toml` | `toml-mode` |

## No Persistent Storage

This extension does not persist any data. All state is:
- Runtime (syntax tables, parsers)
- Buffer-local (indentation settings)
- Transient (parse trees for highlighting)
