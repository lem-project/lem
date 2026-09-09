# Implementation Plan: TOML Mode Extension

**Branch**: `001-toml-mode` | **Date**: 2025-12-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-toml-mode/spec.md`

## Summary

Add a TOML language mode to Lem editor that provides syntax highlighting for TOML configuration files using tree-sitter integration, following the established pattern of yaml-mode and json-mode. The mode will automatically activate for `.toml` files and support line commenting with `#`.

## Technical Context

**Language/Version**: Common Lisp (SBCL)
**Primary Dependencies**: `lem/core`, `lem-tree-sitter`
**Storage**: N/A (editor mode, no data persistence)
**Testing**: Rove framework via `(asdf:test-system "lem-toml-mode/tests")`
**Target Platform**: All Lem-supported platforms (Linux, macOS, Windows via SDL2/ncurses)
**Project Type**: Single extension module
**Performance Goals**: Mode activation < 100ms, syntax highlighting on-demand via tree-sitter incremental parsing
**Constraints**: Must follow existing mode patterns (yaml-mode, json-mode), tree-sitter TOML grammar must be available
**Scale/Scope**: Single extension (~100-150 LOC), one highlights.scm query file

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Layered Architecture | PASS | Extension depends only on `lem/core` and `lem-tree-sitter` |
| II. Protocol-First Design | PASS | Uses `define-major-mode` macro, inherits from `language-mode` |
| III. Test-Driven Development | PASS | Will create `lem-toml-mode/tests` system |
| IV. Code Quality | PASS | Will follow `contract.yml` rules (loop keywords, naming) |
| V. Documentation | PASS | Will include docstrings for exported symbols |

**Extension Guidelines Check**:
- [x] Standalone system named `lem-toml-mode.asd`
- [x] Depends on `lem/core`, minimal other dependencies
- [x] Mode-specific keymap (not global)
- [x] Package name matches filename pattern

## Project Structure

### Documentation (this feature)

```text
specs/001-toml-mode/
├── spec.md              # Feature specification
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output (minimal for this feature)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
extensions/toml-mode/
├── lem-toml-mode.asd           # ASDF system definition
├── toml-mode.lisp              # Main mode implementation
├── tree-sitter/
│   └── highlights.scm          # Tree-sitter highlight queries
└── tests/
    └── main.lisp               # Rove tests
```

**Structure Decision**: Following the established pattern of `yaml-mode` and `json-mode` which are peer directories under `extensions/`. The tree-sitter highlight query file follows the same convention used by these modes.

## Complexity Tracking

> No violations requiring justification. Implementation follows standard extension patterns.

| Aspect | Complexity Level | Justification |
|--------|------------------|---------------|
| Dependencies | Minimal | Only `lem/core` and `lem-tree-sitter` |
| Code Size | Small (~100-150 LOC) | Similar to yaml-mode |
| New Patterns | None | Follows existing mode patterns |

## Phase 0: Research (Complete)

See [research.md](./research.md) for:
- Tree-sitter TOML grammar selection
- Node type to attribute mapping
- highlights.scm query structure
- Existing mode pattern analysis
- Testing strategy

## Phase 1: Design (Complete)

See [data-model.md](./data-model.md) for:
- TOML Mode entity definition
- Syntax table configuration
- Buffer-local variables
- File type registration

See [quickstart.md](./quickstart.md) for:
- Installation instructions
- Usage guide
- Troubleshooting

## Constitution Check (Post-Design)

| Principle | Status | Verification |
|-----------|--------|--------------|
| I. Layered Architecture | PASS | Depends only on `lem/core`, `lem-tree-sitter` |
| II. Protocol-First Design | PASS | Uses `define-major-mode`, inherits `language-mode` |
| III. Test-Driven Development | PASS | Test system defined in research |
| IV. Code Quality | PASS | Pattern matches yaml-mode, json-mode |
| V. Documentation | PASS | Docstrings planned for exports |

## Next Steps

Run `/speckit.tasks` to generate implementation tasks.
