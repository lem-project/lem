# Implementation Plan: Multi-Language Living Canvas

**Branch**: `001-multi-language-canvas` | **Date**: 2025-12-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/001-multi-language-canvas/spec.md`

## Summary

Extend Living Canvas to support multiple programming languages by implementing a pluggable language provider architecture. Primary analysis uses tree-sitter for Python and JavaScript/TypeScript, with the existing Common Lisp provider (micros-based) preserved. The unified command interface (`living-canvas-current-file`, etc.) automatically detects language and selects the appropriate provider.

## Technical Context

**Language/Version**: Common Lisp (SBCL), tree-sitter-cl FFI bindings
**Primary Dependencies**: lem/core, tree-sitter-cl, lem-call-graph (existing), lem-tree-sitter
**Storage**: N/A (in-memory graph structures, optional position caching)
**Testing**: Rove framework (`make test`, `(asdf:test-system "lem-tests")`)
**Target Platform**: Linux, macOS (via SDL2 or ncurses frontends)
**Project Type**: Single (Common Lisp extension system)
**Performance Goals**: < 3s graph generation for 100 functions, < 500ms single file parse
**Constraints**: tree-sitter grammars must be system-installed, SBCL-specific CL features
**Scale/Scope**: ~760 new lines of code, 6 new files, modifications to 2 existing files

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Layered Architecture Boundaries (NON-NEGOTIABLE)

| Check | Status | Evidence |
|-------|--------|----------|
| Dependency direction flows downward | ✅ PASS | Providers → call-graph types → lem/core |
| Frontend code uses lem-if:* protocol | ✅ PASS | Living Canvas uses existing HTML buffer (lem-if:make-view) |
| No internal symbol access (lem::) | ✅ PASS | Design uses public APIs only |

### II. Protocol-First Design

| Check | Status | Evidence |
|-------|--------|----------|
| Extensions use existing protocols | ✅ PASS | call-graph-provider protocol already exists |
| Modes via define-major-mode/minor-mode | N/A | No new modes defined |
| Commands via define-command | ✅ PASS | Modifies existing living-canvas commands |

### III. Test-Driven Development (NON-NEGOTIABLE)

| Check | Status | Evidence |
|-------|--------|----------|
| Test system defined | ✅ PASS | Tests in extensions/living-canvas/tests/ |
| Red-Green-Refactor planned | ✅ PASS | Test checklist in quickstart.md |

### IV. Code Quality and Consistency

| Check | Status | Evidence |
|-------|--------|----------|
| Loop keywords with colons | ✅ PASS | Design follows `:for ... :do` pattern |
| Naming conventions | ✅ PASS | kebab-case functions, *earmuffs* globals |
| Docstrings on exports | ✅ PASS | Provider protocol has documentation |

### V. Documentation at the Right Layer

| Check | Status | Evidence |
|-------|--------|----------|
| Code documents How | ✅ PASS | Implementation in quickstart.md |
| Tests document What | ✅ PASS | Test scenarios specified |
| Commits document Why | ✅ PASS | (to be done at implementation) |

**Gate Result**: ✅ PASS - All constitution checks satisfied. No violations requiring justification.

## Project Structure

### Documentation (this feature)

```text
specs/001-multi-language-canvas/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 research decisions
├── data-model.md        # Entity definitions
├── quickstart.md        # Implementation guide
├── contracts/
│   ├── provider-interface.lisp  # Provider API contract
│   └── tree-sitter-queries.md   # Query patterns
└── tasks.md             # (Phase 2 - /speckit.tasks)
```

### Source Code (repository root)

```text
extensions/
├── call-graph/
│   ├── lem-call-graph.asd       # System definition (MODIFY)
│   ├── package.lisp             # Package exports (MODIFY)
│   ├── types.lisp               # Existing - graph-node, graph-edge, call-graph
│   ├── provider.lisp            # Existing - call-graph-provider protocol
│   └── registry.lisp            # NEW - Provider registry
│
├── living-canvas/
│   ├── lem-living-canvas.asd    # System definition (MODIFY)
│   ├── package.lisp             # Package exports (MODIFY)
│   ├── language-detection.lisp  # NEW - Language detection
│   ├── python-provider.lisp     # NEW - Python provider
│   ├── js-provider.lisp         # NEW - JavaScript/TypeScript provider
│   ├── living-canvas.lisp       # Main implementation (MODIFY)
│   ├── buffer.lisp              # Canvas buffer (unchanged)
│   ├── micros-cl-provider.lisp  # CL provider (unchanged)
│   └── tests/
│       ├── main.lisp            # Test runner (MODIFY)
│       ├── python-provider-test.lisp  # NEW
│       └── js-provider-test.lisp      # NEW

tests/
└── living-canvas/               # Integration tests location
```

**Structure Decision**: Single project pattern (Common Lisp extension). All code in `extensions/` following Lem's established layout. New providers added as separate files within existing systems.

## Component Overview

### New Files

| File | Purpose | Est. Lines |
|------|---------|------------|
| `call-graph/registry.lisp` | Provider registration and selection | ~80 |
| `living-canvas/language-detection.lisp` | Detect language from buffer/file | ~50 |
| `living-canvas/python-provider.lisp` | tree-sitter Python analysis | ~200 |
| `living-canvas/js-provider.lisp` | tree-sitter JS/TS analysis | ~180 |
| `living-canvas/tests/python-provider-test.lisp` | Python provider tests | ~100 |
| `living-canvas/tests/js-provider-test.lisp` | JS provider tests | ~100 |

### Modified Files

| File | Changes |
|------|---------|
| `call-graph/lem-call-graph.asd` | Add registry component |
| `call-graph/package.lisp` | Export registry symbols |
| `living-canvas/lem-living-canvas.asd` | Add new components, dependencies |
| `living-canvas/package.lisp` | Export language detection |
| `living-canvas/living-canvas.lisp` | Integrate provider selection in commands |
| `living-canvas/tests/main.lisp` | Include new test files |

## Dependencies

### Required (Build Time)

| Dependency | Version | Notes |
|------------|---------|-------|
| lem/core | current | Editor core |
| tree-sitter-cl | bundled | FFI bindings |
| lem-tree-sitter | bundled | Lem integration |
| alexandria | any | CL utilities |

### Required (Runtime)

| Dependency | Notes |
|------------|-------|
| tree-sitter-python grammar | System package or Nix |
| tree-sitter-javascript grammar | System package or Nix |
| tree-sitter-typescript grammar | System package or Nix |

### Optional

| Dependency | Purpose |
|------------|---------|
| python-lsp-server | Enhanced Python analysis (Phase 2) |
| typescript-language-server | Enhanced TS analysis (Phase 2) |

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| tree-sitter grammar unavailable | Fallback to "no provider" message with installation instructions |
| Performance regression | Cached query compilation, lazy provider initialization |
| Breaking existing CL workflow | Extensive test coverage, backward-compatible command signature |
| Cross-file resolution inaccurate | Mark edges as `:inferred`, improve in Phase 2 with LSP |

## Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Python file analysis | < 3s for 100 functions | Automated benchmark |
| JS/TS file analysis | < 3s for 100 functions | Automated benchmark |
| Test coverage | > 80% new code | Rove coverage |
| Regression | 0 failing existing tests | CI pipeline |

## Phase 2 Planning (Future)

After initial implementation:

1. **LSP Integration**: Use CallHierarchy protocol for cross-file analysis
2. **Execution Tracing**: Language-specific runtime tracing
3. **Additional Languages**: Rust, Go, Ruby via tree-sitter
4. **Performance**: Incremental graph updates on buffer changes

---

**Implementation Status**: Ready for `/speckit.tasks` to generate task breakdown
