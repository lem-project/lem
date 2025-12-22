# Tasks: Multi-Language Living Canvas

**Input**: Design documents from `/specs/001-multi-language-canvas/`
**Prerequisites**: plan.md ✅, spec.md ✅, research.md ✅, data-model.md ✅, contracts/ ✅, quickstart.md ✅

**Tests**: Included per TDD requirement from constitution (NON-NEGOTIABLE)

**Organization**: Tasks grouped by user story for independent implementation and testing

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions (Common Lisp Extension)

- **Extensions**: `extensions/{module}/` at repository root
- **Tests**: `extensions/{module}/tests/` within each module
- **System definitions**: `extensions/{module}/lem-{module}.asd`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and verify environment

- [ ] T001 Verify tree-sitter-cl is loadable in Lem REPL
- [ ] T002 [P] Verify tree-sitter-python grammar is available via `(ts:load-language-from-system "python")`
- [ ] T003 [P] Verify tree-sitter-javascript grammar is available via `(ts:load-language-from-system "javascript")`
- [ ] T004 [P] Verify tree-sitter-typescript grammar is available via `(ts:load-language-from-system "typescript")`
- [ ] T005 Document grammar installation instructions in specs/001-multi-language-canvas/quickstart.md if missing

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core provider registry infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Tests for Foundational

- [ ] T006 [P] Create test file for provider registry at extensions/call-graph/tests/registry-test.lisp
- [ ] T007 [P] Write failing test for `register-provider` in extensions/call-graph/tests/registry-test.lisp
- [ ] T008 [P] Write failing test for `find-provider` with priority ordering in extensions/call-graph/tests/registry-test.lisp
- [ ] T009 [P] Write failing test for `list-providers` in extensions/call-graph/tests/registry-test.lisp

### Implementation for Foundational

- [ ] T010 Add `(:file "registry")` component to extensions/call-graph/lem-call-graph.asd
- [ ] T011 Create provider-registry class in extensions/call-graph/registry.lisp with `providers` and `language-map` slots
- [ ] T012 Implement `register-provider` function in extensions/call-graph/registry.lisp
- [ ] T013 Implement `unregister-provider` function in extensions/call-graph/registry.lisp
- [ ] T014 Implement `find-provider` function with priority sorting in extensions/call-graph/registry.lisp
- [ ] T015 Implement `list-providers` function in extensions/call-graph/registry.lisp
- [ ] T016 Define `*provider-registry*` global instance in extensions/call-graph/registry.lisp
- [ ] T017 Export registry symbols in extensions/call-graph/package.lisp
- [ ] T018 Verify all registry tests pass with `(asdf:test-system "lem-call-graph")`

**Checkpoint**: Provider registry ready - user story implementation can now begin

---

## Phase 3: User Story 1 - Unified UX with Language Detection (Priority: P1) 🎯 MVP

**Goal**: Developers use same `living-canvas-current-file` command for any language; system auto-detects and selects appropriate provider

**Independent Test**: Open Python/JS/Lisp files, run `living-canvas-current-file`, verify each uses correct provider or shows helpful error for unsupported languages

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T019 [P] [US1] Create test file for language detection at extensions/living-canvas/tests/language-detection-test.lisp
- [ ] T020 [P] [US1] Write failing test for `detect-language` with Python buffer in extensions/living-canvas/tests/language-detection-test.lisp
- [ ] T021 [P] [US1] Write failing test for `detect-language` with JavaScript file extension in extensions/living-canvas/tests/language-detection-test.lisp
- [ ] T022 [P] [US1] Write failing test for `detect-language` with Common Lisp major mode in extensions/living-canvas/tests/language-detection-test.lisp
- [ ] T023 [P] [US1] Write failing test for unsupported language error message in extensions/living-canvas/tests/language-detection-test.lisp

### Implementation for User Story 1

- [ ] T024 [US1] Add `(:file "language-detection")` component to extensions/living-canvas/lem-living-canvas.asd (before living-canvas.lisp)
- [ ] T025 [US1] Create defpackage `:lem-living-canvas/language` in extensions/living-canvas/language-detection.lisp
- [ ] T026 [US1] Define `*mode-language-map*` alist mapping major modes to language keywords in extensions/living-canvas/language-detection.lisp
- [ ] T027 [US1] Define `*extension-language-map*` alist mapping file extensions to language keywords in extensions/living-canvas/language-detection.lisp
- [ ] T028 [US1] Implement `language-for-buffer` function using major mode lookup in extensions/living-canvas/language-detection.lisp
- [ ] T029 [US1] Implement `language-for-file` function using pathname-type lookup in extensions/living-canvas/language-detection.lisp
- [ ] T030 [US1] Implement `detect-language` function with fallback chain (mode → extension) in extensions/living-canvas/language-detection.lisp
- [ ] T031 [US1] Export language detection symbols in extensions/living-canvas/package.lisp
- [ ] T032 [US1] Modify `living-canvas-current-file` command to use `detect-language` and `find-provider` in extensions/living-canvas/living-canvas.lisp
- [ ] T033 [US1] Add error handling for unsupported languages with editor-error message in extensions/living-canvas/living-canvas.lisp
- [ ] T034 [US1] Add backward compatibility fallback for Common Lisp to use existing micros provider in extensions/living-canvas/living-canvas.lisp
- [ ] T035 [US1] Verify all US1 tests pass with `(asdf:test-system "lem-living-canvas")`

**Checkpoint**: Language detection works, commands dispatch to correct providers, unsupported languages show helpful messages

---

## Phase 4: User Story 2 - Python Support (Priority: P1)

**Goal**: Python developers can view function call graphs for Python files with source jump functionality

**Independent Test**: Open a Python file with functions, run `living-canvas-current-file`, verify function nodes appear with call edges, double-click jumps to source

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T036 [P] [US2] Create test file for Python provider at extensions/living-canvas/tests/python-provider-test.lisp
- [ ] T037 [P] [US2] Write failing test for `provider-supports-p` with Python buffer in extensions/living-canvas/tests/python-provider-test.lisp
- [ ] T038 [P] [US2] Write failing test for `provider-supports-p` with .py pathname in extensions/living-canvas/tests/python-provider-test.lisp
- [ ] T039 [P] [US2] Write failing test for `provider-analyze` extracting function definitions in extensions/living-canvas/tests/python-provider-test.lisp
- [ ] T040 [P] [US2] Write failing test for `provider-analyze` extracting call edges in extensions/living-canvas/tests/python-provider-test.lisp
- [ ] T041 [P] [US2] Write failing test for source-location population in graph nodes in extensions/living-canvas/tests/python-provider-test.lisp

### Implementation for User Story 2

- [ ] T042 [US2] Add `"lem-tree-sitter"` to :depends-on in extensions/living-canvas/lem-living-canvas.asd
- [ ] T043 [US2] Add `(:file "python-provider")` component to extensions/living-canvas/lem-living-canvas.asd (after language-detection)
- [ ] T044 [US2] Create defpackage `:lem-living-canvas/python` in extensions/living-canvas/python-provider.lisp
- [ ] T045 [US2] Define `tree-sitter-python-provider` class with `definition-query` and `call-query` slots in extensions/living-canvas/python-provider.lisp
- [ ] T046 [US2] Implement `initialize-instance :after` to load Python grammar and compile queries in extensions/living-canvas/python-provider.lisp
- [ ] T047 [US2] Implement `provider-name` returning `:tree-sitter-python` in extensions/living-canvas/python-provider.lisp
- [ ] T048 [US2] Implement `provider-priority` returning 5 in extensions/living-canvas/python-provider.lisp
- [ ] T049 [US2] Implement `provider-languages` returning `'(:python)` in extensions/living-canvas/python-provider.lisp
- [ ] T050 [US2] Implement `provider-supports-p` checking buffer mode or .py extension in extensions/living-canvas/python-provider.lisp
- [ ] T051 [US2] Implement helper `extract-text-from-source` for buffer/pathname/string in extensions/living-canvas/python-provider.lisp
- [ ] T052 [US2] Implement `extract-definitions` using tree-sitter queries in extensions/living-canvas/python-provider.lisp
- [ ] T053 [US2] Implement `extract-calls` using tree-sitter queries in extensions/living-canvas/python-provider.lisp
- [ ] T054 [US2] Implement `find-enclosing-function` helper for call context in extensions/living-canvas/python-provider.lisp
- [ ] T055 [US2] Implement `provider-analyze` combining definitions and calls into call-graph in extensions/living-canvas/python-provider.lisp
- [ ] T056 [US2] Add auto-registration of Python provider on load in extensions/living-canvas/python-provider.lisp
- [ ] T057 [US2] Export Python provider symbols in extensions/living-canvas/package.lisp
- [ ] T058 [US2] Verify all US2 tests pass with `(asdf:test-system "lem-living-canvas")`
- [ ] T059 [US2] Manual test: Open Python file, run living-canvas-current-file, verify graph displays

**Checkpoint**: Python files show call graphs with function nodes, edges, and source jump functionality

---

## Phase 5: User Story 3 - JavaScript/TypeScript Support (Priority: P2)

**Goal**: JS/TS developers can view function call graphs for JavaScript and TypeScript files

**Independent Test**: Open a TypeScript file with functions, run `living-canvas-current-file`, verify function nodes appear with call edges

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T060 [P] [US3] Create test file for JS provider at extensions/living-canvas/tests/js-provider-test.lisp
- [ ] T061 [P] [US3] Write failing test for `provider-supports-p` with JavaScript buffer in extensions/living-canvas/tests/js-provider-test.lisp
- [ ] T062 [P] [US3] Write failing test for `provider-supports-p` with .ts pathname in extensions/living-canvas/tests/js-provider-test.lisp
- [ ] T063 [P] [US3] Write failing test for `provider-analyze` extracting function declarations in extensions/living-canvas/tests/js-provider-test.lisp
- [ ] T064 [P] [US3] Write failing test for `provider-analyze` extracting arrow functions in extensions/living-canvas/tests/js-provider-test.lisp
- [ ] T065 [P] [US3] Write failing test for `provider-analyze` extracting method definitions in extensions/living-canvas/tests/js-provider-test.lisp

### Implementation for User Story 3

- [ ] T066 [US3] Add `(:file "js-provider")` component to extensions/living-canvas/lem-living-canvas.asd (after python-provider)
- [ ] T067 [US3] Create defpackage `:lem-living-canvas/javascript` in extensions/living-canvas/js-provider.lisp
- [ ] T068 [US3] Define `tree-sitter-js-provider` class with `definition-query` and `call-query` slots in extensions/living-canvas/js-provider.lisp
- [ ] T069 [US3] Implement `initialize-instance :after` to load JavaScript grammar and compile queries in extensions/living-canvas/js-provider.lisp
- [ ] T070 [US3] Implement `provider-name` returning `:tree-sitter-javascript` in extensions/living-canvas/js-provider.lisp
- [ ] T071 [US3] Implement `provider-priority` returning 5 in extensions/living-canvas/js-provider.lisp
- [ ] T072 [US3] Implement `provider-languages` returning `'(:javascript :typescript)` in extensions/living-canvas/js-provider.lisp
- [ ] T073 [US3] Implement `provider-supports-p` checking buffer mode or .js/.ts/.jsx/.tsx extension in extensions/living-canvas/js-provider.lisp
- [ ] T074 [US3] Implement tree-sitter queries for function declarations, arrow functions, methods in extensions/living-canvas/js-provider.lisp
- [ ] T075 [US3] Implement tree-sitter queries for call expressions and method calls in extensions/living-canvas/js-provider.lisp
- [ ] T076 [US3] Implement `provider-analyze` for JavaScript/TypeScript files in extensions/living-canvas/js-provider.lisp
- [ ] T077 [US3] Add auto-registration of JS provider on load in extensions/living-canvas/js-provider.lisp
- [ ] T078 [US3] Export JavaScript provider symbols in extensions/living-canvas/package.lisp
- [ ] T079 [US3] Add TypeScript extension mapping to language-detection in extensions/living-canvas/language-detection.lisp
- [ ] T080 [US3] Verify all US3 tests pass with `(asdf:test-system "lem-living-canvas")`
- [ ] T081 [US3] Manual test: Open TypeScript file, run living-canvas-current-file, verify graph displays

**Checkpoint**: JavaScript and TypeScript files show call graphs with function nodes, edges, and source jump functionality

---

## Phase 6: User Story 4 - Extensibility for New Providers (Priority: P3)

**Goal**: Extension developers can add new language support by implementing the provider protocol

**Independent Test**: Create a minimal test provider, register it, verify it's selected for its language

### Tests for User Story 4

- [ ] T082 [P] [US4] Write test creating minimal provider subclass in extensions/call-graph/tests/registry-test.lisp
- [ ] T083 [P] [US4] Write test verifying custom provider is selected over default in extensions/call-graph/tests/registry-test.lisp

### Implementation for User Story 4

- [ ] T084 [US4] Add docstrings to all provider protocol methods in extensions/call-graph/provider.lisp
- [ ] T085 [US4] Add `define-call-graph-provider` convenience macro to extensions/call-graph/provider.lisp (per contracts/provider-interface.lisp)
- [ ] T086 [US4] Add `provider-error` condition class to extensions/call-graph/provider.lisp
- [ ] T087 [US4] Add `provider-unavailable` condition class to extensions/call-graph/provider.lisp
- [ ] T088 [US4] Export all provider protocol symbols in extensions/call-graph/package.lisp
- [ ] T089 [US4] Verify all US4 tests pass with `(asdf:test-system "lem-call-graph")`

**Checkpoint**: Provider protocol is fully documented and usable by extension developers

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T090 [P] Add docstrings to all exported functions in extensions/living-canvas/*.lisp
- [ ] T091 [P] Add docstrings to all exported functions in extensions/call-graph/*.lisp
- [ ] T092 Ensure all loop macros use `:for ... :do` syntax per contract.yml in all new files
- [ ] T093 Verify naming conventions (kebab-case, *earmuffs*) in all new files
- [ ] T094 Update extensions/living-canvas/tests/main.lisp to include new test files
- [ ] T095 Run full test suite with `make test` and fix any failures
- [ ] T096 [P] Update specs/001-multi-language-canvas/quickstart.md with actual implementation details
- [ ] T097 Manual regression test: Verify Common Lisp living-canvas still works unchanged
- [ ] T098 Performance test: Verify < 3s analysis time for 100-function file

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Story 1 (Phase 3)**: Depends on Foundational completion
- **User Story 2 (Phase 4)**: Depends on Foundational completion, benefits from US1 but independently testable
- **User Story 3 (Phase 5)**: Depends on Foundational completion, benefits from US1 but independently testable
- **User Story 4 (Phase 6)**: Depends on Foundational completion, provider protocol refinement
- **Polish (Phase 7)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Provides language detection and command dispatch - other stories use this but can be tested independently
- **User Story 2 (P1)**: Provides Python provider - independent implementation
- **User Story 3 (P2)**: Provides JS/TS provider - independent implementation
- **User Story 4 (P3)**: Refines provider protocol - can enhance existing providers

### Within Each User Story

- Tests MUST be written and FAIL before implementation
- Package definition before implementation functions
- Helper functions before main `provider-analyze`
- Auto-registration at end of provider file
- Export symbols after implementation complete

### Parallel Opportunities

**Phase 1 (Setup)**:
```
T002, T003, T004 can run in parallel (different grammars)
```

**Phase 2 (Foundational)**:
```
T006, T007, T008, T009 can run in parallel (test files)
```

**Phase 3 (US1)**:
```
T019, T020, T021, T022, T023 can run in parallel (test cases)
```

**Phase 4 (US2)**:
```
T036, T037, T038, T039, T040, T041 can run in parallel (test cases)
```

**Phase 5 (US3)**:
```
T060, T061, T062, T063, T064, T065 can run in parallel (test cases)
```

**Cross-Story Parallelism**:
Once Foundational is complete, US2 and US3 can be developed in parallel by different developers since they create different provider files.

---

## Parallel Example: User Story 2 Tests

```bash
# Launch all tests for User Story 2 together:
Task: "Write failing test for provider-supports-p with Python buffer"
Task: "Write failing test for provider-supports-p with .py pathname"
Task: "Write failing test for provider-analyze extracting function definitions"
Task: "Write failing test for provider-analyze extracting call edges"
Task: "Write failing test for source-location population"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2)

1. Complete Phase 1: Setup (verify environment)
2. Complete Phase 2: Foundational (provider registry)
3. Complete Phase 3: User Story 1 (language detection)
4. Complete Phase 4: User Story 2 (Python support)
5. **STOP and VALIDATE**: Test Python workflow end-to-end
6. Can demo/release with Python support only

### Incremental Delivery

1. Setup + Foundational → Core infrastructure ready
2. Add User Story 1 → Test language detection → Can show unified commands
3. Add User Story 2 → Test Python → **MVP Ready!** Python users can use Living Canvas
4. Add User Story 3 → Test JS/TS → Web developers can use Living Canvas
5. Add User Story 4 → Refine protocol → Community can add more languages
6. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 + User Story 2 (tightly coupled)
   - Developer B: User Story 3 (independent JS/TS implementation)
3. Developer A completes US1+US2, Developer B completes US3
4. Together: User Story 4 (protocol refinement)
5. Together: Phase 7 (polish)

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Total Tasks | 98 |
| Setup Tasks | 5 |
| Foundational Tasks | 13 |
| User Story 1 Tasks | 17 |
| User Story 2 Tasks | 24 |
| User Story 3 Tasks | 22 |
| User Story 4 Tasks | 8 |
| Polish Tasks | 9 |
| Parallelizable Tasks | 36 |

### MVP Scope

For MVP (Python support only), complete:
- Phase 1: 5 tasks
- Phase 2: 13 tasks
- Phase 3: 17 tasks
- Phase 4: 24 tasks
- **Total MVP**: 59 tasks

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story is independently completable and testable
- TDD: Write failing tests before implementation (constitution requirement)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Common Lisp style: Use `:for ... :do` in loops, kebab-case names, docstrings on exports
