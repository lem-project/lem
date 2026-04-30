# Tasks: TOML Mode Extension

**Input**: Design documents from `/specs/001-toml-mode/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: Test tasks are included as the spec mentions TDD approach in Constitution Check.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Extension location**: `extensions/toml-mode/`
- Tests are in `extensions/toml-mode/tests/`
- Tree-sitter queries in `extensions/toml-mode/tree-sitter/`

---

## Phase 1: Setup (Project Structure)

**Purpose**: Create extension directory and ASDF system definition

- [x] T001 Create extension directory structure at extensions/toml-mode/
- [x] T002 Create ASDF system definition in extensions/toml-mode/lem-toml-mode.asd
- [x] T003 [P] Create tree-sitter query directory at extensions/toml-mode/tree-sitter/
- [x] T004 [P] Create tests directory at extensions/toml-mode/tests/

---

## Phase 2: Foundational (Core Mode Infrastructure)

**Purpose**: Create the base mode file with package definition and syntax table

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T005 Create package definition with exports in extensions/toml-mode/toml-mode.lisp
- [x] T006 Define `*toml-syntax-table*` with symbol-chars, string-quote-chars, paren-pairs in extensions/toml-mode/toml-mode.lisp
- [x] T007 Create fallback tmlanguage patterns for non-tree-sitter fallback in extensions/toml-mode/toml-mode.lisp
- [x] T008 Set syntax parser on syntax table in extensions/toml-mode/toml-mode.lisp

**Checkpoint**: Foundation ready - mode file has package, exports, and syntax table

---

## Phase 3: User Story 1 - Syntax Highlighting (Priority: P1) 🎯 MVP

**Goal**: Open TOML files with proper syntax highlighting for all TOML elements

**Independent Test**: Open any `.toml` file, verify different syntax elements are colored distinctly

### Tests for User Story 1

- [x] T009 [P] [US1] Create test file at extensions/toml-mode/tests/main.lisp with package definition
- [x] T010 [P] [US1] Write test: mode activates for `.toml` files in extensions/toml-mode/tests/main.lisp
- [x] T011 [P] [US1] Write test: syntax highlighting is enabled in extensions/toml-mode/tests/main.lisp
- [x] T012 [P] [US1] Write test: mode inherits from language-mode in extensions/toml-mode/tests/main.lisp

### Implementation for User Story 1

- [x] T013 [US1] Create highlights.scm with key captures (@property for bare_key, @string for quoted_key) in extensions/toml-mode/tree-sitter/highlights.scm
- [x] T014 [US1] Add value captures (@string, @number, @constant.builtin for boolean) to extensions/toml-mode/tree-sitter/highlights.scm
- [x] T015 [US1] Add table header captures (@type for table and table_array_element keys) to extensions/toml-mode/tree-sitter/highlights.scm
- [x] T016 [US1] Add comment capture (@comment) to extensions/toml-mode/tree-sitter/highlights.scm
- [x] T017 [US1] Add datetime captures (@string.special for all date/time types) to extensions/toml-mode/tree-sitter/highlights.scm
- [x] T018 [US1] Add punctuation captures (@punctuation.bracket, @punctuation.delimiter, @operator) to extensions/toml-mode/tree-sitter/highlights.scm
- [x] T019 [US1] Define `tree-sitter-query-path` function returning path to highlights.scm in extensions/toml-mode/toml-mode.lisp
- [x] T020 [US1] Define `toml-mode` with `define-major-mode` inheriting from `language-mode` in extensions/toml-mode/toml-mode.lisp
- [x] T021 [US1] Enable tree-sitter in mode body with `enable-tree-sitter-for-mode` in extensions/toml-mode/toml-mode.lisp
- [x] T022 [US1] Set buffer-local variables (enable-syntax-highlight, indent-tabs-mode, tab-width) in extensions/toml-mode/toml-mode.lisp
- [x] T023 [US1] Register file type with `define-file-type` for ".toml" extension in extensions/toml-mode/toml-mode.lisp

**Checkpoint**: User Story 1 complete - TOML files open with full syntax highlighting

---

## Phase 4: User Story 2 - Line Comment Support (Priority: P2)

**Goal**: Use standard comment commands to comment/uncomment lines with `#`

**Independent Test**: Select lines, invoke comment command, verify `#` is inserted/removed

### Tests for User Story 2

- [x] T024 [P] [US2] Write test: line-comment variable is "#" in extensions/toml-mode/tests/main.lisp

### Implementation for User Story 2

- [x] T025 [US2] Set `line-comment` buffer-local variable to "#" in mode definition in extensions/toml-mode/toml-mode.lisp

**Checkpoint**: User Story 2 complete - Comment commands work with `#` character

---

## Phase 5: User Story 3 - Visual Consistency (Priority: P3)

**Goal**: Consistent visual appearance with YAML and JSON modes

**Independent Test**: Open TOML, YAML, JSON files side by side, compare highlighting colors

### Implementation for User Story 3

- [x] T026 [US3] Verify highlights.scm uses same capture names as yaml-mode/json-mode in extensions/toml-mode/tree-sitter/highlights.scm
- [x] T027 [US3] Ensure @string, @number, @comment captures match other modes in extensions/toml-mode/tree-sitter/highlights.scm

**Checkpoint**: User Story 3 complete - Visual consistency achieved with YAML/JSON modes

---

## Phase 6: Polish & Verification

**Purpose**: Final validation and integration

- [x] T028 [P] Add test system definition to extensions/toml-mode/lem-toml-mode.asd
- [x] T029 [P] Run all tests with `(asdf:test-system "lem-toml-mode/tests")`
- [x] T030 Verify mode loads correctly with `(ql:quickload :lem-toml-mode)`
- [x] T031 Manual verification: open sample TOML file (e.g., Cargo.toml) and check highlighting
- [x] T032 Verify quickstart.md scenarios work as documented

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
- **Polish (Phase 6)**: Depends on all user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Foundational (Phase 2) - Independent of US1
- **User Story 3 (P3)**: Can start after US1 highlights.scm is created - Verifies consistency

### Within Each User Story

- Tests written first (TDD approach per Constitution)
- Mode infrastructure before tree-sitter integration
- Core highlighting before polish

### Parallel Opportunities

- T003, T004 can run in parallel (different directories)
- T009, T010, T011, T012 can run in parallel (same file but independent tests)
- T013-T018 build sequentially on highlights.scm
- T028, T029 can run in parallel

---

## Parallel Example: User Story 1 Tests

```bash
# Launch all tests for User Story 1 together:
Task: "Create test file at extensions/toml-mode/tests/main.lisp"
Task: "Write test: mode activates for .toml files"
Task: "Write test: syntax highlighting is enabled"
Task: "Write test: mode inherits from language-mode"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T004)
2. Complete Phase 2: Foundational (T005-T008)
3. Complete Phase 3: User Story 1 (T009-T023)
4. **STOP and VALIDATE**: Test by opening a `.toml` file
5. Mode is usable for basic editing

### Incremental Delivery

1. Setup + Foundational → Extension structure ready
2. Add User Story 1 → Syntax highlighting works → MVP usable!
3. Add User Story 2 → Comment commands work → Enhanced editing
4. Add User Story 3 → Visual consistency verified → Production ready
5. Polish phase → Tests pass, documentation verified

### Single Developer Strategy

Execute phases in order:
1. Phase 1 (Setup): ~5 minutes
2. Phase 2 (Foundational): ~15 minutes
3. Phase 3 (US1 - Highlighting): ~30 minutes → **MVP checkpoint**
4. Phase 4 (US2 - Comments): ~5 minutes
5. Phase 5 (US3 - Consistency): ~10 minutes
6. Phase 6 (Polish): ~10 minutes

**Total estimated effort**: ~75 minutes for complete implementation

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Follow yaml-mode as reference implementation
- Verify tests fail before implementing (TDD)
- Commit after each phase completion
