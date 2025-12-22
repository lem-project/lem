<!--
================================================================================
SYNC IMPACT REPORT
================================================================================
Version change: (template) → 1.0.0
Bump rationale: MAJOR - Initial constitution creation from template

Modified principles:
  - [PRINCIPLE_1_NAME] → I. Layered Architecture Boundaries
  - [PRINCIPLE_2_NAME] → II. Protocol-First Design
  - [PRINCIPLE_3_NAME] → III. Test-Driven Development
  - [PRINCIPLE_4_NAME] → IV. Code Quality and Consistency
  - [PRINCIPLE_5_NAME] → V. Documentation at the Right Layer

Added sections:
  - Architectural Constraints
  - Extension Development Guidelines
  - Governance (filled)

Removed sections: None (template placeholders replaced)

Templates status:
  - .specify/templates/plan-template.md ✅ (already has Constitution Check section)
  - .specify/templates/spec-template.md ✅ (no constitution-specific content needed)
  - .specify/templates/tasks-template.md ✅ (no constitution-specific content needed)

Follow-up TODOs: None
================================================================================
-->

# Lem Editor Constitution

## Core Principles

### I. Layered Architecture Boundaries (NON-NEGOTIABLE)

The Lem editor follows a strict four-layer architecture with unidirectional dependencies:

```
Frontends → Interface (lem-if:*) → Core → Extensions
```

- **Dependency direction MUST flow downward only.** Lower layers MUST NOT reference upper layers.
- All frontend-specific code MUST implement the `lem-if:*` generic function protocol.
- Internal symbol access (`lem::`) is PROHIBITED. Use only public APIs from `lem:` or `lem-core:` packages.
- If internal access is unavoidable, document the reason explicitly.

**Rationale**: This separation enables multiple frontend implementations (SDL2, NCurses, Webview, Server) to share a common core without coupling.

### II. Protocol-First Design

Extension and customization MUST occur through established protocols and macros:

- When adding features, PREFER extending existing protocols over creating new abstractions.
- Frontend abstraction is achieved by subclassing the `implementation` class and implementing `lem-if:*` generic functions.
- Modes MUST be defined using `define-major-mode` or `define-minor-mode` macros.
- Commands MUST be defined using `define-command` with explicit argument descriptors (`:universal`, `:string`, `:number`, `:buffer`, `:file`, `:region`).

**Rationale**: Protocol-based design ensures consistency across 65+ extensions and enables proper keymap inheritance and hook integration.

### III. Test-Driven Development (NON-NEGOTIABLE)

All new features MUST follow the Red-Green-Refactor cycle:

1. Write a failing test that captures the expected behavior.
2. Implement the minimum code to make the test pass.
3. Refactor while keeping tests green.

- Every extension system MUST have a corresponding `lem-{name}/tests` test system.
- Integration tests run under the Rove framework via `make test` or `(asdf:test-system "lem-tests")`.
- Tests define WHAT the system does; implementation defines HOW.

**Rationale**: TDD ensures behavioral correctness and enables safe refactoring of the editor core.

### IV. Code Quality and Consistency

All code MUST comply with the rules defined in `contract.yml`:

- **Loop syntax**: Keywords MUST use colons: `(loop :for x :in list :do ...)`
- **Naming conventions**:
  - Functions/variables: `kebab-case` (e.g., `find-buffer`)
  - Special variables: `*earmuffs*` (e.g., `*global-keymap*`)
  - Constants: `+plus-signs+` (e.g., `+default-tab-size+`)
  - Predicates: `-p` suffix (e.g., `buffer-modified-p`)
- **Macros**: Keep macros small. Use the `call-with-*` pattern for complex logic.
- **Alexandria**: Allowed utilities include `if-let`, `when-let`, `with-gensyms`. Avoid `curry`.

**Rationale**: Enforced by code-contractor on every PR to maintain codebase consistency.

### V. Documentation at the Right Layer

Documentation MUST be placed at the appropriate abstraction level:

| Layer    | Documents                                      |
|----------|------------------------------------------------|
| Code     | How (implementation details)                   |
| Tests    | What (specification and behavior)              |
| Commits  | Why (rationale for changes)                    |
| Comments | Why not (design decisions and trade-offs)      |

- All exported functions, methods, and classes MUST have docstrings.
- `define-command` forms MUST include documentation explaining what the command does.
- Generic functions MUST use the `:documentation` option.

**Rationale**: Proper documentation layering prevents redundancy and ensures each artifact serves its purpose.

## Architectural Constraints

Core abstractions MUST remain stable and consistent:

- **Buffer**: Text storage with points, marks, undo history, syntax tables, and buffer-local variables.
- **Window**: Buffer display organized in a tree structure supporting splits and floating windows.
- **Mode**: Major (one per buffer) and Minor (multiple, toggleable) modes with hooks and keymaps.
- **Keymap**: Hierarchical key binding storage with parent-child relationships for inheritance.

Error handling conventions:

- `editor-error`: User-facing errors displayed in the echo area.
- `error`: Internal/programming errors for unexpected conditions.

Concurrency model:

- Event processing flows through the `event-queue` for thread-safe async execution.
- All user input and redraw operations coordinate through `send-event`.

System definition:

- Extensions MUST be defined as independent ASDF systems.
- System names follow the pattern `lem-{name}` (e.g., `lem-python-mode`).

## Extension Development Guidelines

Extensions MUST follow these structural requirements:

- Define as standalone systems named `lem-{name}.asd`.
- Depend on `lem/core`; minimize dependencies on other extensions.
- Hook into the editor lifecycle through mode hooks (`after-save-hook`, `kill-buffer-hook`, etc.).
- Register keybindings in mode-specific keymaps, not the global keymap.

Extension structure pattern:

```
extensions/my-mode/
├── lem-my-mode.asd          # System definition
├── my-mode.lisp             # Main mode implementation
├── syntax-parser.lisp       # Syntax highlighting (optional)
├── commands.lisp            # Commands (optional)
└── lsp-config.lisp          # LSP integration (optional)
```

Package naming:

- Package name MUST match filename (e.g., `foo.lisp` → `:lem-foo` or `:lem-ext/foo`).
- First form in file MUST be `defpackage` or `uiop:define-package`.

## Governance

This constitution supersedes all other practices and conventions within the Lem project.

**Amendment procedure**:

1. Amendments require an impact assessment on existing code.
2. A migration plan MUST be provided for breaking changes.
3. Amendments MUST maintain consistency with `contract.yml`.
4. Changes require PR review and approval from maintainers.

**Compliance verification**:

- All PRs MUST be reviewed for compliance with these principles.
- code-contractor enforces `contract.yml` rules automatically.
- Constitution violations MUST be documented and justified if unavoidable.

**Versioning policy**:

- MAJOR: Backward-incompatible governance or principle changes.
- MINOR: New principles or materially expanded guidance.
- PATCH: Clarifications, wording, or non-semantic refinements.

**Version**: 1.0.0 | **Ratified**: 2025-12-23 | **Last Amended**: 2025-12-23
