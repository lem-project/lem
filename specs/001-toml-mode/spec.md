# Feature Specification: TOML Mode Extension

**Feature Branch**: `001-toml-mode`
**Created**: 2025-12-30
**Status**: Draft
**Input**: User description: "Add a TOML mode extension to Lem editor that enables developers to comfortably edit TOML configuration files."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Open and Edit TOML Files with Syntax Highlighting (Priority: P1)

As a developer, I want to open TOML configuration files in Lem and see proper syntax highlighting so that I can quickly identify different elements like keys, values, strings, and comments.

**Why this priority**: This is the core functionality of any language mode. Without syntax highlighting, there is no meaningful value in having a dedicated TOML mode. This delivers immediate visual feedback that helps developers parse configuration files at a glance.

**Independent Test**: Can be fully tested by opening any `.toml` file and verifying that different syntax elements are colored distinctly. Delivers immediate value for any TOML file editing.

**Acceptance Scenarios**:

1. **Given** a file named `Cargo.toml` exists, **When** I open it in Lem, **Then** TOML mode activates automatically and syntax highlighting is applied
2. **Given** TOML mode is active, **When** I view a table header like `[package]`, **Then** it is visually distinguished from regular text
3. **Given** TOML mode is active, **When** I view string values (both `"basic"` and `'literal'`), **Then** they are highlighted as strings
4. **Given** TOML mode is active, **When** I view comments starting with `#`, **Then** they are highlighted as comments
5. **Given** TOML mode is active, **When** I view boolean values (`true`/`false`), **Then** they are highlighted distinctly

---

### User Story 2 - Comment Out Lines Using Standard Commands (Priority: P2)

As a developer, I want to use Lem's standard comment commands to quickly comment and uncomment lines in TOML files so that I can temporarily disable configuration options.

**Why this priority**: Commenting is a fundamental editing operation when working with configuration files. Developers frequently need to disable/enable settings without deleting them.

**Independent Test**: Can be tested by selecting lines and using the comment command, verifying that `#` is properly inserted/removed.

**Acceptance Scenarios**:

1. **Given** TOML mode is active with cursor on a line, **When** I invoke the line-comment command, **Then** a `#` character is inserted at the beginning of the line
2. **Given** a line already starts with `#`, **When** I invoke the uncomment command, **Then** the leading `#` is removed
3. **Given** multiple lines are selected, **When** I invoke the comment command, **Then** all selected lines are commented with `#`

---

### User Story 3 - Consistent Visual Experience with Other Modes (Priority: P3)

As a developer switching between different configuration file formats, I want TOML mode to have a consistent visual appearance with YAML and JSON modes so that I have a familiar editing experience across formats.

**Why this priority**: Consistency improves user experience and reduces cognitive load when switching between similar file types. This is important but not blocking for basic functionality.

**Independent Test**: Can be tested by opening TOML, YAML, and JSON files side by side and comparing visual consistency of similar elements (strings, numbers, booleans).

**Acceptance Scenarios**:

1. **Given** TOML, YAML, and JSON files are open, **When** I compare string highlighting, **Then** strings use the same color scheme across all three modes
2. **Given** TOML, YAML, and JSON files are open, **When** I compare number highlighting, **Then** numbers use the same color scheme across all three modes
3. **Given** TOML, YAML, and JSON files are open, **When** I compare comment highlighting, **Then** comments use the same color scheme across all three modes

---

### Edge Cases

- What happens when opening a file with `.toml` extension but invalid TOML syntax? (Mode should still activate and provide best-effort highlighting)
- How does the system handle very large TOML files? (Performance should remain acceptable using tree-sitter's incremental parsing)
- What happens with nested inline tables like `{a = {b = 1}}`? (Should highlight nested structures correctly)
- How are multi-line strings (triple-quoted) handled? (Should highlight the entire multi-line block as a string)
- What happens with datetime values like `2024-01-15T10:30:00Z`? (Should be highlighted as a distinct type)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST automatically activate TOML mode when opening files with `.toml` extension
- **FR-002**: System MUST provide syntax highlighting for TOML table headers (`[table]` and `[[array-of-tables]]`)
- **FR-003**: System MUST provide syntax highlighting for key names in key-value pairs
- **FR-004**: System MUST provide syntax highlighting for string values (basic strings with `"`, literal strings with `'`, multi-line variants)
- **FR-005**: System MUST provide syntax highlighting for numeric values (integers, floats, special float values like `inf` and `nan`)
- **FR-006**: System MUST provide syntax highlighting for boolean values (`true` and `false`)
- **FR-007**: System MUST provide syntax highlighting for date and time values (offset datetime, local datetime, local date, local time)
- **FR-008**: System MUST provide syntax highlighting for comments (lines starting with `#`)
- **FR-009**: System MUST provide syntax highlighting for arrays (`[...]`) and inline tables (`{...}`)
- **FR-010**: System MUST support line comment functionality using the `#` character
- **FR-011**: System MUST integrate with Lem's existing language-mode infrastructure
- **FR-012**: System MUST use tree-sitter for syntax parsing to ensure consistency with other modern language modes

### Key Entities

- **TOML Document**: A configuration file containing tables, key-value pairs, and comments
- **Table**: A named section in TOML (e.g., `[package]`, `[dependencies]`) that groups related key-value pairs
- **Key-Value Pair**: A configuration entry with a key name and associated value
- **Value Types**: Strings, integers, floats, booleans, dates/times, arrays, and inline tables

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can identify different TOML elements (keys, values, comments, tables) at a glance without reading the content
- **SC-002**: Mode activates within 100ms of opening a `.toml` file, providing immediate syntax highlighting
- **SC-003**: 100% of TOML v1.0.0 syntax elements are correctly highlighted (tables, arrays of tables, all value types, comments)
- **SC-004**: Comment/uncomment operations complete in under 50ms regardless of selection size
- **SC-005**: Users familiar with Lem's YAML or JSON modes can immediately use TOML mode without additional learning

## Assumptions

- Tree-sitter TOML grammar is available and compatible with Lem's tree-sitter integration
- Users are familiar with TOML file format basics
- The implementation follows TOML v1.0.0 specification
- Default indentation uses spaces (2 spaces) consistent with common TOML conventions
