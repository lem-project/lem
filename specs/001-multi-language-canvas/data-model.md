# Data Model: Multi-Language Living Canvas

**Date**: 2025-12-23
**Status**: Complete

## Entity Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                     Provider Registry                            │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐ │
│  │  CL Provider    │  │ Python Provider │  │  JS/TS Provider │ │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘ │
└───────────┼─────────────────────┼─────────────────────┼─────────┘
            │                     │                     │
            └─────────────────────┴─────────────────────┘
                                  │
                                  ▼
                         ┌────────────────┐
                         │   Call Graph   │
                         │ (nodes, edges) │
                         └───────┬────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              ▼                  ▼                  ▼
       ┌────────────┐    ┌─────────────┐    ┌──────────────┐
       │ Graph Node │────│ Graph Edge  │────│Source Location│
       └────────────┘    └─────────────┘    └──────────────┘
```

## Entities

### 1. Call Graph Provider (Protocol)

**Purpose**: Abstract interface for language-specific analysis backends.

**Protocol Definition**:

```lisp
(defclass call-graph-provider ()
  ()
  (:documentation "Base class for language-specific call graph providers."))

(defgeneric provider-name (provider)
  (:documentation "Return keyword identifying the provider (e.g., :python, :javascript)."))

(defgeneric provider-supports-p (provider source)
  (:documentation "Return T if this provider can analyze SOURCE.
SOURCE can be a buffer, pathname, or string (code)."))

(defgeneric provider-analyze (provider source &key type)
  (:documentation "Analyze SOURCE and return a CALL-GRAPH structure.
TYPE can be :file, :buffer, :package, :project, or :system."))

(defgeneric provider-priority (provider)
  (:documentation "Return integer priority. Higher = tried first.
Default is 0. LSP providers typically use 10, tree-sitter use 5."))
```

**Implementations**:

| Provider | Language(s) | Priority | Analysis Method |
|----------|-------------|----------|-----------------|
| `micros-cl-call-graph-provider` | Common Lisp | 10 | SBCL introspection via RPC |
| `tree-sitter-python-provider` | Python | 5 | tree-sitter AST |
| `tree-sitter-js-provider` | JavaScript, TypeScript | 5 | tree-sitter AST |
| `lsp-call-graph-provider` | Any with CallHierarchy | 3 | LSP protocol |

### 2. Provider Registry

**Purpose**: Manages provider registration and selection.

```lisp
(defclass provider-registry ()
  ((providers
    :initform (make-hash-table :test 'equal)
    :accessor registry-providers
    :documentation "Hash table: provider-name → provider instance")
   (language-providers
    :initform (make-hash-table :test 'equal)
    :accessor registry-language-providers
    :documentation "Hash table: language-keyword → list of providers"))
  (:documentation "Central registry for call graph providers."))
```

**Fields**:

| Field | Type | Description |
|-------|------|-------------|
| `providers` | `hash-table (keyword → provider)` | All registered providers by name |
| `language-providers` | `hash-table (keyword → list)` | Providers per language, ordered by priority |

**Operations**:

```lisp
(defgeneric register-provider (registry provider languages)
  (:documentation "Register PROVIDER for LANGUAGES (list of keywords)."))

(defgeneric unregister-provider (registry provider-name)
  (:documentation "Remove provider with PROVIDER-NAME from registry."))

(defgeneric find-provider (registry language &optional source)
  (:documentation "Find best provider for LANGUAGE that supports SOURCE."))

(defgeneric list-providers (registry &optional language)
  (:documentation "List all providers, optionally filtered by LANGUAGE."))
```

**Validation Rules**:
- Provider name must be a keyword
- Languages must be a non-empty list of keywords
- Provider must implement required protocol methods

### 3. Graph Node

**Purpose**: Represents a function, method, class, or other callable entity.

**Existing Structure** (from `call-graph/types.lisp`):

```lisp
(defstruct graph-node
  (id nil :type (or string null))
  (name nil :type (or string null))
  (package nil :type (or string null))
  (type nil :type (or keyword null))
  (docstring nil :type (or string null))
  (arglist nil :type (or string null))
  (source-location nil :type (or cons null))
  (source-file nil :type (or pathname string null))
  (position nil :type (or cons null)))
```

**Field Details**:

| Field | Type | Required | Description | Example |
|-------|------|----------|-------------|---------|
| `id` | `string` | Yes | Unique identifier | `"mymodule:my_function"` |
| `name` | `string` | Yes | Display name | `"my_function"` |
| `package` | `string` | No | Module/namespace | `"mymodule"` |
| `type` | `keyword` | Yes | Entity type | `:function`, `:method`, `:class` |
| `docstring` | `string` | No | Documentation | `"Returns the sum..."` |
| `arglist` | `string` | No | Signature | `"(a, b, c=None)"` |
| `source-location` | `(pathname . line)` | No | Definition location | `(#P"foo.py" . 42)` |
| `source-file` | `pathname` | No | Redundant with above | For quick access |
| `position` | `(x . y)` | No | Canvas position | For layout persistence |

**Type Keywords**:

| Keyword | Description | Languages |
|---------|-------------|-----------|
| `:function` | Top-level function | All |
| `:method` | Class/instance method | Python, JS, CL |
| `:class` | Class definition | Python, JS, TS |
| `:macro` | Macro definition | Common Lisp |
| `:generic-function` | Generic function | Common Lisp |
| `:command` | Editor command | Common Lisp (Lem) |
| `:major-mode` | Editor mode | Common Lisp (Lem) |
| `:minor-mode` | Editor minor mode | Common Lisp (Lem) |
| `:arrow-function` | Arrow function | JavaScript, TypeScript |
| `:async-function` | Async function | Python, JavaScript |
| `:generator` | Generator function | Python, JavaScript |
| `:interface` | Interface | TypeScript |

**Validation Rules**:
- `id` must be unique within a call graph
- `id` format: `"module:name"` or `"module.class:method"`
- `type` must be a recognized keyword
- `source-location` line numbers are 1-indexed

### 4. Graph Edge

**Purpose**: Represents a call relationship between nodes.

**Existing Structure**:

```lisp
(defstruct graph-edge
  (source nil :type (or string null))
  (target nil :type (or string null))
  (call-type nil :type (or keyword null)))
```

**Field Details**:

| Field | Type | Required | Description | Example |
|-------|------|----------|-------------|---------|
| `source` | `string` | Yes | Caller node ID | `"main:process"` |
| `target` | `string` | Yes | Callee node ID | `"utils:helper"` |
| `call-type` | `keyword` | No | Relationship type | `:direct`, `:inferred` |

**Call Type Keywords**:

| Keyword | Description |
|---------|-------------|
| `:direct` | Explicit function call in source |
| `:inferred` | Derived from symbol usage |
| `:indirect` | Via function pointer/callback |
| `:super` | Super class method call |
| `:import` | Import/use relationship |

**Validation Rules**:
- `source` and `target` must reference existing node IDs
- Self-referential edges (recursion) are allowed
- Duplicate edges with same source/target are allowed (multiple call sites)

### 5. Call Graph

**Purpose**: Complete graph representation for visualization.

**Existing Structure**:

```lisp
(defstruct call-graph
  (nodes nil :type (or hash-table null))
  (edges nil :type list)
  (root-package nil :type (or string null)))
```

**Field Details**:

| Field | Type | Description |
|-------|------|-------------|
| `nodes` | `hash-table (id → graph-node)` | All nodes indexed by ID |
| `edges` | `list of graph-edge` | All call relationships |
| `root-package` | `string` | Analysis scope identifier |

**Derived Properties**:

```lisp
(defun call-graph-node-count (graph)
  "Return number of nodes in GRAPH."
  (hash-table-count (call-graph-nodes graph)))

(defun call-graph-edge-count (graph)
  "Return number of edges in GRAPH."
  (length (call-graph-edges graph)))

(defun call-graph-roots (graph)
  "Return nodes with no incoming edges (entry points)."
  ...)

(defun call-graph-leaves (graph)
  "Return nodes with no outgoing edges (terminals)."
  ...)
```

### 6. Language Definition

**Purpose**: Configuration for language-specific analysis.

**New Structure**:

```lisp
(defstruct language-definition
  (name nil :type keyword)
  (display-name nil :type string)
  (file-extensions nil :type list)
  (major-modes nil :type list)
  (tree-sitter-grammar nil :type (or string null))
  (queries nil :type (or hash-table null)))
```

**Field Details**:

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `name` | `keyword` | Identifier | `:python` |
| `display-name` | `string` | UI display | `"Python"` |
| `file-extensions` | `list of string` | Extensions | `("py" "pyw")` |
| `major-modes` | `list of symbol` | Lem modes | `(python-mode)` |
| `tree-sitter-grammar` | `string` | Grammar name | `"python"` |
| `queries` | `hash-table` | Query patterns | See below |

**Query Keys**:

| Key | Purpose |
|-----|---------|
| `:definitions` | Find function/class definitions |
| `:calls` | Find function calls |
| `:imports` | Find import statements |
| `:exports` | Find exports (JS modules) |
| `:docstrings` | Extract documentation |
| `:classes` | Find class definitions |

### 7. Analysis Context

**Purpose**: Context passed during analysis for configuration.

**New Structure**:

```lisp
(defstruct analysis-context
  (buffer nil :type (or buffer null))
  (file nil :type (or pathname null))
  (project-root nil :type (or pathname null))
  (include-external-p nil :type boolean)
  (max-depth nil :type (or integer null))
  (options nil :type list))
```

**Field Details**:

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `buffer` | `buffer` | nil | Source buffer if analyzing buffer |
| `file` | `pathname` | nil | Source file if analyzing file |
| `project-root` | `pathname` | nil | Project root for multi-file analysis |
| `include-external-p` | `boolean` | nil | Include external/library calls |
| `max-depth` | `integer` | nil | Maximum call depth to traverse |
| `options` | `plist` | nil | Provider-specific options |

## Relationships

```
Provider Registry ────1:N──── Provider
        │
        │ selects
        ▼
    Provider ────analyzes──── Source
        │
        │ produces
        ▼
    Call Graph ────1:N──── Graph Node
        │                       │
        │                       │ references
        │                       ▼
        └──────1:N──── Graph Edge
                           │
                           │ links
                           ▼
                      Source Location
```

## State Transitions

### Provider Registration Flow

```
┌─────────────┐   register   ┌────────────┐   activate   ┌────────┐
│ Unregistered│─────────────▶│ Registered │─────────────▶│ Active │
└─────────────┘              └────────────┘              └────────┘
                                   │                          │
                                   │ unregister               │ deactivate
                                   ▼                          ▼
                             ┌────────────┐            ┌──────────┐
                             │  Removed   │            │ Inactive │
                             └────────────┘            └──────────┘
```

### Analysis Flow

```
┌─────────┐   detect   ┌──────────┐   parse   ┌────────┐   build   ┌───────┐
│ Source  │───────────▶│ Language │──────────▶│  AST   │─────────▶│ Graph │
└─────────┘            └──────────┘           └────────┘          └───────┘
                             │                     │                   │
                             │ unknown             │ error             │ success
                             ▼                     ▼                   ▼
                       ┌──────────┐          ┌─────────┐         ┌─────────┐
                       │ Fallback │          │ Partial │         │ Display │
                       └──────────┘          └─────────┘         └─────────┘
```

## Serialization

### Cytoscape.js Format (Existing)

```json
{
  "nodes": [
    {
      "data": {
        "id": "mymodule:func",
        "label": "func",
        "type": "function",
        "parent": "mymodule",
        "docstring": "...",
        "arglist": "(a, b)",
        "sourceFile": "/path/to/file.py",
        "sourceLine": 42
      }
    }
  ],
  "edges": [
    {
      "data": {
        "source": "mymodule:func",
        "target": "utils:helper",
        "callType": "direct"
      }
    }
  ]
}
```

### Provider Configuration (New)

```lisp
;; Provider definition example
(define-call-graph-provider python-provider
  :name :python
  :languages (:python)
  :priority 5
  :supports (lambda (source)
              (or (python-buffer-p source)
                  (python-file-p source)))
  :analyze #'analyze-python-source)
```

## Indexes and Lookups

| Index | Key | Value | Purpose |
|-------|-----|-------|---------|
| `nodes` | node-id (string) | graph-node | Primary node lookup |
| `providers` | provider-name (keyword) | provider | Provider by name |
| `language-providers` | language (keyword) | list of providers | Providers for language |
| `source-cache` | file-hash (integer) | call-graph | Analysis caching |

## Constraints and Invariants

1. **Node ID Uniqueness**: No two nodes in a call-graph may share the same ID
2. **Edge Validity**: Edge source and target must reference existing nodes (or external markers)
3. **Provider Priority**: At most one provider may be active for analysis at a time
4. **Language Consistency**: A call-graph should contain nodes from a single language context
5. **Source Location Validity**: Line numbers must be positive integers (1-indexed)
6. **Graph Acyclicity** (soft): While cycles (recursion) are allowed, UI should handle them gracefully
