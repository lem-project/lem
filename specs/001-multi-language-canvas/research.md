# Research: Multi-Language Living Canvas

**Date**: 2025-12-23
**Status**: Complete

## Technical Decisions Summary

| Area | Decision | Rationale |
|------|----------|-----------|
| Primary Analysis | tree-sitter | Already integrated in Lem, supports incremental parsing, error recovery |
| Python Analysis | tree-sitter + Python AST fallback | tree-sitter-python for structure, Python AST subprocess for cross-file resolution |
| JS/TS Analysis | tree-sitter | tree-sitter-javascript/typescript available, handles JSX/TSX |
| Provider Registry | Hash-table with priority ordering | Simple, fast lookup, supports fallback chain |
| Language Detection | Major-mode → file extension → content inspection | Hierarchical detection aligns with Lem's existing patterns |
| Data Structures | Existing call-graph types | Already language-agnostic, proven with CL provider |

## Research Findings

### 1. Existing Infrastructure Assessment

**Call Graph Core Library** (`extensions/call-graph/`)

Decision: **Reuse existing structures**

The call-graph library provides language-agnostic data structures suitable for any language:

```lisp
(defstruct graph-node
  id              ; "module:function" format
  name            ; Function/method name
  package         ; Module/namespace
  type            ; :function, :method, :class, etc.
  docstring       ; Documentation
  arglist         ; Argument signature
  source-location ; (pathname . line-number)
  source-file)    ; Full path

(defstruct graph-edge
  source          ; Caller node ID
  target          ; Callee node ID
  call-type)      ; :direct, :inferred

(defstruct call-graph
  nodes           ; Hash table: id → graph-node
  edges           ; List of graph-edge
  root-package)   ; Analysis scope
```

Rationale: These structures work for any language. The CL provider proves the abstraction is correct.

Alternatives considered:
- New unified AST representation → Rejected: would require converting between formats
- Language-specific node types → Rejected: adds unnecessary complexity to core

### 2. Tree-Sitter Integration

**Available Infrastructure** (`extensions/tree-sitter/`)

Decision: **Use tree-sitter as primary analysis method**

Key APIs available:
- `ts:make-parser` / `ts:with-parser` - Create parsers
- `ts:parser-parse-string` - Parse source code
- `ts:tree-root-node` - Get AST root
- `ts:query-compile` / `ts:query-captures` - Query-based extraction
- `ts:node-start-point` / `ts:node-end-point` - Position tracking

Rationale:
1. Already integrated and tested in Lem (JSON, YAML, Nix, Markdown modes)
2. Supports 150+ languages via external grammars
3. Incremental parsing for real-time updates
4. Error recovery allows partial analysis of invalid code
5. S-expression query language for pattern matching

Alternatives considered:
- LSP-only approach → Rejected: requires running language servers, slow startup
- Regex-based extraction → Rejected: unreliable for nested structures
- External tools per language → Rejected: inconsistent APIs, process overhead

**Grammar Loading Strategy**:

```lisp
;; Load from system paths (Nix, package managers, etc.)
(ts:load-language-from-system "python")
(ts:load-language-from-system "javascript")
(ts:load-language-from-system "typescript")
```

### 3. Python Provider Design

Decision: **tree-sitter primary + Python AST subprocess for enhanced analysis**

**Phase 1: tree-sitter Based Analysis**

Query patterns for Python:
```scm
;; Function definitions
(function_definition
  name: (identifier) @function.name) @function

;; Class definitions
(class_definition
  name: (identifier) @class.name) @class

;; Method definitions (inside class)
(class_definition
  body: (block
    (function_definition
      name: (identifier) @method.name) @method))

;; Call expressions
(call
  function: (identifier) @call.direct)

(call
  function: (attribute
    attribute: (identifier) @call.method))
```

**Phase 2: Python AST Enhancement (Optional)**

For cross-file resolution and type information:
```python
# External Python script for enhanced analysis
import ast
import json
import sys

def analyze_file(filepath):
    with open(filepath) as f:
        tree = ast.parse(f.read())
    # Extract definitions and calls
    # Return JSON-serialized results
```

Rationale:
- tree-sitter handles most cases efficiently
- Python AST adds import resolution and cross-file analysis
- Subprocess approach avoids embedding Python interpreter

Alternatives considered:
- Pure Python subprocess → Rejected: slower than tree-sitter for single-file
- Pure tree-sitter → Kept as Phase 1, but limited for imports
- Jedi library via subprocess → Considered for Phase 2, too heavy initially

### 4. JavaScript/TypeScript Provider Design

Decision: **tree-sitter for both JS and TS**

**Query Patterns for JavaScript/TypeScript**:

```scm
;; Function declarations
(function_declaration
  name: (identifier) @function.name) @function

;; Arrow functions assigned to variables
(lexical_declaration
  (variable_declarator
    name: (identifier) @function.name
    value: (arrow_function) @function))

;; Method definitions
(method_definition
  name: (property_identifier) @method.name) @method

;; Function calls
(call_expression
  function: (identifier) @call.direct)

(call_expression
  function: (member_expression
    property: (property_identifier) @call.method))

;; Export declarations
(export_statement
  declaration: (function_declaration
    name: (identifier) @export.function))
```

**TypeScript Additions**:
```scm
;; Type annotations (for metadata)
(type_annotation) @type
```

Rationale:
- Single tree-sitter approach handles both JS and TS
- JSX/TSX variants supported by grammar extensions
- ES modules import/export naturally captured

Alternatives considered:
- TypeScript compiler API → Rejected: too heavy, requires Node.js
- Babel AST → Rejected: external dependency, process overhead

### 5. Provider Registry Design

Decision: **Simple hash-table registry with priority ordering**

```lisp
(defclass provider-registry ()
  ((providers :initform (make-hash-table :test 'equal)
              :accessor registry-providers)
   (language-map :initform (make-hash-table :test 'equal)
                 :accessor registry-language-map)))

;; Registration
(defun register-provider (registry language-name provider)
  (push provider (gethash language-name (registry-language-map registry))))

;; Selection (highest priority first)
(defun select-provider (registry language-name)
  (let ((providers (gethash language-name (registry-language-map registry))))
    (first (sort (copy-list providers) #'> :key #'provider-priority))))
```

Rationale:
- Fast O(1) lookup by language
- Priority allows fallback chains (tree-sitter → LSP → basic)
- Matches existing CL provider pattern

Alternatives considered:
- Global special variable list → Rejected: not extensible
- CLOS method dispatch → Considered, but overkill for simple selection
- Capability-based selection → Deferred to Phase 2

### 6. Language Detection Strategy

Decision: **Hierarchical detection: mode → extension → content**

```lisp
(defun detect-language (buffer)
  "Detect programming language for BUFFER."
  (or
   ;; 1. Major mode (most reliable)
   (mode-to-language (buffer-major-mode buffer))
   ;; 2. File extension
   (extension-to-language (pathname-type (buffer-filename buffer)))
   ;; 3. Content inspection (shebang, etc.)
   (inspect-content-language buffer)))

(defparameter *mode-language-map*
  '((python-mode . :python)
    (js-mode . :javascript)
    (typescript-mode . :typescript)
    (lisp-mode . :common-lisp)))

(defparameter *extension-language-map*
  '(("py" . :python)
    ("js" . :javascript)
    ("mjs" . :javascript)
    ("jsx" . :javascript)
    ("ts" . :typescript)
    ("tsx" . :typescript)
    ("lisp" . :common-lisp)
    ("cl" . :common-lisp)
    ("asd" . :common-lisp)))
```

Rationale:
- Aligns with Lem's existing file-type associations
- Major mode is most accurate when available
- Extension fallback handles new buffers

Alternatives considered:
- LSP language ID only → Rejected: requires LSP to be running
- Content-first detection → Rejected: slower, less reliable

### 7. Call Graph Construction Algorithm

Decision: **Two-pass: definition extraction → call resolution**

**Pass 1: Extract Definitions**

```lisp
(defun extract-definitions (tree language-queries)
  "Extract all function/method/class definitions from AST."
  (let ((query (ts:query-compile language (gethash :definitions language-queries)))
        (nodes (make-hash-table :test 'equal)))
    (dolist (capture (ts:query-captures query (ts:tree-root-node tree)))
      (let* ((node (ts:capture-node capture))
             (name (capture-name-from-node node))
             (graph-node (make-graph-node
                          :id (format nil "~A:~A" module-name name)
                          :name name
                          :type (node-type-to-symbol (ts:node-type node))
                          :source-location (cons filepath (1+ (car (ts:node-start-point node))))
                          :source-file filepath)))
        (setf (gethash (graph-node-id graph-node) nodes) graph-node)))
    nodes))
```

**Pass 2: Resolve Calls**

```lisp
(defun extract-calls (tree language-queries definitions)
  "Extract call relationships and resolve to known definitions."
  (let ((query (ts:query-compile language (gethash :calls language-queries)))
        (edges nil))
    (dolist (capture (ts:query-captures query (ts:tree-root-node tree)))
      (let* ((call-node (ts:capture-node capture))
             (caller (find-enclosing-function call-node definitions))
             (callee-name (extract-callee-name call-node))
             (callee (gethash callee-name definitions)))
        (when (and caller callee)
          (push (make-graph-edge
                 :source (graph-node-id caller)
                 :target (graph-node-id callee)
                 :call-type :direct)
                edges))))
    edges))
```

Rationale:
- Clear separation of concerns
- Definitions collected first for resolution lookup
- Unresolved calls can be handled gracefully (external functions)

Alternatives considered:
- Single-pass with deferred resolution → More complex state management
- Full scope analysis → Deferred to Phase 2, requires symbol tables

### 8. Performance Considerations

Decision: **Lazy loading + caching strategy**

| Metric | Target | Strategy |
|--------|--------|----------|
| Single file parse | < 500ms | tree-sitter incremental parsing |
| Graph generation | < 3s for 100 functions | Two-pass algorithm |
| UI responsiveness | < 100ms updates | Async analysis, cached results |
| Memory | < 10MB per file | Share parser instances |

**Caching Architecture**:

```lisp
(defclass provider-cache ()
  ((file-hash :accessor cache-file-hash)       ; Detect file changes
   (parse-tree :accessor cache-parse-tree)     ; Reuse tree for queries
   (call-graph :accessor cache-call-graph)     ; Final result
   (timestamp :accessor cache-timestamp)))     ; Invalidation

(defun analyze-with-cache (provider source)
  (let ((cache (get-cache provider source)))
    (if (cache-valid-p cache source)
        (cache-call-graph cache)
        (let ((result (provider-analyze provider source)))
          (update-cache cache source result)
          result))))
```

Rationale:
- tree-sitter already supports incremental parsing
- Caching avoids re-analysis on repeated access
- Hash-based invalidation is simple and reliable

### 9. Error Handling Strategy

Decision: **Graceful degradation with user feedback**

| Error Type | Handling | User Message |
|------------|----------|--------------|
| Grammar not loaded | Try alternative provider | "Tree-sitter grammar unavailable, using basic analysis" |
| Parse error | Show partial graph | "Some code couldn't be parsed, showing available functions" |
| No definitions found | Empty graph with message | "No functions found in this file" |
| External call unresolved | Show node without edge | Callee node marked as "external" |
| Provider unavailable | Try next in priority | "Python provider unavailable, install tree-sitter-python" |

```lisp
(define-condition provider-error (error)
  ((provider :initarg :provider)
   (message :initarg :message)))

(defun safe-analyze (provider source)
  (handler-case
      (provider-analyze provider source)
    (provider-error (c)
      (editor-message "~A" (slot-value c 'message))
      nil)
    (error (c)
      (editor-error "Analysis failed: ~A" c)
      nil)))
```

Rationale:
- Users should see something rather than nothing
- Clear messages help users resolve issues
- Follows Lem's `editor-error` convention

## Dependencies

### Required (Phase 1)

| Dependency | Version | Purpose |
|------------|---------|---------|
| tree-sitter-cl | bundled | FFI bindings for tree-sitter |
| tree-sitter-python | system | Python grammar |
| tree-sitter-javascript | system | JS/TS grammar |
| tree-sitter-typescript | system | TypeScript additions |
| lem-tree-sitter | bundled | Lem integration layer |
| lem-call-graph | bundled | Data structures |
| lem-living-canvas | bundled | UI and commands |

### Optional (Phase 2)

| Dependency | Purpose |
|------------|---------|
| python3 | Enhanced cross-file analysis |
| pylsp | LSP-based type information |
| typescript-language-server | Enhanced TS analysis |

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| tree-sitter grammar unavailable | Low | High | Provide installation instructions, fallback to regex |
| Performance degradation on large files | Medium | Medium | Lazy loading, chunked analysis |
| Cross-file resolution inaccurate | High | Low | Mark as "inferred", defer to Phase 2 |
| Breaking existing CL provider | Low | High | Extensive test coverage, preserve interface |

## Implementation Priority

1. **Provider Registry** - Foundation for all providers
2. **tree-sitter Python Provider** - Highest user demand
3. **tree-sitter JS/TS Provider** - Second highest demand
4. **Language Detection Enhancement** - Polish and edge cases
5. **LSP Integration** (Phase 2) - Enhanced analysis accuracy

## References

- Lem tree-sitter integration: `extensions/tree-sitter/`
- Call graph types: `extensions/call-graph/types.lisp`
- CL provider reference: `extensions/living-canvas/micros-cl-provider.lisp`
- tree-sitter-cl API: `.qlot/dists/tree-sitter-cl/`
- Spec: `specs/001-multi-language-canvas/spec.md`
