# Tree-Sitter Query Contracts

**Date**: 2025-12-23
**Status**: Specification

This document defines the tree-sitter query patterns for extracting call graph information from various programming languages.

## Query Categories

Each language provider requires queries for:

1. **Definitions** - Function, method, and class definitions
2. **Calls** - Function/method call sites
3. **Imports** (optional) - Import/require statements for cross-file resolution

## Python Queries

### definitions.scm

```scm
;; Top-level function definitions
(function_definition
  name: (identifier) @function.name
  parameters: (parameters) @function.params
  body: (block
    (expression_statement
      (string) @function.docstring)?)) @function

;; Async function definitions
(function_definition
  "async" @function.async
  name: (identifier) @function.name
  parameters: (parameters) @function.params) @function

;; Class definitions
(class_definition
  name: (identifier) @class.name
  body: (block) @class.body) @class

;; Method definitions (inside class body)
(class_definition
  body: (block
    (function_definition
      name: (identifier) @method.name
      parameters: (parameters) @method.params) @method))

;; Decorated functions
(decorated_definition
  (decorator
    (identifier) @decorator.name)?
  definition: (function_definition
    name: (identifier) @function.name) @function)

;; Lambda expressions assigned to variables
(assignment
  left: (identifier) @lambda.name
  right: (lambda) @lambda)
```

### calls.scm

```scm
;; Direct function calls
(call
  function: (identifier) @call.function)

;; Method calls
(call
  function: (attribute
    object: (_) @call.object
    attribute: (identifier) @call.method))

;; Constructor calls (PascalCase heuristic)
(call
  function: (identifier) @call.constructor
  (#match? @call.constructor "^[A-Z]"))

;; Super calls
(call
  function: (attribute
    object: (call
      function: (identifier) @call.super
      (#eq? @call.super "super"))
    attribute: (identifier) @call.method))

;; Chained method calls
(call
  function: (attribute
    object: (call) @call.chain.prev
    attribute: (identifier) @call.chain.method))
```

### imports.scm

```scm
;; import module
(import_statement
  name: (dotted_name) @import.module)

;; from module import name
(import_from_statement
  module_name: (dotted_name) @import.module
  name: (dotted_name
    (identifier) @import.name))

;; from module import *
(import_from_statement
  module_name: (dotted_name) @import.module
  (wildcard_import) @import.star)

;; import module as alias
(import_statement
  name: (aliased_import
    name: (dotted_name) @import.module
    alias: (identifier) @import.alias))
```

## JavaScript/TypeScript Queries

### definitions.scm

```scm
;; Function declarations
(function_declaration
  name: (identifier) @function.name
  parameters: (formal_parameters) @function.params) @function

;; Async function declarations
(function_declaration
  "async" @function.async
  name: (identifier) @function.name) @function

;; Arrow functions assigned to const/let
(lexical_declaration
  (variable_declarator
    name: (identifier) @function.name
    value: (arrow_function
      parameters: (formal_parameters) @function.params) @function))

;; Arrow functions assigned to var
(variable_declaration
  (variable_declarator
    name: (identifier) @function.name
    value: (arrow_function) @function))

;; Class declarations
(class_declaration
  name: (identifier) @class.name
  body: (class_body) @class.body) @class

;; Method definitions
(method_definition
  name: (property_identifier) @method.name
  parameters: (formal_parameters) @method.params) @method

;; Constructor
(method_definition
  name: (property_identifier) @method.constructor
  (#eq? @method.constructor "constructor")) @constructor

;; Getter/setter methods
(method_definition
  "get" @method.getter
  name: (property_identifier) @method.name) @method

(method_definition
  "set" @method.setter
  name: (property_identifier) @method.name) @method

;; Generator functions
(generator_function_declaration
  name: (identifier) @function.name
  "*" @function.generator) @function

;; Object method shorthand
(pair
  key: (property_identifier) @function.name
  value: (function) @function)
```

### calls.scm

```scm
;; Direct function calls
(call_expression
  function: (identifier) @call.function)

;; Method calls
(call_expression
  function: (member_expression
    object: (_) @call.object
    property: (property_identifier) @call.method))

;; Constructor calls with new
(new_expression
  constructor: (identifier) @call.constructor)

;; Chained calls
(call_expression
  function: (member_expression
    object: (call_expression) @call.chain.prev
    property: (property_identifier) @call.chain.method))

;; Optional chaining calls
(call_expression
  function: (member_expression
    "?." @call.optional
    property: (property_identifier) @call.method))

;; IIFE (Immediately Invoked Function Expression)
(call_expression
  function: (parenthesized_expression
    (function) @call.iife))
```

### imports.scm

```scm
;; ES6 import default
(import_statement
  (import_clause
    (identifier) @import.default)
  source: (string) @import.module)

;; ES6 import named
(import_statement
  (import_clause
    (named_imports
      (import_specifier
        name: (identifier) @import.name)))
  source: (string) @import.module)

;; ES6 import namespace
(import_statement
  (import_clause
    (namespace_import
      (identifier) @import.namespace))
  source: (string) @import.module)

;; CommonJS require
(variable_declarator
  name: (identifier) @import.name
  value: (call_expression
    function: (identifier) @import.require
    (#eq? @import.require "require")
    arguments: (arguments
      (string) @import.module)))

;; Dynamic import
(call_expression
  function: (import) @import.dynamic
  arguments: (arguments
    (string) @import.module))
```

### exports.scm

```scm
;; ES6 export default
(export_statement
  "default" @export.default
  declaration: (_) @export.value)

;; ES6 export named function
(export_statement
  declaration: (function_declaration
    name: (identifier) @export.function))

;; ES6 export named class
(export_statement
  declaration: (class_declaration
    name: (identifier) @export.class))

;; ES6 export named const/let
(export_statement
  declaration: (lexical_declaration
    (variable_declarator
      name: (identifier) @export.variable)))

;; Re-export
(export_statement
  source: (string) @export.from
  (export_clause
    (export_specifier
      name: (identifier) @export.name)))

;; Export all
(export_statement
  "*" @export.all
  source: (string) @export.from)
```

## TypeScript-Specific Queries

### types.scm

```scm
;; Interface declarations
(interface_declaration
  name: (type_identifier) @interface.name
  body: (interface_body) @interface.body) @interface

;; Type alias
(type_alias_declaration
  name: (type_identifier) @type.name
  value: (_) @type.value) @type

;; Enum declarations
(enum_declaration
  name: (identifier) @enum.name
  body: (enum_body) @enum.body) @enum

;; Function with return type
(function_declaration
  name: (identifier) @function.name
  return_type: (type_annotation
    (type_identifier) @function.return_type)?) @function

;; Method with type annotations
(method_definition
  name: (property_identifier) @method.name
  return_type: (type_annotation
    (_) @method.return_type)?) @method
```

## Query Execution Pattern

```lisp
(defun extract-with-queries (tree language query-definitions)
  "Extract call graph information from TREE using QUERY-DEFINITIONS."
  (let ((nodes (make-hash-table :test 'equal))
        (edges nil))
    ;; Pass 1: Extract definitions
    (dolist (capture (ts:query-captures
                      (ts:query-compile language (gethash :definitions query-definitions))
                      (ts:tree-root-node tree)))
      (process-definition-capture capture nodes))
    ;; Pass 2: Extract calls and create edges
    (dolist (capture (ts:query-captures
                      (ts:query-compile language (gethash :calls query-definitions))
                      (ts:tree-root-node tree)))
      (process-call-capture capture nodes edges))
    (values nodes edges)))

(defun process-definition-capture (capture nodes)
  "Process a definition capture and add to NODES hash table."
  (let* ((node (ts:capture-node capture))
         (name-capture (find-named-child capture "name"))
         (name (ts:node-text name-capture))
         (start-point (ts:node-start-point node))
         (type (capture-name-to-type (ts:capture-name capture))))
    (setf (gethash name nodes)
          (make-graph-node
           :id (format nil "~A:~A" *current-module* name)
           :name name
           :type type
           :source-location (cons *current-file* (1+ (car start-point)))
           :source-file *current-file*))))

(defun process-call-capture (capture nodes edges)
  "Process a call capture and add edge to EDGES if target found."
  (let* ((node (ts:capture-node capture))
         (callee-name (extract-callee-name node))
         (caller (find-enclosing-function node nodes)))
    (when (and caller (gethash callee-name nodes))
      (push (make-graph-edge
             :source (graph-node-id caller)
             :target (gethash callee-name nodes)
             :call-type :direct)
            edges))))
```

## Capture Naming Conventions

| Capture Name | Purpose |
|--------------|---------|
| `@function` | Function definition node |
| `@function.name` | Function name identifier |
| `@function.params` | Function parameters |
| `@function.docstring` | Documentation string |
| `@function.async` | Async keyword marker |
| `@method` | Method definition node |
| `@method.name` | Method name identifier |
| `@class` | Class definition node |
| `@class.name` | Class name identifier |
| `@call.function` | Called function name |
| `@call.method` | Called method name |
| `@call.object` | Object receiving method call |
| `@import.module` | Imported module path |
| `@import.name` | Imported symbol name |
| `@export.function` | Exported function name |

## Testing Queries

Each query file should have corresponding test files:

```
contracts/
├── python/
│   ├── definitions.scm
│   ├── calls.scm
│   ├── imports.scm
│   └── tests/
│       ├── definitions-test.py
│       ├── calls-test.py
│       └── imports-test.py
├── javascript/
│   ├── definitions.scm
│   ├── calls.scm
│   ├── imports.scm
│   └── tests/
│       ├── definitions-test.js
│       └── ...
└── typescript/
    └── ...
```

Test files should contain sample code that exercises each query pattern.
