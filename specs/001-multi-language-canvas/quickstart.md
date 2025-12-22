# Quick Start: Multi-Language Living Canvas

**Date**: 2025-12-23
**Audience**: Developers implementing this feature

## Overview

This guide walks through implementing multi-language support for Living Canvas. The implementation adds Python and JavaScript/TypeScript providers while preserving the existing Common Lisp functionality.

## Prerequisites

1. **Lem Development Environment**
   ```bash
   git clone https://github.com/lem-project/lem
   cd lem
   qlot install
   make sdl2  # or make ncurses
   ```

2. **tree-sitter Grammars**
   ```bash
   # Via Nix (recommended)
   nix develop

   # Or system packages
   # Ubuntu/Debian
   apt install libtree-sitter-dev tree-sitter-python tree-sitter-javascript

   # macOS
   brew install tree-sitter
   ```

3. **Verify tree-sitter-cl**
   ```lisp
   ;; In Lem REPL
   (ql:quickload :tree-sitter-cl)
   (ts:load-language-from-system "python")
   (ts:list-languages)  ; Should include "python"
   ```

## Implementation Steps

### Step 1: Provider Registry (2 files, ~150 LOC)

Create the provider registry that manages language providers.

**File: `extensions/call-graph/registry.lisp`**

```lisp
(defpackage :lem-call-graph/registry
  (:use :cl :lem-call-graph)
  (:export #:*provider-registry*
           #:register-provider
           #:unregister-provider
           #:find-provider
           #:list-providers))
(in-package :lem-call-graph/registry)

(defclass provider-registry ()
  ((providers :initform (make-hash-table :test 'eq)
              :accessor registry-providers)
   (language-map :initform (make-hash-table :test 'eq)
                 :accessor registry-language-map)))

(defvar *provider-registry* (make-instance 'provider-registry)
  "Global provider registry instance.")

(defun register-provider (registry provider languages)
  "Register PROVIDER for LANGUAGES."
  (let ((name (provider-name provider)))
    (setf (gethash name (registry-providers registry)) provider)
    (dolist (lang languages)
      (push provider (gethash lang (registry-language-map registry))))))

(defun find-provider (registry language &optional source)
  "Find best provider for LANGUAGE."
  (let ((providers (gethash language (registry-language-map registry))))
    (when providers
      (let ((sorted (sort (copy-list providers) #'> :key #'provider-priority)))
        (if source
            (find-if (lambda (p) (provider-supports-p p source)) sorted)
            (first sorted))))))
```

**Update: `extensions/call-graph/lem-call-graph.asd`**

```lisp
(defsystem "lem-call-graph"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "provider")
               (:file "registry")))  ; Add this line
```

### Step 2: Language Detection (1 file, ~80 LOC)

Add language detection to Living Canvas commands.

**File: `extensions/living-canvas/language-detection.lisp`**

```lisp
(defpackage :lem-living-canvas/language
  (:use :cl :lem)
  (:export #:detect-language
           #:language-for-buffer
           #:language-for-file))
(in-package :lem-living-canvas/language)

(defparameter *mode-language-map*
  '((lem-python-mode:python-mode . :python)
    (lem-js-mode:js-mode . :javascript)
    (lem-typescript-mode:typescript-mode . :typescript)
    (lem-lisp-mode:lisp-mode . :common-lisp)))

(defparameter *extension-language-map*
  '(("py" . :python)
    ("pyw" . :python)
    ("js" . :javascript)
    ("mjs" . :javascript)
    ("jsx" . :javascript)
    ("ts" . :typescript)
    ("tsx" . :typescript)
    ("lisp" . :common-lisp)
    ("cl" . :common-lisp)
    ("asd" . :common-lisp)))

(defun language-for-buffer (buffer)
  "Detect language for BUFFER based on major mode."
  (let ((mode (buffer-major-mode buffer)))
    (cdr (assoc mode *mode-language-map* :test #'eq))))

(defun language-for-file (pathname)
  "Detect language for PATHNAME based on extension."
  (let ((ext (pathname-type pathname)))
    (when ext
      (cdr (assoc ext *extension-language-map* :test #'string-equal)))))

(defun detect-language (source)
  "Detect programming language for SOURCE (buffer or pathname)."
  (etypecase source
    (buffer (or (language-for-buffer source)
                (when-let ((file (buffer-filename source)))
                  (language-for-file file))))
    (pathname (language-for-file source))
    (string :unknown)))
```

### Step 3: Tree-Sitter Python Provider (1 file, ~200 LOC)

Implement the Python provider using tree-sitter.

**File: `extensions/living-canvas/python-provider.lisp`**

```lisp
(defpackage :lem-living-canvas/python
  (:use :cl :lem :lem-call-graph)
  (:export #:tree-sitter-python-provider))
(in-package :lem-living-canvas/python)

(defclass tree-sitter-python-provider (call-graph-provider)
  ((definition-query :accessor provider-definition-query)
   (call-query :accessor provider-call-query))
  (:documentation "Python call graph provider using tree-sitter."))

(defmethod initialize-instance :after ((provider tree-sitter-python-provider) &key)
  (ts:load-language-from-system "python")
  (setf (provider-definition-query provider)
        (ts:query-compile (ts:get-language "python")
                          "(function_definition name: (identifier) @name) @function
                           (class_definition name: (identifier) @name) @class"))
  (setf (provider-call-query provider)
        (ts:query-compile (ts:get-language "python")
                          "(call function: (identifier) @call)
                           (call function: (attribute attribute: (identifier) @method))")))

(defmethod provider-name ((p tree-sitter-python-provider)) :tree-sitter-python)
(defmethod provider-priority ((p tree-sitter-python-provider)) 5)
(defmethod provider-languages ((p tree-sitter-python-provider)) '(:python))

(defmethod provider-supports-p ((p tree-sitter-python-provider) source)
  (or (and (typep source 'buffer)
           (eq (buffer-major-mode source) 'lem-python-mode:python-mode))
      (and (pathnamep source)
           (string-equal "py" (pathname-type source)))))

(defmethod provider-analyze ((p tree-sitter-python-provider) source &key (type :file))
  (declare (ignore type))
  (let* ((text (etypecase source
                 (buffer (buffer-text source))
                 (pathname (uiop:read-file-string source))))
         (filepath (etypecase source
                     (buffer (buffer-filename source))
                     (pathname source)))
         (tree (ts:with-parser (parser "python")
                 (ts:parser-parse-string parser text)))
         (root (ts:tree-root-node tree))
         (nodes (make-hash-table :test 'equal))
         (edges nil))
    ;; Extract definitions
    (dolist (cap (ts:query-captures (provider-definition-query p) root))
      (when (member (ts:capture-name cap) '("function" "class") :test #'string=)
        (let* ((node (ts:capture-node cap))
               (name-node (ts:node-child-by-field-name node "name"))
               (name (subseq text
                             (ts:node-start-byte name-node)
                             (ts:node-end-byte name-node)))
               (start-point (ts:node-start-point node))
               (graph-node (make-graph-node
                            :id name
                            :name name
                            :type (if (string= (ts:capture-name cap) "class")
                                      :class :function)
                            :source-location (cons filepath (1+ (car start-point)))
                            :source-file filepath)))
          (setf (gethash name nodes) graph-node))))
    ;; Extract calls
    (dolist (cap (ts:query-captures (provider-call-query p) root))
      (let* ((call-node (ts:capture-node cap))
             (callee-name (subseq text
                                  (ts:node-start-byte call-node)
                                  (ts:node-end-byte call-node)))
             (caller (find-enclosing-function root call-node nodes text)))
        (when (and caller (gethash callee-name nodes))
          (push (make-graph-edge
                 :source (graph-node-id caller)
                 :target callee-name
                 :call-type :direct)
                edges))))
    (make-call-graph :nodes nodes :edges edges)))

(defun find-enclosing-function (root call-node nodes text)
  "Find the function containing CALL-NODE."
  (let ((parent (ts:node-parent call-node)))
    (loop :while parent
          :do (when (string= "function_definition" (ts:node-type parent))
                (let* ((name-node (ts:node-child-by-field-name parent "name"))
                       (name (subseq text
                                     (ts:node-start-byte name-node)
                                     (ts:node-end-byte name-node))))
                  (return (gethash name nodes))))
          :do (setf parent (ts:node-parent parent)))))

;; Auto-register on load
(lem-call-graph/registry:register-provider
 lem-call-graph/registry:*provider-registry*
 (make-instance 'tree-sitter-python-provider)
 '(:python))
```

### Step 4: Update Living Canvas Commands (modify existing, ~50 LOC changes)

Modify Living Canvas commands to use provider registry.

**Modify: `extensions/living-canvas/living-canvas.lisp`**

```lisp
;; Add to imports
(:use ... :lem-call-graph/registry :lem-living-canvas/language)

;; Modify living-canvas-current-file command
(define-command living-canvas-current-file () ()
  "Display call graph for current file."
  (let* ((buffer (current-buffer))
         (language (detect-language buffer))
         (provider (find-provider *provider-registry* language buffer)))
    (cond
      (provider
       (let ((graph (provider-analyze provider buffer :type :buffer)))
         (display-call-graph graph)))
      ((eq language :common-lisp)
       ;; Fallback to existing micros provider
       (living-canvas-current-file-cl))
      (t
       (editor-error "No provider available for ~A" language)))))

;; Add helper for backward compatibility
(defun living-canvas-current-file-cl ()
  "Original Common Lisp implementation using micros."
  ;; ... existing implementation ...
  )
```

### Step 5: JavaScript/TypeScript Provider (1 file, ~180 LOC)

Similar structure to Python provider.

**File: `extensions/living-canvas/js-provider.lisp`**

```lisp
(defpackage :lem-living-canvas/javascript
  (:use :cl :lem :lem-call-graph)
  (:export #:tree-sitter-js-provider))
(in-package :lem-living-canvas/javascript)

(defclass tree-sitter-js-provider (call-graph-provider)
  ((definition-query :accessor provider-definition-query)
   (call-query :accessor provider-call-query)))

;; Similar implementation to Python provider
;; See contracts/tree-sitter-queries.md for JS query patterns
```

### Step 6: Update System Definition

**Modify: `extensions/living-canvas/lem-living-canvas.asd`**

```lisp
(defsystem "lem-living-canvas"
  :depends-on ("lem/core"
               "lem-call-graph"
               "lem-tree-sitter"  ; Add
               "lem-lisp-mode"
               "lem-python-mode"  ; Add
               "lem-js-mode")     ; Add
  :serial t
  :components ((:file "package")
               (:file "language-detection")  ; Add
               (:file "python-provider")     ; Add
               (:file "js-provider")         ; Add
               (:file "buffer")
               (:file "living-canvas")
               (:file "micros-cl-provider")))
```

### Step 7: Write Tests

**File: `extensions/living-canvas/tests/python-provider-test.lisp`**

```lisp
(defpackage :lem-living-canvas/tests/python
  (:use :cl :rove :lem :lem-call-graph :lem-living-canvas/python))
(in-package :lem-living-canvas/tests/python)

(deftest test-python-provider-supports
  (let ((provider (make-instance 'tree-sitter-python-provider)))
    (ok (provider-supports-p provider #P"test.py"))
    (ng (provider-supports-p provider #P"test.js"))))

(deftest test-python-extract-functions
  (let* ((provider (make-instance 'tree-sitter-python-provider))
         (code "def foo():
                    pass

                def bar():
                    foo()")
         (graph (provider-analyze provider code :type :file)))
    (ok (= 2 (hash-table-count (call-graph-nodes graph))))
    (ok (gethash "foo" (call-graph-nodes graph)))
    (ok (gethash "bar" (call-graph-nodes graph)))
    (ok (= 1 (length (call-graph-edges graph))))))
```

## Testing Checklist

### Unit Tests
- [ ] Provider registry operations
- [ ] Language detection (buffer mode, file extension)
- [ ] Python definition extraction
- [ ] Python call extraction
- [ ] JavaScript definition extraction
- [ ] JavaScript call extraction

### Integration Tests
- [ ] Python file → graph display
- [ ] JavaScript file → graph display
- [ ] TypeScript file → graph display
- [ ] Common Lisp still works (regression)
- [ ] Source jump from graph node

### Manual Testing
```bash
# Run Lem
make sdl2

# Open Python file
# M-x living-canvas-current-file
# Verify graph displays

# Open JavaScript file
# M-x living-canvas-current-file
# Verify graph displays

# Open Common Lisp file
# M-x living-canvas-current-file
# Verify existing functionality works
```

## File Summary

| File | Purpose | Lines |
|------|---------|-------|
| `call-graph/registry.lisp` | Provider registry | ~80 |
| `living-canvas/language-detection.lisp` | Language detection | ~50 |
| `living-canvas/python-provider.lisp` | Python provider | ~200 |
| `living-canvas/js-provider.lisp` | JS/TS provider | ~180 |
| `living-canvas/living-canvas.lisp` | Command updates | ~50 (changes) |
| `living-canvas/tests/*.lisp` | Test files | ~200 |

**Total new code**: ~760 lines

## Common Issues

### tree-sitter Grammar Not Found

```lisp
;; Error: Language "python" not found
;; Solution: Ensure grammar is installed
(ts:load-language-from-system "python")
;; Check LD_LIBRARY_PATH or use Nix environment
```

### Provider Not Selecting

```lisp
;; Debug provider selection
(find-provider *provider-registry* :python)
;; Check if provider is registered
(list-providers *provider-registry*)
```

### Graph Empty

```lisp
;; Debug query execution
(let* ((tree (ts:parser-parse-string parser text))
       (captures (ts:query-captures query (ts:tree-root-node tree))))
  (format t "Captures: ~A~%" (length captures)))
```

## Next Steps After Implementation

1. **Phase 2**: Add LSP-based providers for enhanced cross-file analysis
2. **Phase 2**: Add execution tracing for dynamic call graphs
3. **Optimization**: Cache parsed trees and graphs
4. **UI Enhancement**: Language-specific node styling
