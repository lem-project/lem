(uiop:define-package :lem-tree-sitter/indent
  (:use :cl)
  (:local-nicknames (:ts :tree-sitter))
  (:export
   #:load-indent-query
   #:calc-indent-from-query))
(in-package :lem-tree-sitter/indent)

;;; Indent Query Loading

(defun load-indent-query (language-name query-path)
  "Load and compile an indent query from QUERY-PATH for LANGUAGE-NAME.
Returns the compiled query or NIL if loading fails."
  (handler-case
      (let ((language (ts:get-language language-name))
            (source (uiop:read-file-string query-path)))
        (ts:query-compile language source))
    (error (e)
      (log:debug "Failed to load indent query: ~A" e)
      nil)))

;;; Indent Calculation

(defstruct indent-capture
  "Represents an indent-related capture from a tree-sitter query."
  (type :indent :type (member :indent :outdent))
  (node nil)
  (start-row 0 :type fixnum)
  (start-column 0 :type fixnum)
  (end-row 0 :type fixnum)
  (end-column 0 :type fixnum))

(defun parse-capture-type (capture-name)
  "Parse capture name to indent type.
Returns :INDENT for @indent, :OUTDENT for @outdent, or NIL for unknown."
  (cond
    ((string= capture-name "indent") :indent)
    ((string= capture-name "outdent") :outdent)
    (t nil)))

(defun collect-indent-captures (query node)
  "Collect all indent-related captures from QUERY matches on NODE.
Returns a list of INDENT-CAPTURE structures."
  (let ((captures nil))
    (dolist (capture (ts:query-captures query node))
      (let* ((name (ts:capture-name capture))
             (type (parse-capture-type name)))
        (when type
          (let* ((captured-node (ts:capture-node capture))
                 (start-point (ts:node-start-point captured-node))
                 (end-point (ts:node-end-point captured-node))
                 (start-row (ts:ts-point-row start-point)))
            (push (make-indent-capture :type type
                                       :node captured-node
                                       :start-row start-row
                                       :start-column (ts:ts-point-column start-point)
                                       :end-row (ts:ts-point-row end-point)
                                       :end-column (ts:ts-point-column end-point))
                  captures)))))
    (nreverse captures)))

(defun compute-indent-level (captures current-row &optional (line-start-column 0))
  "Compute net indent level from CAPTURES for CURRENT-ROW.

Applies Helix-style scope rules:
- @indent (scope: tail): only applies if node starts before current row and
  still covers it, i.e. the current row is inside the node
- @outdent: applies when the node starts the current line (at LINE-START-COLUMN,
  the first non-blank column), e.g. a closing bracket or an else clause

Returns the net indent level (can be negative)."
  (let ((indent-count 0)
        (outdent-count 0))
    (dolist (capture captures)
      (let ((capture-start-row (indent-capture-start-row capture)))
        (ecase (indent-capture-type capture)
          (:indent
           ;; scope: tail - the node starts on a previous line and the current
           ;; line is still inside it (a node ending at column 0 of a row does
           ;; not cover that row)
           (when (and (< capture-start-row current-row)
                      (or (> (indent-capture-end-row capture) current-row)
                          (and (= (indent-capture-end-row capture) current-row)
                               (> (indent-capture-end-column capture) 0))))
             (incf indent-count)))
          (:outdent
           ;; only a node that begins the line outdents it: a closing bracket
           ;; at the start of the line does, one inside the line does not
           (when (and (= capture-start-row current-row)
                      (= (indent-capture-start-column capture) line-start-column))
             (incf outdent-count))))))
    (- indent-count outdent-count)))

(defun calc-indent-from-query (tree query point indent-size)
  "Calculate indentation for POINT using tree-sitter indent QUERY.

TREE is the tree-sitter syntax tree.
QUERY is the compiled indent query.
POINT is the Lem point at the start of the line to indent.
INDENT-SIZE is the number of spaces per indent level.

Returns the calculated indentation in spaces, or NIL if calculation fails."
  (when (and tree query)
    (handler-case
        (let* ((root (ts:tree-root-node tree))
               ;; Get current row (0-indexed)
               (current-row (1- (lem:line-number-at-point point)))
               ;; Get all captures from the entire tree
               (captures (collect-indent-captures query root))
               ;; Filter captures relevant to the current line's ancestors
               ;; For now, we use all captures up to and including current row
               (relevant-captures
                 (remove-if (lambda (cap)
                              (> (indent-capture-start-row cap) current-row))
                            captures))
               (line-start-column (lem:with-point ((p point))
                                    (lem:back-to-indentation p)
                                    (lem:point-column p)))
               ;; Compute indent level
               (indent-level (compute-indent-level relevant-captures current-row line-start-column)))
          ;; Return indent in spaces, minimum 0
          (max 0 (* indent-level indent-size)))
      (error (e)
        (log:debug "Indent query error: ~A" e)
        nil))))
