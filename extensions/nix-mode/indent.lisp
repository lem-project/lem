(defpackage :lem-nix-mode/indent
  (:use :cl :lem :lem/language-mode)
  (:local-nicknames (:ts :tree-sitter)
                    (:ts-ext :lem-tree-sitter)
                    (:ts-indent :lem-tree-sitter/indent))
  (:export :*indent-size*
           :calc-indent))
(in-package :lem-nix-mode/indent)

(defparameter *indent-size* 2
  "Default indentation size for Nix code.")

;;; Tree-sitter based indentation

(defparameter *nix-indent-node-types*
  '("attrset_expression" "rec_attrset_expression"
    "list_expression" "let_expression" "with_expression"
    "if_expression" "function_expression" "parenthesized_expression"
    "formals"  ; function parameter list { ... }
    "indented_string_expression"  ; multiline strings '' ... ''
    ;; Also handle simple node type names
    "attrset" "rec_attrset" "list" "let" "with" "if" "function")
  "Node types that increase indentation in Nix.")

(defparameter *nix-closer-node-types*
  '("}" "]" ")" "in")
  "Node types that are closers and should dedent.")

;;; Tree-sitter integration

(defun get-buffer-treesitter-parser (buffer)
  "Get the treesitter-parser for BUFFER, if any."
  (ts-ext:get-buffer-treesitter-parser buffer))

(defun treesitter-parser-tree (parser)
  "Get the syntax tree from a treesitter parser."
  (when parser
    (ts-ext:treesitter-parser-tree parser)))

(defun treesitter-parser-indent-query (parser)
  "Get the indent query from a treesitter parser."
  (when parser
    (ts-ext:treesitter-parser-indent-query parser)))

(defun tree-sitter-available-for-buffer-p (buffer)
  "Check if tree-sitter is available and has a valid tree for BUFFER."
  (let ((parser (get-buffer-treesitter-parser buffer)))
    (and parser (treesitter-parser-tree parser))))

(defun get-node-at-byte (buffer byte-offset &key (named t))
  "Get the tree-sitter node at BYTE-OFFSET in BUFFER.
If NAMED is T (default), returns only named nodes.
If NAMED is NIL, returns any node including anonymous ones.
Uses tree cursor to find the deepest node at the given byte offset."
  (let ((parser (get-buffer-treesitter-parser buffer)))
    (when parser
      (alexandria:when-let ((tree (treesitter-parser-tree parser)))
        (let ((root (ts:tree-root-node tree)))
          ;; Walk down from root to find deepest node containing byte-offset
          (labels ((find-node (node)
                     (let ((start (ts:node-start-byte node))
                           (end (ts:node-end-byte node)))
                       (when (and (<= start byte-offset) (< byte-offset end))
                         ;; Check children for a more specific match
                         (let ((children (if named
                                             (ts:node-named-children node)
                                             (ts:node-children node))))
                           (or (some #'find-node children)
                               (when (or (not named) (ts:node-named-p node))
                                 node)))))))
            (find-node root)))))))

(defun node-indent-contributing-p (node-type)
  "Check if NODE-TYPE contributes to indentation."
  (member node-type *nix-indent-node-types* :test #'string=))

(defun count-indent-depth (node &optional skip-first)
  "Count the indentation depth by walking up parent nodes.
If SKIP-FIRST is T, don't count NODE itself (used when on the opening line of a container)."
  (let ((depth 0)
        (first t))
    (loop :for current := node :then (ts:node-parent current)
          :while current
          :do (let ((node-type (ts:node-type current)))
                (when (node-indent-contributing-p node-type)
                  (if (and first skip-first)
                      (setf first nil)
                      ;; indented_string_expression contributes 2 levels
                      (if (string= node-type "indented_string_expression")
                          (incf depth 2)
                          (incf depth)))))
              (setf first nil))
    depth))

(defun get-node-at-line-start (point)
  "Get the node at the start of the line (first non-whitespace position)."
  (with-point ((p point))
    (back-to-indentation p)
    (let* ((buffer (point-buffer p))
           (byte-offset (point-bytes p)))
      (get-node-at-byte buffer byte-offset))))

(defun get-line-indent-at-row (buffer row)
  "Get the indentation of line at ROW (0-indexed) in BUFFER."
  (with-point ((p (buffer-point buffer)))
    (buffer-start p)
    (move-to-line p (1+ row))  ; move-to-line is 1-indexed
    (back-to-indentation p)
    (point-column p)))

(defun get-node-line-indent (buffer node)
  "Get the indentation of the line where NODE starts in BUFFER."
  (let* ((start-point (ts:node-start-point node))
         (row (ts:ts-point-row start-point)))
    (get-line-indent-at-row buffer row)))

(defun get-parent-container-indent (buffer node)
  "Get the indentation of the line where the parent container starts."
  (loop :for parent := (ts:node-parent node) :then (ts:node-parent parent)
        :while parent
        :when (node-indent-contributing-p (ts:node-type parent))
        :do (return (get-node-line-indent buffer parent))
        :finally (return 0)))

(defun current-line-starts-with-closer-ts-p (point)
  "Check if current line starts with a closing token using tree-sitter."
  (alexandria:when-let ((node (get-node-at-line-start point)))
    (member (ts:node-type node) *nix-closer-node-types* :test #'string=)))

(defun find-parent-of-type (node types)
  "Find the first ancestor of NODE with a type in TYPES list."
  (loop :for parent := (ts:node-parent node) :then (ts:node-parent parent)
        :while parent
        :when (member (ts:node-type parent) types :test #'string=)
        :do (return parent)))

(defun is-in-binding-set-p (node)
  "Check if NODE is inside a binding_set or IS a binding_set."
  ;; First check if node itself is binding_set
  (when (string= (ts:node-type node) "binding_set")
    (return-from is-in-binding-set-p t))
  ;; Then check ancestors
  (loop :for parent := (ts:node-parent node) :then (ts:node-parent parent)
        :while parent
        :when (string= (ts:node-type parent) "binding_set")
        :do (return t)
        :when (string= (ts:node-type parent) "let_expression")
        :do (return nil)))  ; Found let before binding_set means we're in body

(defun is-let-body-p (node)
  "Check if NODE is in the body part of a let_expression (after 'in')."
  ;; In Nix AST, let_expression has: binding_set, in keyword, then body
  ;; We're in the body if we're inside a let_expression but NOT inside its binding_set
  (let ((let-expr (find-parent-of-type node '("let_expression"))))
    (when let-expr
      ;; Check if we're NOT in the binding_set of this let
      (not (is-in-binding-set-p node)))))

(defun node-starts-at-line-p (node current-line-row)
  "Check if NODE starts at CURRENT-LINE-ROW (0-indexed)."
  (let ((start-point (ts:node-start-point node)))
    (= (ts:ts-point-row start-point) current-line-row)))

(defun calc-indent-tree-sitter (point)
  "Calculate indentation using tree-sitter."
  (let* ((buffer (point-buffer point))
         (parser (get-buffer-treesitter-parser buffer)))
    (unless (and parser (treesitter-parser-tree parser))
      (return-from calc-indent-tree-sitter nil))
    ;; Get node at the position before the current line's indentation
    (with-point ((p point))
      (back-to-indentation p)
      (let* ((byte-offset (point-bytes p))
             (current-line (1- (line-number-at-point p)))  ; 0-indexed row
             ;; First try to get any node (including anonymous like braces)
             (any-node (get-node-at-byte buffer byte-offset :named nil))
             (named-node (get-node-at-byte buffer byte-offset :named t)))
        (unless (or any-node named-node)
          (return-from calc-indent-tree-sitter nil))
        (let ((any-type (when any-node (ts:node-type any-node)))
              (named-type (when named-node (ts:node-type named-node))))
          (cond
            ;; Closing brace/bracket - align with the line where parent container starts
            ((and any-type (member any-type '("}" "]" ")") :test #'string=))
             (alexandria:when-let ((parent (ts:node-parent any-node)))
               (get-node-line-indent buffer parent)))
            ;; Closing '' of indented string - align with opening
            ((and named-type (string= named-type "indented_string_expression")
                  (not (node-starts-at-line-p named-node current-line)))
             ;; This is the closing line of the indented string
             (get-node-line-indent buffer named-node))
            ;; 'in' keyword - align with 'let'
            ((and any-type (string= any-type "in"))
             (alexandria:when-let ((let-expr (find-parent-of-type any-node '("let_expression" "let"))))
               (get-node-line-indent buffer let-expr)))
            ;; 'then' or 'else' - same level as 'if' (no extra indent)
            ((and any-type (member any-type '("then" "else") :test #'string=))
             (alexandria:when-let ((if-expr (find-parent-of-type any-node '("if_expression" "if"))))
               (get-node-line-indent buffer if-expr)))
            ;; Body of let expression (after 'in') - same level as 'let'
            ((and named-node (is-let-body-p named-node))
             (alexandria:when-let ((let-expr (find-parent-of-type named-node '("let_expression"))))
               (get-node-line-indent buffer let-expr)))
            ;; Normal content - count parent depth
            (named-node
             (let* ((node-is-container (node-indent-contributing-p named-type))
                    (on-opening-line (and node-is-container
                                          (node-starts-at-line-p named-node current-line)))
                    (depth (count-indent-depth named-node on-opening-line)))
               (* depth *indent-size*)))
            ;; Fallback
            (t nil)))))))

;;; Fallback heuristic-based indentation

(defun buffer-first-line-p (point)
  "Check if POINT is on the first line of the buffer."
  (with-point ((p point))
    (line-start p)
    (start-buffer-p p)))

(defun line-starts-with-multiline-string-delimiter-p (point)
  "Check if the line at POINT starts with '' (multiline string delimiter)."
  (with-point ((p point))
    (back-to-indentation p)
    (and (char= (character-at p 0) #\')
         (char= (character-at p 1) #\'))))

(defun find-matching-multiline-string-opener-indent (point)
  "Find the indentation of the opening '' for a closing '' at POINT."
  (with-point ((p point))
    (line-start p)
    (let ((count 1))  ; Start with 1 for the closing we're on
      ;; Go backwards, counting '' occurrences until we find the matching opener
      (loop
        (unless (line-offset p -1)
          (return-from find-matching-multiline-string-opener-indent nil))
        (with-point ((line-start-pt p) (line-end-pt p))
          (line-start line-start-pt)
          (line-end line-end-pt)
          ;; Count '' on this line
          (with-point ((scan-pt line-start-pt))
            (loop
              (unless (search-forward scan-pt "''" line-end-pt)
                (return))
              (incf count))))
        ;; If count becomes even, we found matching opener
        (when (evenp count)
          (return-from find-matching-multiline-string-opener-indent
            (get-line-indent p)))))))

(defun inside-multiline-string-p (point)
  "Check if POINT is inside a Nix multiline string ('' ... '').
Returns T if inside the string content (not on opening/closing lines)."
  (with-point ((p point))
    (line-start p)
    ;; If current line starts with '', it's the closing line, not inside
    (when (line-starts-with-multiline-string-delimiter-p p)
      (return-from inside-multiline-string-p nil))
    (let ((count 0)
          (current-line (line-number-at-point p)))
      ;; Go to buffer start and count '' occurrences before current line
      (buffer-start p)
      (loop
        (let ((line-num (line-number-at-point p)))
          ;; Stop when we reach the current line
          (when (>= line-num current-line)
            (return))
          ;; Search for '' on this line
          (with-point ((line-end-pt p))
            (line-end line-end-pt)
            (loop
              (unless (search-forward p "''" line-end-pt)
                (return))
              (incf count))))
        (unless (line-offset p 1)
          (return)))
      ;; If count is odd, we're inside a multiline string
      (oddp count))))

(defun line-empty-p (point)
  "Check if the line at POINT is empty or whitespace-only."
  (with-point ((p point))
    (line-start p)
    (skip-whitespace-forward p)  ; Don't skip across newlines
    (end-line-p p)))

(defun get-previous-nonblank-line-point (point)
  "Return a point at the start of the previous non-blank line, or NIL if none."
  (with-point ((p point))
    (loop
      (unless (line-offset p -1)
        (return nil))
      (unless (line-empty-p p)
        (return (copy-point p :temporary))))))

(defun get-line-indent (point)
  "Get the indentation column of the line at POINT."
  (with-point ((p point))
    (back-to-indentation p)
    (point-column p)))

(defun line-ends-with-opener-p (point)
  "Check if the line at POINT ends with an opening delimiter ({ [ ()."
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (when (> (point-charpos p) 0)
      (character-offset p -1)
      (member (character-at p) '(#\{ #\[ #\() :test #'char=))))

(defun line-ends-with-let-p (point)
  "Check if the line at POINT ends with 'let' keyword."
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (when (>= (point-charpos p) 3)
      (character-offset p -3)
      (looking-at p "let\\b"))))

(defun line-ends-with-equals-p (point)
  "Check if the line at POINT ends with '=' (attribute assignment)."
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (when (> (point-charpos p) 0)
      (character-offset p -1)
      (char= (character-at p) #\=))))

(defun line-ends-with-then-p (point)
  "Check if the line at POINT ends with 'then' keyword."
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (when (>= (point-charpos p) 4)
      (character-offset p -4)
      (looking-at p "then\\b"))))

(defun line-ends-with-lambda-after-equals-p (point)
  "Check if line ends with ':' (lambda) and has '=' earlier on the line.
This pattern like 'x = { ... }:' needs body indented."
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (when (and (> (point-charpos p) 0)
               (progn (character-offset p -1) (char= (character-at p) #\:)))
      ;; Check if there's an '=' earlier on the line
      (line-start p)
      (let ((line-end-pt (with-point ((e p)) (line-end e) e)))
        (search-forward p "=" line-end-pt)))))

(defun current-line-first-char (point)
  "Return the first non-whitespace character on the current line, or NIL."
  (with-point ((p point))
    (back-to-indentation p)
    (unless (end-line-p p)
      (character-at p))))

(defun current-line-starts-with-closer-p (point)
  "Check if the current line at POINT starts with a closing delimiter (} ] ))."
  (member (current-line-first-char point) '(#\} #\] #\)) :test #'eql))

(defun current-line-starts-with-keyword-p (point keyword)
  "Check if the current line at POINT starts with KEYWORD."
  (with-point ((p point))
    (back-to-indentation p)
    (looking-at p (format nil "~A\\b" keyword))))

(defun find-matching-opener-indent (point)
  "Find the indentation of the matching opening delimiter for a closer at POINT."
  (with-point ((p point))
    (back-to-indentation p)
    (when (scan-lists p -1 1 t)
      (get-line-indent p))))

(defun line-contains-keyword-p (point keyword)
  "Check if the line at POINT contains KEYWORD as a whole word."
  (with-point ((p point))
    (back-to-indentation p)
    (let ((line-end-point (with-point ((e p)) (line-end e) e)))
      (search-forward-regexp p (format nil "\\b~A\\b" keyword) line-end-point))))

(defun find-matching-keyword-indent (point open-keyword close-keyword)
  "Find the indentation of matching OPEN-KEYWORD for CLOSE-KEYWORD at POINT.
Searches for keywords anywhere on the line, not just at the start."
  (with-point ((p point))
    (let ((depth 1))
      (loop
        (unless (line-offset p -1)
          (return-from find-matching-keyword-indent nil))
        (cond
          ((line-contains-keyword-p p open-keyword)
           (decf depth)
           (when (zerop depth)
             (return-from find-matching-keyword-indent (get-line-indent p))))
          ((line-contains-keyword-p p close-keyword)
           (incf depth)))))))

(defun calc-indent-from-indent-query (point)
  "Calculate indentation using tree-sitter indent query.
Returns the indent level or NIL if indent query is not available."
  (let* ((buffer (point-buffer point))
         (parser (get-buffer-treesitter-parser buffer)))
    (when parser
      (alexandria:when-let ((tree (treesitter-parser-tree parser)))
        (alexandria:when-let ((indent-query (treesitter-parser-indent-query parser)))
          (ts-indent:calc-indent-from-query tree indent-query point *indent-size*))))))

(defun calc-indent (point)
  "Calculate indentation for POINT in Nix mode.
Uses tree-sitter indent query when available, then falls back to tree-sitter
traversal, and finally to heuristics."
  (line-start point)
  ;; First line always has indent 0
  (when (buffer-first-line-p point)
    (return-from calc-indent 0))
  ;; Try tree-sitter indent query first (preferred)
  (alexandria:when-let ((query-indent (calc-indent-from-indent-query point)))
    (return-from calc-indent query-indent))
  ;; Fallback 1: tree-sitter manual traversal
  (alexandria:when-let ((ts-indent (calc-indent-tree-sitter point)))
    (return-from calc-indent ts-indent))
  ;; Fallback 2: heuristic-based indentation
  (calc-indent-fallback point))

(defun calc-indent-fallback (point)
  "Fallback indentation calculation using heuristics."
  ;; Check for Nix multiline strings ('' ... '') before syntax-ppss
  (when (inside-multiline-string-p point)
    ;; Inside multiline string - add indent-size to current indent
    (return-from calc-indent-fallback (+ (get-line-indent point) *indent-size*)))
  (with-point-syntax point
    (let ((state (syntax-ppss point)))
      (cond
        ;; Inside a string - add indent-size to current indent
        ((pps-state-string-p state)
         (+ (get-line-indent point) *indent-size*))
        ;; Inside a block comment - maintain previous indent
        ((pps-state-comment-p state)
         (let ((prev-point (get-previous-nonblank-line-point point)))
           (if prev-point
               (get-line-indent prev-point)
               0)))
        ;; Calculate based on context
        (t (calc-indent-for-line point state))))))

(defun calc-indent-for-line (point state)
  "Calculate indentation for a normal line at POINT with parse STATE."
  (let ((paren-depth (pps-state-paren-depth state)))
    (cond
      ;; Line starts with closing multiline string delimiter - align with opener
      ((line-starts-with-multiline-string-delimiter-p point)
       (or (find-matching-multiline-string-opener-indent point) 0))
      ;; Line starts with closing delimiter - align with opener
      ((current-line-starts-with-closer-p point)
       (or (find-matching-opener-indent point) 0))
      ;; Line starts with 'in' - align with matching 'let'
      ((current-line-starts-with-keyword-p point "in")
       (or (find-matching-keyword-indent point "let" "in") 0))
      ;; Line starts with 'then' - at top-level same as if, inside blocks +indent
      ((current-line-starts-with-keyword-p point "then")
       (let ((if-indent (find-matching-keyword-indent point "if" "then")))
         (cond
           ((null if-indent) 0)
           ((> paren-depth 0) (+ if-indent *indent-size*))
           (t if-indent))))
      ;; Line starts with 'else' - at top-level same as if, inside blocks +indent
      ((current-line-starts-with-keyword-p point "else")
       (let ((if-indent (find-matching-keyword-indent point "if" "else")))
         (cond
           ((null if-indent) 0)
           ((> paren-depth 0) (+ if-indent *indent-size*))
           (t if-indent))))
      ;; Normal line - check previous line
      (t
       (let ((prev-point (get-previous-nonblank-line-point point)))
         (if (null prev-point)
             0
             (let ((prev-indent (get-line-indent prev-point)))
               (cond
                 ;; Previous line ends with opener - increase indent
                 ((line-ends-with-opener-p prev-point)
                  (+ prev-indent *indent-size*))
                 ;; Previous line ends with 'let' - increase indent
                 ((line-ends-with-let-p prev-point)
                  (+ prev-indent *indent-size*))
                 ;; Previous line ends with '=' - increase indent
                 ((line-ends-with-equals-p prev-point)
                  (+ prev-indent *indent-size*))
                 ;; Previous line ends with 'then' - increase indent
                 ((line-ends-with-then-p prev-point)
                  (+ prev-indent *indent-size*))
                 ;; Lambda like 'x = { ... }:' inside block - increase indent for body
                 ((and (line-ends-with-lambda-after-equals-p prev-point) (> paren-depth 0))
                  (+ prev-indent *indent-size*))
                 ;; Default: use max of paren depth and prev indent to preserve let/lambda nesting
                 ((> paren-depth 0)
                  (max (* paren-depth *indent-size*) prev-indent))
                 (t prev-indent)))))))))
