(in-package :cl-tree-sitter/parser)

;;;; Parser Creation and Management

(defun make-parser (&optional language)
  "Create a new parser, optionally setting its language."
  (ffi:ensure-tree-sitter-loaded)
  (let* ((ptr (ffi:ts-parser-new))
         (parser (make-instance 'types:ts-parser :ptr ptr)))
    (when language
      (parser-set-language parser language))
    parser))

(defun parser-set-language (parser language)
  "Set the language for a parser.
   LANGUAGE can be a ts-language object or a language name string."
  (let ((lang (etypecase language
                (types:ts-language language)
                (string (cl-tree-sitter/language:get-language language)))))
    (unless lang
      (error "Language not found: ~A" language))
    (unless (ffi:ts-parser-set-language (types:ts-parser-ptr parser)
                                        (types:ts-language-ptr lang))
      (error "Failed to set language for parser"))
    (setf (types:ts-parser-language parser) lang)
    parser))

(defun parser-parse-string (parser string &optional old-tree)
  "Parse a string and return a syntax tree.
   If OLD-TREE is provided, it will be used for incremental parsing."
  (let* ((old-tree-ptr (if old-tree
                           (types:ts-tree-ptr old-tree)
                           (cffi:null-pointer)))
         (tree-ptr (ffi:ts-parser-parse-string (types:ts-parser-ptr parser)
                                               old-tree-ptr
                                               string)))
    (if (cffi:null-pointer-p tree-ptr)
        nil
        (make-instance 'types:ts-tree
                       :ptr tree-ptr
                       :source string))))

(defun parser-parse (parser read-fn old-tree)
  "Parse using a read function for input.
   READ-FN: (byte-offset point) -> string or nil
   This is for parsing buffers or streams."
  ;; For now, use parse-string. Full input API requires TSInput struct.
  (declare (ignore read-fn old-tree))
  (error "parser-parse with read function not yet implemented"))

(defun parser-reset (parser)
  "Reset the parser state."
  (ffi:ts-parser-reset (types:ts-parser-ptr parser))
  parser)

(defun parser-delete (parser)
  "Explicitly delete a parser (normally handled by GC)."
  (trivial-garbage:cancel-finalization parser)
  (ffi:ts-parser-delete (types:ts-parser-ptr parser)))

(defmacro with-parser ((var &optional language) &body body)
  "Execute BODY with a parser bound to VAR.
   The parser is automatically deleted on exit."
  `(let ((,var (make-parser ,language)))
     (unwind-protect
          (progn ,@body)
       (parser-delete ,var))))
