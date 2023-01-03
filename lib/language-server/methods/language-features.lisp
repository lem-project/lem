(in-package :lem-language-server)

(defvar *safe-package*
  (let ((package (make-package :lem-language-server-internal-package :use '())))
    (import '(nil t quote) package)
    package))

(defun scan-current-package (point &optional (default "COMMON-LISP-USER"))
  (lem:with-point ((p point))
    (loop
      (ppcre:register-groups-bind (package-name)
          ("^\\s*\\(\\s*(?:cl:|common-lisp:)?in-package (?:#?:|')?([^\)\\s]*)\\s*\\)"
           (string-downcase (lem:line-string p)))
        (return package-name))
      (unless (lem:line-offset p -1)
        (return default)))))

(defun safe-read-from-string (string &optional (package *safe-package*))
  (let ((*read-eval* nil)
        (*read-suppress* nil))
    (uiop:safe-read-from-string string :package package)))

(defun find-package-or (package-name &optional (default "COMMON-LISP-USER"))
  (or (find-package (safe-read-from-string package-name))
      (find-package default)))

(defun describe-symbol-at-point (point)
  (let ((package-name (scan-current-package point))
        (symbol-string (lem:symbol-string-at-point point)))
    (describe-symbol symbol-string package-name)))

(define-request (hover "textDocument/hover") (params lsp:hover-params)
  (let* ((point (convert-to-point params))
         (text (or (describe-symbol-at-point point) "")))
    (convert-to-json (make-instance 'lsp:hover :contents text))))
