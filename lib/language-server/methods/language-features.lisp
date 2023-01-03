(in-package :lem-language-server)

(defun scan-current-package (point &optional (default "COMMON-LISP-USER"))
  (lem:with-point ((p point))
    (loop
      (ppcre:register-groups-bind (package-name)
          ("^\\s*\\(\\s*(?:cl:|common-lisp:)?in-package (?:#?:|')?([^\)\\s]*)\\s*\\)"
           (string-downcase (lem:line-string p)))
        (return package-name))
      (unless (lem:line-offset p -1)
        (return default)))))

(defun describe-symbol-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (describe-symbol symbol-string package-name)))

(define-request (hover "textDocument/hover") (params lsp:hover-params)
  (let* ((point (convert-to-point params))
         (text (or (describe-symbol-at-point point) "")))
    (convert-to-json (make-instance 'lsp:hover :contents text))))
