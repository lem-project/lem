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

(defun definitions-at-point (point)
  (when-let* ((package-name (scan-current-package point))
                (symbol-string (lem:symbol-string-at-point point)))
      (micros/client:remote-eval-sync (server-backend-connection *server*)
                                      `(micros:find-definitions-for-emacs ,symbol-string)
                                      :package-name package-name)))

(defun move-to-location-position (point location-position)
  (destructuring-ecase location-position
    ((:position position)
     (lem:move-to-bytes point position))
    ((:offset start offset)
     (lem:move-to-position point start)
     (lem:character-offset point offset))
    ((:line line-number &optional column)
     (lem:move-to-line point line-number)
     (if column
         (lem:line-offset point 0 column)
         (lem:back-to-indentation point)))
    ((:function-name name)
     (lem:buffer-start point)
     (lem:search-forward-regexp point
                                (ppcre:create-scanner
                                 `(:sequence
                                   "(def"
                                   (:greedy-repetition 1 nil (:char-class :word-char-class #\-))
                                   (:greedy-repetition 1 nil :whitespace-char-class)
                                   (:greedy-repetition 0 nil #\()
                                   ,name
                                   (:char-class :whitespace-char-class #\( #\)))
                                 :case-insensitive-mode t))
     (lem:form-offset point -1))
    ((:eof)
     (lem:buffer-end point))
    ;; maybe unused
    ((:method name specializers &rest qualifiers)
     (declare (ignore name specializers qualifiers)))
    ((:source-path source-path start-position)
     (declare (ignore source-path start-position)))))

(defun resolve-location-buffer (location-buffer)
  (destructuring-ecase location-buffer
    ((:file filename)
     (lem:find-file-buffer filename
                           :syntax-table lem-lisp-syntax:*syntax-table*
                           :temporary t))
    ;; maybe unused
    ((:buffer buffer-name)
     (declare (ignore buffer-name)))
    ((:buffer-and-file buffer filename)
     (declare (ignore buffer filename)))
    ((:source-form string)
     (declare (ignore string)))
    ((:zip file entry)
     (declare (ignore file entry)))))

(defun resolve-location (location)
  (destructuring-ecase location
    ((:location location-buffer position hints)
     (declare (ignore hints))
     (let ((buffer (resolve-location-buffer location-buffer)))
       (cond ((null buffer)
              (log:error "unresolve location" location)
              nil)
             (t
              (lem:with-point ((point (lem:buffer-point buffer)))
                (when (move-to-location-position point position)
                  point))))))
    ((:error message)
     ;; TODO: send message to client
     (log:debug message)
     nil)))

(defun collect-points-from-definitions (definitions)
  (loop :for (dspec location) :in definitions
        :when (resolve-location location)
        :collect :it))

(defun point-to-location (point)
  (let ((uri (pathname-to-uri (lem:buffer-filename (lem:point-buffer point)))))
    (lem:with-point ((end point))
      (lem:form-offset end 1)
      (make-instance 'lsp:location
                     :uri uri
                     :range (points-to-lsp-range point end)))))

(define-request (go-to-definition "textDocument/definition") (params lsp:definition-params)
  (let* ((point (convert-to-point params))
         (definitions (definitions-at-point point)))
    (convert-to-json
     (map 'vector #'point-to-location (collect-points-from-definitions definitions)))))

(defun hover-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (micros/client:remote-eval-sync (server-backend-connection *server*)
                                    `(micros/lsp-api:hover-symbol ,symbol-string)
                                    :package-name package-name)))

(define-request (hover "textDocument/hover") (params lsp:hover-params)
  (let* ((point (convert-to-point params))
         (text (or (hover-at-point point) "")))
    (convert-to-json (make-instance 'lsp:hover :contents text))))
