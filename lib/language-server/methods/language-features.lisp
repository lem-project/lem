(in-package :lem-language-server)

(defun in-package-line-p (line)
  (ppcre:register-groups-bind (package-name)
      ("^\\s*\\(\\s*(?:cl:|common-lisp:)?in-package (?:#?:|')?([^\)\\s]*)\\s*\\)"
       (string-downcase line))
    package-name))

(defun buffer-package (buffer)
  (lem:with-point ((point (lem:buffer-start-point buffer)))
    (loop :when (in-package-line-p (lem:line-string point))
          :return :it
          :while (lem:line-offset point 1))))

(defun scan-current-package (point &optional (default "COMMON-LISP-USER"))
  (lem:with-point ((p point))
    (loop
      (let ((package-name (in-package-line-p (lem:line-string p))))
        (when package-name
          (return package-name)))
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
     (lem:move-to-bytes point (1+ position)))
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

(define-request (go-to-definition-request "textDocument/definition") (params lsp:definition-params)
  (let* ((point (text-document-position-params-to-point params))
         (definitions (definitions-at-point point))
         (definition-points (collect-points-from-definitions definitions)))
    (when-let ((point (lem-lisp-syntax:search-local-definition
                       point
                       (lem:symbol-string-at-point point))))
      (push point definition-points))
    (convert-to-json (map 'vector #'point-to-lsp-location definition-points))))

(defun find-references-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (micros/client:remote-eval-sync (server-backend-connection *server*)
                                    `(micros:xrefs '(:calls :macroexpands :binds
                                                     :references :sets :specializes)
                                                   ,symbol-string)
                                    :package-name package-name)))

(define-request (find-references-request "textDocument/references") (params lsp:reference-params)
  (let* ((point (text-document-position-params-to-point params))
         (result (find-references-at-point point)))
    (convert-to-json
     (map 'vector
          #'point-to-lsp-location
          (loop :for (type . definitions) :in result
                :append (collect-points-from-definitions definitions))))))

(defun hover-at-point (point)
  (when-let* ((package-name (scan-current-package point))
              (symbol-string (lem:symbol-string-at-point point)))
    (micros/client:remote-eval-sync (server-backend-connection *server*)
                                    `(micros/lsp-api:hover-symbol ,symbol-string)
                                    :package-name package-name)))

(define-request (hover-request "textDocument/hover") (params lsp:hover-params)
  (let* ((point (text-document-position-params-to-point params))
         (text (or (hover-at-point point) "")))
    (convert-to-json (make-instance 'lsp:hover :contents text))))

(define-request (document-highlight-request "textDocument/documentHighlight")
    (params lsp:document-highlight-params)
  (let* ((point (text-document-position-params-to-point params))
         (symbol-string (lem:symbol-string-at-point point))
         (buffer (lem:point-buffer point)))
    (lem:with-point ((point (lem:buffer-point buffer)))
      (lem:buffer-start point)
      (convert-to-json
       (coerce (loop :while (lem:search-forward-symbol point symbol-string)
                     :collect (lem:with-point ((start point))
                                (lem:search-backward-symbol start symbol-string)
                                (make-instance 'lsp:document-highlight
                                               :kind lsp:document-highlight-kind-text
                                               :range (points-to-lsp-range start point))))
               'vector)))))

(defun signature-help-at-point (point)
  (when (and (backward-up-list point)
             (forward-down-list point))
    (let* ((symbol-string (lem:symbol-string-at-point point))
           (package-name (scan-current-package point))
           (arglist
             (micros/client:remote-eval-sync (server-backend-connection *server*)
                                             `(micros:operator-arglist ,symbol-string ,package-name))))
      (when arglist
        (make-instance 'lsp:signature-help
                       :signatures (vector (make-instance 'lsp:signature-information
                                                          :label (string-downcase arglist))))))))

(define-request (signature-help-request "textDocument/signatureHelp")
    (params lsp:signature-help-params)
  (let ((point (text-document-position-params-to-point params)))
    (if-let (signature-help (signature-help-at-point point))
      (convert-to-json signature-help)
      :null)))

(defun make-text-edit (point string)
  (lem:with-point ((start point)
                   (end point))
    (lem:skip-symbol-backward start)
    (lem:skip-symbol-forward end)
    (make-instance 'lsp:text-edit
                   :new-text string
                   :range (points-to-lsp-range start end))))

(define-request (completion-request "textDocument/completion") (params lsp:completion-params)
  (let* ((point (text-document-position-params-to-point params))
         (symbol-string (lem:symbol-string-at-point point))
         (package-name (scan-current-package point)))
    (if (null symbol-string)
        :null
        (convert-to-json
         (map 'vector
              (lambda (completed-item)
                (destructuring-bind (label classification signature documentation)
                    completed-item
                  (make-instance 'lsp:completion-item
                                 :label label
                                 :label-details (make-instance 'lsp:completion-item-label-details
                                                               ;; :description "" ; TODO: set value
                                                               :detail (if signature
                                                                           (format nil " ~A" signature)
                                                                           ""))
                                 :detail classification
                                 :text-edit (make-text-edit point label)
                                 :documentation (make-instance 'lsp:markup-content
                                                               :kind lsp:markup-kind-markdown
                                                               :value documentation))))
              (micros/client:remote-eval-sync (server-backend-connection *server*)
                                              `(micros/lsp-api:completions ,symbol-string
                                                                           ,package-name)))))))

(defstruct symbol-definition
  symbol-spec
  range
  line-string)

(defun collect-definition-symbols-in-buffer (buffer default-package-name)
  (lem:with-point ((point (lem:buffer-point buffer)))
    (lem:buffer-start point)
    (loop :while (lem:search-forward-regexp point "^\\s*\\(def")
          :when (cond ((lem:in-comment-p point)
                       (lem:maybe-beginning-of-comment point)
                       (lem:skip-space-and-comment-forward point))
                      ((lem:in-string-p point)
                       (lem:maybe-beginning-of-string point)
                       (lem:form-offset point 1))
                      (t
                       (when (and (lem:form-offset point 1)
                                  (lem:skip-space-and-comment-forward point)
                                  (or (lem:syntax-symbol-char-p (lem:character-at point))
                                      (forward-down-list point)))
                         (let ((symbol-string (lem:symbol-string-at-point point)))
                           (multiple-value-bind (symbol-name package-name internalp)
                               (micros::tokenize-symbol-thoroughly symbol-string)
                             (declare (ignore internalp))
                             (when symbol-name
                               (cond ((equal package-name "") nil) ; keyword
                                     ((equal package-name "#") nil) ; uninternal symbol
                                     (t
                                      (lem:with-point ((start point)
                                                       (end point))
                                        (lem:skip-symbol-forward start)
                                        (lem:skip-symbol-forward end)
                                        (make-symbol-definition
                                         :symbol-spec (micros/lsp-api:make-symbol-spec
                                                       :name symbol-name
                                                       :package (or package-name default-package-name))
                                         :range (points-to-lsp-range start end)
                                         :line-string (lem:line-string point)))))))))))
          :collect :it)))

(defun document-symbol (buffer)
  ;; in-packageがファイル先頭に一つだけあることを前提にしている
  (let* ((package-name (buffer-package buffer))
         (symbol-definitions (collect-definition-symbols-in-buffer buffer package-name)))
    (convert-to-json
     (map 'vector
          (lambda (symbol-information symbol-definition)
            (let ((range (symbol-definition-range symbol-definition))
                  (line-string (symbol-definition-line-string symbol-definition))
                  (name (micros/lsp-api::symbol-information-name symbol-information))
                  ;; (detail (micros/lsp-api::symbol-information-detail symbol-information))
                  (kind (micros/lsp-api::symbol-information-kind symbol-information)))
              ;; symbolに複数の束縛があることを考慮していない(variableとfunctionなど)
              (make-instance 'lsp:document-symbol
                             :name name
                             :detail line-string ;(or detail "")
                             :kind (case kind
                                     (:variable
                                      lsp:symbol-kind-variable)
                                     (:function
                                      lsp:symbol-kind-function)
                                     (:class
                                      lsp:symbol-kind-class)
                                     (:package
                                      lsp:symbol-kind-package)
                                     (otherwise
                                      lsp:symbol-kind-null))
                             :range range
                             :selection-range range)))
          (micros/client:remote-eval-sync (server-backend-connection *server*)
                                          `(micros/lsp-api:symbol-informations
                                            ',(mapcar #'symbol-definition-symbol-spec symbol-definitions))
                                          :package-name package-name)
          symbol-definitions))))

(define-request (document-symbol-request "textDocument/documentSymbol")
    (params lsp:document-symbol-params)
  (let* ((text-document-identifier (lsp:document-symbol-params-text-document params))
         (text-document (find-text-document text-document-identifier)))
    (document-symbol (text-document-buffer text-document))))
