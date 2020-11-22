(defpackage :lem-lsp-mode/lsp-mode
  (:use :cl :lem :alexandria)
  (:import-from :lem-lsp-mode/json)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/protocol)
  (:import-from :lem-lsp-mode/request)
  (:import-from :lem-lsp-mode/client))
(in-package :lem-lsp-mode/lsp-mode)

(cl-package-locks:lock-package :lem-lsp-mode/lsp-mode)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-mode/protocol)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)
(lem-lsp-mode/project:local-nickname :request :lem-lsp-mode/request)
(lem-lsp-mode/project:local-nickname :json :lem-lsp-mode/json)
(lem-lsp-mode/project:local-nickname :client :lem-lsp-mode/client)
(lem-lsp-mode/project:local-nickname :completion :lem.completion-mode)

;;;
(defvar *language-id-server-info-map* (make-hash-table :test 'equal))

(defstruct server-info
  port
  process)

(defun server-process-buffer-name (spec)
  (format nil "*Lsp <~A>*" (spec-langauge-id spec)))

(defun make-server-process-buffer (spec)
  (make-buffer (server-process-buffer-name spec)))

(defun run-server (spec)
  (flet ((output-callback (string)
           (let* ((buffer (make-server-process-buffer spec))
                  (point (buffer-point buffer)))
             (buffer-end point)
             (insert-string point string))))
    (let* ((port (lem-utils/socket:random-available-port))
           (process (lem-process:run-process (funcall (spec-command spec) port)
                                             :output-callback #'output-callback)))
      (make-server-info :process process :port port))))

(defun get-running-server-info (spec)
  (gethash (spec-langauge-id spec) *language-id-server-info-map*))

(defun ensure-running-server-process (spec)
  (unless (get-running-server-info spec)
    (setf (gethash (spec-langauge-id spec) *language-id-server-info-map*)
          (run-server spec))
    (values)))

(defun quit-all-server-process ()
  (maphash (lambda (language-id server-info)
             (declare (ignore language-id))
             (lem-process:delete-process (server-info-process server-info)))
           *language-id-server-info-map*))

;;;
(defmacro with-jsonrpc-error (() &body body)
  (with-unique-names (c)
    `(handler-case (progn ,@body)
       (jsonrpc/errors:jsonrpc-callback-error (,c)
         (editor-error "~A" ,c)))))

;;;
(defvar *workspaces* '())

(defstruct workspace
  root-uri
  client
  language-id
  server-capabilities
  server-info
  (trigger-characters (make-hash-table)))

(defun find-workspace (root-uri language-id)
  (dolist (workspace *workspaces*)
    (when (and (equal (workspace-root-uri workspace)
                      root-uri)
               (equal (workspace-language-id workspace)
                      language-id))
      (return workspace))))

(defun buffer-workspace (buffer)
  (buffer-value buffer 'workspace))

(defun (setf buffer-workspace) (workspace buffer)
  (setf (buffer-value buffer 'workspace) workspace))

(defun buffer-language-spec (buffer)
  (get-language-spec (buffer-major-mode buffer)))

(defun buffer-language-id (buffer)
  (let ((spec (buffer-language-spec buffer)))
    (when spec
      (spec-langauge-id spec))))

(defun buffer-version (buffer)
  (buffer-modified-tick buffer))

(defun buffer-uri (buffer)
  (utils:pathname-to-uri (buffer-filename buffer)))

(defun get-workspace-from-point (point)
  (buffer-workspace (point-buffer point)))

(lem:define-minor-mode lsp-mode
    (:name "Language Client"
     :enable-hook 'enable-hook))

(defun enable-hook ()
  (add-hook *exit-editor-hook* 'quit-all-server-process)
  (ensure-lsp-buffer (current-buffer))
  (text-document/did-open (current-buffer)))

(defun find-root-pathname (directory uri-patterns)
  (or (utils:find-root-pathname directory
                                (lambda (file)
                                  (let ((file-name (file-namestring file)))
                                    (dolist (uri-pattern uri-patterns)
                                      (when (search uri-pattern file-name)
                                        (return t))))))
      (pathname directory)))

(defgeneric make-client (client-params))

(defstruct client-params)
(defstruct (tcp-client-params (:include client-params)) port)

(defun get-connected-port (spec)
  (let ((server-info (get-running-server-info spec)))
    (assert server-info)
    (server-info-port server-info)))

(defun make-client-params-from-spec (spec)
  (ecase (spec-mode spec)
    (:tcp (make-tcp-client-params :port (get-connected-port spec)))))

(defmethod make-client ((client-params tcp-client-params))
  (make-instance 'client:tcp-client :port (tcp-client-params-port client-params)))

(defun make-client-and-connect (spec)
  (let ((client (make-client (make-client-params-from-spec spec))))
    (client:jsonrpc-connect client)
    client))

(defun convert-to-characters (string-characters)
  (map 'list
       (lambda (string) (char string 0))
       string-characters))

(defun get-completion-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (protocol:completion-options-trigger-characters
        (protocol:server-capabilities-completion-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun get-signature-help-trigger-characters (workspace)
  (convert-to-characters
   (handler-case
       (protocol:signature-help-options-trigger-characters
        (protocol:server-capabilities-signature-help-provider
         (workspace-server-capabilities workspace)))
     (unbound-slot ()
       nil))))

(defun self-insert-hook (c)
  (when-let* ((workspace (buffer-workspace (current-buffer)))
              (command (gethash c (workspace-trigger-characters workspace))))
    (funcall command c)))

(defun buffer-change-event-to-content-change-event (point arg)
  (labels ((inserting-content-change-event (string)
             (let ((position (point-to-position point)))
               (json:make-json :range (make-instance 'protocol:range
                                                     :start position
                                                     :end position)
                               :range-length 0
                               :text string)))
           (deleting-content-change-event (count)
             (with-point ((end point))
               (character-offset end count)
               (json:make-json :range (make-instance 'protocol:range
                                                     :start (point-to-position point)
                                                     :end (point-to-position end))
                               :range-length (count-characters point end)
                               :text ""))))
    (etypecase arg
      (character
       (inserting-content-change-event (string arg)))
      (string
       (inserting-content-change-event arg))
      (integer
       (deleting-content-change-event arg)))))

(defun handle-change-buffer (point arg)
  (let ((buffer (point-buffer point))
        (change-event (buffer-change-event-to-content-change-event point arg)))
    (text-document/did-change buffer (json:json-array change-event))))

(defun assign-workspace-to-buffer (buffer workspace)
  (setf (buffer-workspace buffer) workspace)
  (add-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)
  (add-hook (variable-value 'self-insert-after-hook :buffer buffer) 'self-insert-hook)
  (setf (variable-value 'lem.language-mode:completion-spec)
        (completion:make-completion-spec #'text-document/completion
                                         :prefix-search t))
  (setf (variable-value 'lem.language-mode:find-definitions-function)
        #'find-definitions)
  (setf (variable-value 'lem.language-mode:find-references-function)
        #'find-references)
  (dolist (character (get-completion-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          (lambda (c)
            (declare (ignore c))
            (lem.language-mode::complete-symbol))))
  (dolist (character (get-signature-help-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'lsp-signature-help-with-trigger-character)))

(defun initialize-workspace (workspace)
  (push workspace *workspaces*)
  ;; initialize, initializedが失敗したときに、無効なworkspaceが残ってしまう問題があるかもしれない
  (initialize workspace)
  (initialized workspace)
  workspace)

(defun ensure-lsp-buffer (buffer)
  (let ((spec (buffer-language-spec buffer)))
    (ensure-running-server-process spec)
    (let* ((language-id (spec-langauge-id spec))
           (root-uri (utils:pathname-to-uri
                      (find-root-pathname (buffer-directory buffer)
                                          (spec-root-uri-patterns spec))))
           (workspace (or (find-workspace root-uri language-id)
                          (initialize-workspace
                           (make-workspace :client (make-client-and-connect spec)
                                           :root-uri root-uri
                                           :language-id language-id)))))
      (assign-workspace-to-buffer buffer workspace))))

(defun initialize (workspace)
  (let ((initialize-result
          (request:lsp-call-method
           (workspace-client workspace)
           (make-instance
            'request:initialize-request
            :params (make-instance
                     'protocol:initialize-params
                     :process-id (utils:get-pid)
                     :client-info (json:make-json :name "lem" #|:version "0.0.0"|#)
                     :root-uri (workspace-root-uri workspace)
                     :capabilities (make-instance
                                    'protocol:client-capabilities
                                    :workspace (json:make-json
                                                ;; :apply-edit
                                                ;; :workspace-edit
                                                ;; :did-change-configuration
                                                ;; :symbol
                                                ;; :execute-command
                                                )
                                    :text-document (make-instance
                                                    'protocol:text-document-client-capabilities
                                                    :hover (make-instance 'protocol:hover-client-capabilities)
                                                    :completion (make-instance 'protocol:completion-client-capabilities
                                                                               :completion-item (json:make-json)
                                                                               :context-support t)
                                                    :signature-help (make-instance
                                                                     'protocol:signature-help-client-capabilities
                                                                     :signature-information
                                                                     (json:make-json
                                                                      :documentation-format (json:json-array "plaintext")
                                                                      :parameter-information (json:make-json
                                                                                              :label-offset-support
                                                                                              (json:json-false))))
                                                    :definition (make-instance
                                                                 'protocol:definition-client-capabilities
                                                                 :link-support (json:json-false)))
                                    ;; :experimental
                                    )
                     :trace "off"
                     :workspace-folders (json:json-null))))))
    (setf (workspace-server-capabilities workspace)
          (protocol:initialize-result-capabilities initialize-result))
    (setf (workspace-server-info workspace)
          (protocol:initialize-result-server-info initialize-result)))
  (values))

(defun initialized (workspace)
  (request:lsp-call-method (workspace-client workspace)
                           (make-instance 'request:initialized-request)))

(defun point-to-position (point)
  (make-instance 'protocol:position
                 :line (1- (line-number-at-point point))
                 :character (point-charpos point)))

(defun buffer-to-text-document-item (buffer)
  (make-instance 'protocol:text-document-item
                 :uri (buffer-uri buffer)
                 :language-id (buffer-language-id buffer)
                 :version (buffer-version buffer)
                 :text (buffer-text buffer)))

(defun text-document/did-open (buffer)
  (request:lsp-call-method
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-open
                  :params (make-instance 'protocol:did-open-text-document-params
                                         :text-document (buffer-to-text-document-item buffer)))))

(defun text-document/did-change (buffer content-changes)
  (request:lsp-call-method
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-change
                  :params (make-instance 'protocol:did-change-text-document-params
                                         :text-document (make-instance 'protocol:versioned-text-document-identifier
                                                                       :version (buffer-version buffer)
                                                                       :uri (buffer-uri buffer))
                                         :content-changes content-changes))))

;;; hover

;; TODO
;; - workDoneProgress
;; - partialResult
;; - hoverClientCapabilitiesのcontentFormatを設定する
;; - hoverのrangeを使って範囲に背景色をつける
;; - markdownの中のコード表示時に対象の言語のシンタックスハイライトをする
;; - serverでサポートしているかのチェックをする

(defun hover-to-string (hover)
  (flet ((marked-string-to-string (marked-string)
           (if (stringp marked-string)
               marked-string
               (or (json:json-get marked-string "value")
                   ""))))
    (let ((contents (protocol:hover-contents hover)))
      (cond
        ;; MarkedString
        ((json:json-object-p contents)
         (marked-string-to-string contents))
        ;; MarkedString[]
        ((json:json-array-p contents)
         (with-output-to-string (out)
           (dolist (content contents)
             (write-string (marked-string-to-string content)
                           out))))
        ;; MarkupContent
        ((typep contents 'protocol:markup-content)
         (protocol:markup-content-value contents))
        (t
         "")))))

(defun make-text-document-position-arguments (point)
  (list
   :text-document (make-instance
                   'protocol:text-document-identifier
                   :uri (buffer-uri (point-buffer point)))
   :position (point-to-position point)))

(defun provide-hover-p (workspace)
  (handler-case (protocol:server-capabilities-hover-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/hover (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-hover-p workspace)
      (let ((result
              (request:lsp-call-method
               (workspace-client workspace)
               (make-instance 'request:hover-request
                              :params (apply #'make-instance
                                             'protocol:hover-params
                                             (make-text-document-position-arguments point))))))
        (when result
          (hover-to-string result))))))

(define-command lsp-hover () ()
  (message "~A" (text-document/hover (current-point))))

;;; completion

;; TODO
;; - serverでサポートしているかのチェックをする
;; - workDoneProgress
;; - partialResult
;; - completionParams.context, どのように補完が起動されたかの情報を含める
;; - completionItemの使っていない要素が多分にある
;; - completionResolve

(defclass completion-item (completion:completion-item)
  ((sort-text
    :initarg :sort-text
    :reader completion-item-sort-text)))

(defun convert-completion-items (items)
  (sort (map 'list
             (lambda (item)
               (make-instance 'completion-item
                              :label (protocol:completion-item-label item)
                              :detail (handler-case (protocol:completion-item-detail item)
                                        (unbound-slot () ""))
                              :sort-text (handler-case (protocol:completion-item-sort-text item)
                                           (unbound-slot ()
                                             (protocol:completion-item-label item)))))
             items)
        #'string<
        :key #'completion-item-sort-text))

(defun convert-completion-list (completion-list)
  (convert-completion-items (protocol:completion-list-items completion-list)))

(defun convert-completion-response (value)
  (cond ((typep value 'protocol:completion-list)
         (convert-completion-list value))
        ((json:json-array-p value)
         (convert-completion-items value))
        (t
         nil)))

(defun provide-completion-p (workspace)
  (handler-case (protocol:server-capabilities-completion-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/completion (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-completion-p workspace)
      (convert-completion-response
       (request:lsp-call-method
        (workspace-client workspace)
        (make-instance 'request:completion-request
                       :params (apply #'make-instance
                                      'protocol:completion-params
                                      (make-text-document-position-arguments point))))))))

;;; signatureHelp

(define-attribute signature-help-active-parameter-attribute
  (t :underline-p t))

(defun provide-signature-help-p (workspace)
  (handler-case (protocol:server-capabilities-signature-help-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun display-signature-help (signature-help)
  (let* ((buffer (make-buffer nil :temporary t))
         (point (buffer-point buffer)))
    (setf (lem:variable-value 'lem::truncate-character :buffer buffer) #\space)
    (let ((active-parameter
            (handler-case (protocol:signature-help-active-parameter signature-help)
              (unbound-slot () nil)))
          (active-signature
            (handler-case (protocol:signature-help-active-signature signature-help)
              (unbound-slot () nil))))
      (utils:do-sequence (signature index (protocol:signature-help-signatures signature-help))
        (when (plusp index) (insert-character point #\newline))
        (let ((active-signature-p (eql index active-signature)))
          (if active-signature-p
              (insert-string point "* ")
              (insert-string point "- "))
          (insert-string point
                         (protocol:signature-information-label signature))
          (when active-signature-p
            (let ((parameters
                    (handler-case
                        (protocol:signature-information-parameters signature)
                      (unbound-slot () nil))))
              (when (< active-parameter (length parameters))
                ;; TODO: labelが[number, number]の場合に対応する
                (let ((label (protocol:parameter-information-label (elt parameters active-parameter))))
                  (when (stringp label)
                    (with-point ((p point))
                      (line-start p)
                      (when (search-forward p label)
                        (with-point ((start p))
                          (character-offset start (- (length label)))
                          (put-text-property start p :attribute 'signature-help-active-parameter-attribute)))))))))
          (insert-character point #\space)
          (insert-character point #\newline)
          (insert-string point (protocol:signature-information-documentation signature))))
      (buffer-start (buffer-point buffer))
      (message-buffer buffer))))

(defun text-document/signature-help (point &optional signature-help-context)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-signature-help-p workspace)
      (let ((result (request:lsp-call-method
                     (workspace-client workspace)
                     (make-instance 'request:signature-help
                                    :params (apply #'make-instance
                                                   'protocol:signature-help-params
                                                   (append (when signature-help-context
                                                             `(:context ,signature-help-context))
                                                           (make-text-document-position-arguments point)))))))
        (when result
          (display-signature-help result))))))

(defun lsp-signature-help-with-trigger-character (character)
  (text-document/signature-help (current-point)
                                (make-instance 'protocol:signature-help-context
                                               :trigger-kind protocol:signature-help-trigger-kind.trigger-character
                                               :trigger-character (string character)
                                               :is-retrigger (json:json-false)
                                               #|:active-signature-help|#)))

(define-command lsp-signature-help () ()
  (text-document/signature-help (current-point)
                                (make-instance 'protocol:signature-help-context
                                               :trigger-kind protocol:signature-help-trigger-kind.invoked
                                               :is-retrigger (json:json-false))))

;;; declaration

(defun provide-declaration-p (workspace)
  (handler-case (protocol:server-capabilities-declaration-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/declaration (point)
  (declare (ignore point))
  ;; TODO: goplsが対応していなかったので後回し
  nil)

;;; definition

(defun provide-definition-p (workspace)
  (handler-case (protocol:server-capabilities-definition-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defgeneric convert-location (location)
  (:method ((location protocol:location))
    ;; TODO: end-positionも使い、定義位置への移動後のハイライトをstart/endの範囲にする
    (let* ((start-position (protocol:range-start (protocol:location-range location)))
           (end-position (protocol:range-end (protocol:location-range location)))
           (uri (protocol:location-uri location))
           (file (utils:uri-to-pathname uri)))
      (declare (ignore end-position))
      (lem.language-mode:make-xref-location
       :filespec file
       :position (lem.language-mode::make-position
                  (1+ (protocol:position-line start-position))
                  (protocol:position-character start-position)))))
  (:method ((location protocol:location-link))
    (error "locationLink is unsupported")))

(defun convert-definition-response (value)
  (cond ((typep value 'protocol:location)
         (list (convert-location value)))
        ((json:json-array-p value)
         ;; TODO: location-link
         (map 'list #'convert-location value))
        (t
         nil)))

(defun text-document/definition (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-definition-p workspace)
      (convert-definition-response
       (request:lsp-call-method
        (workspace-client workspace)
        (make-instance 'request:definition
                       :params (apply #'make-instance
                                      'protocol:definition-params
                                      (make-text-document-position-arguments point))))))))

(defun find-definitions (point)
  (with-jsonrpc-error ()
    (text-document/definition point)))

;;; type definition

(defun provide-type-definition-p (workspace)
  (handler-case (protocol:server-capabilities-type-definition-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-type-definition-response (value)
  (convert-definition-response value))

(defun text-document/type-definition (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-type-definition-p workspace)
      (convert-type-definition-response
       (request:lsp-call-method
        (workspace-client workspace)
        (make-instance 'request:type-definition
                       :params (apply #'make-instance
                                      'protocol:type-definition-params
                                      (make-text-document-position-arguments point))))))))

(define-command lsp-type-definition () ()
  (let ((xref-locations
          (with-jsonrpc-error ()
            (text-document/type-definition (current-point)))))
    (lem.language-mode::show-locations xref-locations)))

;;; implementation

(defun provide-implementation-p (workspace)
  (handler-case (protocol:server-capabilities-implementation-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-implementation-response (value)
  (convert-definition-response value))

(defun text-document/implementation (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-implementation-p workspace)
      (convert-implementation-response
       (request:lsp-call-method
        (workspace-client workspace)
        (make-instance 'request:implementation
                       :params (apply #'make-instance
                                      'protocol:type-definition-params
                                      (make-text-document-position-arguments point))))))))

(define-command lsp-implementation () ()
  (let ((xref-locations
          (with-jsonrpc-error ()
            (text-document/implementation (current-point)))))
    (lem.language-mode::show-locations xref-locations)))

;;; references

(defun provide-references-p (workspace)
  (handler-case (protocol:server-capabilities-references-provider (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun xref-location-to-content (location)
  (let* ((buffer (find-file-buffer (lem.language-mode:xref-location-filespec location) :temporary t))
         (point (buffer-point buffer)))
    (lem.language-mode::move-to-location-position point (lem.language-mode:xref-location-position location))
    (string-trim '(#\space #\tab) (line-string point))))

(defun convert-references-response (value)
  (lem.language-mode:make-xref-references
   :type nil
   :locations (mapcar (lambda (location)
                        (lem.language-mode:make-xref-location
                         :filespec (lem.language-mode:xref-location-filespec location)
                         :position (lem.language-mode:xref-location-position location)
                         :content (xref-location-to-content location)))
                      (convert-definition-response value))))

(defun text-document/references (point &optional include-declaration)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-references-p workspace)
      (convert-references-response
       (request:lsp-call-method
        (workspace-client workspace)
        (make-instance 'request:references
                       :params (apply #'make-instance
                                      'protocol:reference-params
                                      :context (make-instance 'protocol:reference-context
                                                              :include-declaration (json:to-json-boolean
                                                                                    include-declaration))
                                      (make-text-document-position-arguments point))))))))

(defun find-references (point)
  (text-document/references point))

;;;
(defvar *language-spec-table* (make-hash-table))

(defun get-language-spec (major-mode)
  (gethash major-mode *language-spec-table*))

(defun spec-langauge-id (spec)
  (getf spec :language-id))

(defun spec-root-uri-patterns (spec)
  (getf spec :root-uri-patterns))

(defun spec-mode (spec)
  (getf spec :mode))

(defun spec-port (spec)
  (getf spec :port))

(defun spec-command (spec)
  (getf spec :command))

(defmacro def-language-spec (major-mode &rest plist)
  `(progn
     (setf (gethash ',major-mode *language-spec-table*)
           (list ,@plist))
     ,(when (mode-hook major-mode)
        `(add-hook ,(mode-hook major-mode) 'lsp-mode))))

(def-language-spec lem-go-mode:go-mode
  :language-id "go"
  :root-uri-patterns '("go.mod")
  :command (lambda (port) `("gopls" "serve" "-port" ,(princ-to-string port)))
  :mode :tcp
  :port 12345)


#|
Language Features
- [X] completion
- [ ] completion resolve
- [X] hover
- [X] signatureHelp
- [ ] declaration
- [X] definition
- [X] typeDefinition
- [X] implementation
- [X] references
- [ ] documentHighlight
- [ ] documentSymbol
- [ ] codeAction
- [ ] codeLens
- [ ] codeLens resolve
- [ ] documentLink
- [ ] documentLink resolve
- [ ] documentColor
- [ ] colorPresentation
- [ ] formatting
- [ ] rangeFormatting
- [ ] onTypeFormatting
- [ ] rename
- [ ] prepareRename
- [ ] foldingRange
- [ ] selectionRange

- partialResult
- workDoneProgress
|#
