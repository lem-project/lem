(defpackage :lem-lsp-mode/lsp-mode
  (:use :cl :lem :alexandria)
  (:shadow :execute-command)
  (:import-from :lem-lsp-utils)
  (:import-from :lem-lsp-mode/utils)
  (:import-from :lem-lsp-mode/request)
  (:import-from :lem-lsp-mode/client)
  (:import-from :lem-lsp-mode/context-menu))
(in-package :lem-lsp-mode/lsp-mode)

(cl-package-locks:lock-package :lem-lsp-mode/lsp-mode)
(lem-lsp-mode/project:local-nickname :protocol :lem-lsp-utils/protocol)
(lem-lsp-mode/project:local-nickname :json :lem-lsp-utils/json)
(lem-lsp-mode/project:local-nickname :utils :lem-lsp-mode/utils)
(lem-lsp-mode/project:local-nickname :request :lem-lsp-mode/request)
(lem-lsp-mode/project:local-nickname :client :lem-lsp-mode/client)
(lem-lsp-mode/project:local-nickname :completion :lem.completion-mode)
(lem-lsp-mode/project:local-nickname :context-menu :lem-lsp-mode/context-menu)
(lem-lsp-mode/project:local-nickname :spinner :lem.loading-spinner)

;;;
(defparameter *client-capabilities-text*
  (load-time-value
   (uiop:read-file-string
    (asdf:system-relative-pathname :lem-lsp-mode
                                   "client-capabilities.json"))))

(defun client-capabilities ()
  (json:coerce-json
   (yason:parse *client-capabilities-text* :json-booleans-as-symbols t)
   'protocol:client-capabilities))

;;;
(defvar *language-id-server-info-map* (make-hash-table :test 'equal))

(defstruct server-info
  port
  process
  disposable)

(defun server-process-buffer-name (spec)
  (format nil "*Lsp <~A>*" (spec-language-id spec)))

(defun make-server-process-buffer (spec)
  (make-buffer (server-process-buffer-name spec)))

(defun get-spec-command (spec &rest args)
  (let ((command (spec-command spec)))
    (if (functionp command)
        (apply command args)
        command)))

(defmethod run-server-using-mode ((mode (eql :tcp)) spec)
  (flet ((output-callback (string)
           (let* ((buffer (make-server-process-buffer spec))
                  (point (buffer-point buffer)))
             (buffer-end point)
             (insert-string point string))))
    (let* ((port (or (spec-port spec) (lem-utils/socket:random-available-port)))
           (process (when-let (command (get-spec-command spec port))
                      (lem-process:run-process command
                                               :output-callback #'output-callback))))
      (make-server-info :process process
                        :port port
                        :disposable (lambda () (lem-process:delete-process process))))))

(defmethod run-server-using-mode ((mode (eql :stdio)) spec)
  (let ((process (async-process:create-process (get-spec-command spec) :nonblock nil)))
    (make-server-info :process process
                      :disposable (lambda () (async-process:delete-process process)))))

(defun run-server (spec)
  (run-server-using-mode (spec-mode spec) spec))

(defun get-running-server-info (spec)
  (gethash (spec-language-id spec) *language-id-server-info-map*))

(defun remove-server-info (spec)
  (remhash (spec-language-id spec) *language-id-server-info-map*))

(defun ensure-running-server-process (spec)
  (unless (get-running-server-info spec)
    (setf (gethash (spec-language-id spec) *language-id-server-info-map*)
          (run-server spec))
    t))

(defun kill-server-process (spec)
  (when-let* ((server-info (get-running-server-info spec))
              (disposable (server-info-disposable server-info)))
    (funcall disposable)
    (remove-server-info spec)))

(defun quit-all-server-process ()
  (maphash (lambda (language-id server-info)
             (declare (ignore language-id))
             (when-let ((disposable (server-info-disposable server-info)))
               (funcall disposable)))
           *language-id-server-info-map*)
  (clrhash *language-id-server-info-map*))

;;;
(defmacro with-jsonrpc-error (() &body body)
  (with-unique-names (c)
    `(handler-case (progn ,@body)
       (jsonrpc/errors:jsonrpc-callback-error (,c)
         (editor-error "~A" ,c)))))

(defun invoke-async (function then catch)
  (bt:make-thread
   (lambda ()
     (handler-case
         (let ((response (funcall function)))
           (when then (send-event (lambda () (funcall then response)))))
       (jsonrpc/errors:jsonrpc-callback-error (c)
         (when catch (send-event (lambda () (funcall catch c)))))))))

(defmacro async (form &key then catch)
  `(invoke-async (lambda () ,form)
                 ,then
                 ,catch))

(defun simple-editor-error (message)
  (editor-error "~A" message))

(defun jsonrpc-editor-error (message code)
  (editor-error "JSONRPC-CALLBACK-ERROR: ~A (Code=~A)" message code))

(defun async-request (client request &key then)
  (request:request-async client request
                         (lambda (response)
                           (send-event (lambda () (funcall then response))))
                         (lambda (message code)
                           (send-event (lambda () (jsonrpc-editor-error message code))))))

;;;
(defgeneric spec-initialization-options (spec)
  (:method (spec) nil))

(defclass spec ()
  ((language-id
    :initarg :language-id
    :initform (required-argument :language-id)
    :reader spec-language-id)
   (root-uri-patterns
    :initarg :root-uri-patterns
    :initform nil
    :reader spec-root-uri-patterns)
   (command
    :initarg :command
    :initform nil
    :reader spec-command)
   (mode
    :initarg :mode
    :initform (required-argument :mode)
    :reader spec-mode)
   (port
    :initarg :port
    :initform nil
    :reader spec-port)))

(defun get-language-spec (major-mode)
  (make-instance (get major-mode 'spec)))

(defun register-language-spec (major-mode spec-name)
  (setf (get major-mode 'spec) spec-name))

;;;
(defvar *workspaces* '())

(defstruct workspace
  root-uri
  client
  spec
  server-capabilities
  server-info
  (trigger-characters (make-hash-table)))

(defun workspace-language-id (workspace)
  (spec-language-id (workspace-spec workspace)))

(defun find-workspace (language-id &key (errorp t))
  (dolist (workspace *workspaces*
                     (when errorp
                       (editor-error "The ~A workspace is not found." language-id)))
    (when (equal (workspace-language-id workspace)
                 language-id)
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
      (spec-language-id spec))))

(defun buffer-version (buffer)
  (buffer-modified-tick buffer))

(defun buffer-uri (buffer)
  (lem-lsp-utils/uri:pathname-to-uri (buffer-filename buffer)))

(defun get-workspace-from-point (point)
  (buffer-workspace (point-buffer point)))

(define-minor-mode lsp-mode
    (:name "lsp"
     :enable-hook 'enable-hook)
  (setf (variable-value 'lem.language-mode:completion-spec)
        (completion:make-completion-spec #'text-document/completion :prefix-search t))
  (setf (variable-value 'lem.language-mode:find-definitions-function)
        #'find-definitions)
  (setf (variable-value 'lem.language-mode:find-references-function)
        #'find-references))

(defun enable-hook ()
  (let ((buffer (current-buffer)))
    (handler-case
        (progn
          (add-hook *exit-editor-hook* 'quit-all-server-process)
          (ensure-lsp-buffer buffer
                             (lambda ()
                               (text-document/did-open buffer)
                               (enable-document-highlight-idle-timer)
                               (redraw-display))))
      (editor-error (c)
        (message "~A" c)))))

(defun find-root-pathname (directory uri-patterns)
  (or (utils:find-root-pathname directory
                                (lambda (file)
                                  (let ((file-name (file-namestring file)))
                                    (dolist (uri-pattern uri-patterns)
                                      (when (search uri-pattern file-name)
                                        (return t))))))
      (pathname directory)))

(defun get-connected-port (spec)
  (let ((server-info (get-running-server-info spec)))
    (assert server-info)
    (server-info-port server-info)))

(defun get-spec-process (spec)
  (let ((server-info (get-running-server-info spec)))
    (assert server-info)
    (server-info-process server-info)))

(defun make-client (spec)
  (ecase (spec-mode spec)
    (:tcp (make-instance 'client:tcp-client :port (get-connected-port spec)))
    (:stdio (make-instance 'client:stdio-client :process (get-spec-process spec)))))

(defun make-client-and-connect (spec)
  (let ((client (make-client spec)))
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
             (let ((position (point-to-lsp-position point)))
               (json:make-json :range (make-instance 'protocol:range
                                                     :start position
                                                     :end position)
                               :range-length 0
                               :text string)))
           (deleting-content-change-event (count)
             (with-point ((end point))
               (character-offset end count)
               (json:make-json :range (make-instance 'protocol:range
                                                     :start (point-to-lsp-position point)
                                                     :end (point-to-lsp-position end))
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
  (add-hook (variable-value 'kill-buffer-hook :buffer buffer) 'text-document/did-close)
  (add-hook (variable-value 'after-save-hook :buffer buffer) 'text-document/did-save)
  (add-hook (variable-value 'before-change-functions :buffer buffer) 'handle-change-buffer)
  (add-hook (variable-value 'self-insert-after-hook :buffer buffer) 'self-insert-hook)
  (dolist (character (get-completion-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'completion-with-trigger-character))
  (dolist (character (get-signature-help-trigger-characters workspace))
    (setf (gethash character (workspace-trigger-characters workspace))
          #'lsp-signature-help-with-trigger-character)))

(defun initialize-workspace (workspace continuation)
  (jsonrpc:expose (client:client-connection (workspace-client workspace))
                  "textDocument/publishDiagnostics"
                  'text-document/publish-diagnostics)
  (jsonrpc:expose (client:client-connection (workspace-client workspace))
                  "window/showMessage"
                  'window/show-message)
  (initialize workspace
              (lambda ()
                (initialized workspace)
                (push workspace *workspaces*)
                (funcall continuation workspace))))

(defun establish-connection (spec)
  (when (ensure-running-server-process spec)
    (let ((client (make-client spec)))
      (loop :with condition := nil
            :repeat 3
            :do (handler-case (client:jsonrpc-connect client)
                  (:no-error (&rest values)
                    (declare (ignore values))
                    (return client))
                  (error (c)
                    (setq condition c)
                    (sleep 0.1)))
            :finally (editor-error "Could not establish a connection with the Language Server (condition: ~A)"
                                   condition)))))

(defun ensure-lsp-buffer (buffer &optional continuation)
  (let* ((spec (buffer-language-spec buffer))
         (root-uri (lem-lsp-utils/uri:pathname-to-uri
                    (find-root-pathname (buffer-directory buffer)
                                        (spec-root-uri-patterns spec)))))
    (handler-bind ((error (lambda (c)
                            (declare (ignore c))
                            (kill-server-process spec))))
      (let ((new-client (establish-connection spec)))
        (cond ((null new-client)
               (let ((workspace (find-workspace (spec-language-id spec) :errorp t)))
                 (assign-workspace-to-buffer buffer workspace)
                 (when continuation (funcall continuation))))
              (t
               (let ((spinner (spinner:start-loading-spinner
                               :modeline
                               :loading-message "initializing"
                               :buffer buffer)))
                 (initialize-workspace
                  (make-workspace :client new-client
                                  :root-uri root-uri
                                  :spec spec)
                  (lambda (workspace)
                    (assign-workspace-to-buffer buffer workspace)
                    (when continuation (funcall continuation))
                    (spinner:stop-loading-spinner spinner))))))))))

(defun check-connection ()
  (let* ((buffer (current-buffer))
         (spec (buffer-language-spec buffer)))
    (unless (get-running-server-info spec)
      (ensure-lsp-buffer buffer))))

(defun point-to-lsp-position (point)
  (make-instance 'protocol:position
                 :line (1- (line-number-at-point point))
                 :character (point-charpos point)))

(defun move-to-lsp-position (point position)
  (buffer-start point)
  (or (line-offset point
                   (protocol:position-line position)
                   (protocol:position-character position))
      (buffer-end point)))

(defun make-lsp-range (start end)
  (make-instance 'protocol:range
                 :start (point-to-lsp-position start)
                 :end (point-to-lsp-position end)))

(defun buffer-to-text-document-item (buffer)
  (make-instance 'protocol:text-document-item
                 :uri (buffer-uri buffer)
                 :language-id (buffer-language-id buffer)
                 :version (buffer-version buffer)
                 :text (buffer-text buffer)))

(defun make-text-document-identifier (buffer)
  (make-instance
   'protocol:text-document-identifier
   :uri (buffer-uri buffer)))

(defun make-text-document-position-arguments (point)
  (list :text-document (make-text-document-identifier (point-buffer point))
        :position (point-to-lsp-position point)))

(defun find-buffer-from-uri (uri)
  (let ((pathname (lem-lsp-utils/uri:uri-to-pathname uri)))
    (dolist (buffer (buffer-list))
      (when (uiop:pathname-equal pathname (buffer-filename buffer))
        (return buffer)))))

(defun get-buffer-from-text-document-identifier (text-document-identifier)
  (let ((uri (protocol:text-document-identifier-uri text-document-identifier)))
    (find-buffer-from-uri uri)))

(defun apply-text-edits (buffer text-edits)
  (flet ((replace-points ()
           (let ((points '()))
             (with-point ((start (buffer-point buffer) :left-inserting)
                          (end (buffer-point buffer) :left-inserting))
               (lem-utils:do-sequence (text-edit text-edits)
                 (let ((range (protocol:text-edit-range text-edit))
                       (new-text (protocol:text-edit-new-text text-edit)))
                   (move-to-lsp-position start (protocol:range-start range))
                   (move-to-lsp-position end (protocol:range-end range))
                   (push (list (copy-point start)
                               (copy-point end)
                               new-text)
                         points))))
             (nreverse points))))
    (let ((points (replace-points)))
      (unwind-protect
           (loop :for (start end text) :in points
                 :do (delete-between-points start end)
                     (insert-string start text))
        (loop :for (start end) :in points
              :do (delete-point start)
                  (delete-point end))))))

(defgeneric apply-document-change (document-change))

(defmethod apply-document-change ((document-change protocol:text-document-edit))
  (let* ((buffer
           (get-buffer-from-text-document-identifier
            (protocol:text-document-edit-text-document document-change))))
    (apply-text-edits buffer (protocol:text-document-edit-edits document-change))))

(defmethod apply-document-change ((document-change protocol:create-file))
  (error "createFile is not yet supported"))

(defmethod apply-document-change ((document-change protocol:rename-file))
  (error "renameFile is not yet supported"))

(defmethod apply-document-change ((document-change protocol:delete-file))
  (error "deleteFile is not yet supported"))

(defun apply-workspace-edit (workspace-edit)
  (labels ((apply-document-changes (document-changes)
             (lem-utils:do-sequence (document-change document-changes)
               (apply-document-change document-change)))
           (apply-changes (changes)
             (declare (ignore changes))
             (error "Not yet implemented")))
    (if-let ((document-changes (handler-case
                                   (protocol:workspace-edit-document-changes workspace-edit)
                                 (unbound-slot () nil))))
      (apply-document-changes document-changes)
      (when-let ((changes (handler-case (protocol:workspace-edit-changes workspace-edit)
                            (unbound-slot () nil))))
        (apply-changes changes)))))

;;; General Messages

(defun initialize (workspace continuation)
  (async-request
   (workspace-client workspace)
   (make-instance
    'request:initialize-request
    :params (apply #'make-instance
                   'protocol:initialize-params
                   :process-id (utils:get-pid)
                   :client-info (json:make-json :name "lem" #|:version "0.0.0"|#)
                   :root-uri (workspace-root-uri workspace)
                   :capabilities (client-capabilities)
                   :trace "off"
                   :workspace-folders (json:json-null)
                   (when-let ((value (spec-initialization-options (workspace-spec workspace))))
                     (list :initialization-options value))))
   :then (lambda (initialize-result)
           (setf (workspace-server-capabilities workspace)
                 (protocol:initialize-result-capabilities initialize-result))
           (handler-case (protocol:initialize-result-server-info initialize-result)
             (unbound-slot () nil)
             (:no-error (server-info)
               (setf (workspace-server-info workspace)
                     server-info)))
           (funcall continuation))))

(defun initialized (workspace)
  (request:request (workspace-client workspace)
                   (make-instance 'request:initialized-request)))

;;; Window

;; TODO
;; - window/showMessageRequest
;; - window/logMessage
;; - window/workDoneProgress/create
;; - window/workDoenProgress/cancel

(defun window/show-message (params)
  (request::do-request-log "window/showMessage" params :from :server)
  (let* ((params (json:coerce-json params 'protocol:show-message-params))
         (text (format nil "~A: ~A"
                       (switch ((protocol:show-message-params-type params) :test #'=)
                         (protocol:message-type.error
                          "Error")
                         (protocol:message-type.warning
                          "Warning")
                         (protocol:message-type.info
                          "Info")
                         (protocol:message-type.log
                          "Log"))
                       (protocol:show-message-params-message params))))
    (send-event (lambda ()
                  (display-popup-message text
                                         :gravity :top
                                         :timeout 3)))))

;;; Text Synchronization

(defun text-document/did-open (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-open
                  :params (make-instance 'protocol:did-open-text-document-params
                                         :text-document (buffer-to-text-document-item buffer)))))

(defun text-document/did-change (buffer content-changes)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance
    'request:text-document-did-change
    :params (make-instance 'protocol:did-change-text-document-params
                           :text-document (make-instance 'protocol:versioned-text-document-identifier
                                                         :version (buffer-version buffer)
                                                         :uri (buffer-uri buffer))
                           :content-changes content-changes))))

(defun provide-did-save-text-document-p (workspace)
  (let ((sync (protocol:server-capabilities-text-document-sync
               (workspace-server-capabilities workspace))))
    (etypecase sync
      (number
       (member sync
               (list protocol:text-document-sync-kind.full
                     protocol:text-document-sync-kind.incremental)))
      (protocol:text-document-sync-options
       (handler-case (protocol:text-document-sync-options-save sync)
         (unbound-slot ()
           nil))))))

(defun text-document/did-save (buffer)
  (when (provide-did-save-text-document-p (buffer-workspace buffer))
    (request:request
     (workspace-client (buffer-workspace buffer))
     (make-instance 'request:text-document-did-save
                    :params (make-instance 'protocol:did-save-text-document-params
                                           :text-document (make-text-document-identifier buffer)
                                           :text (buffer-text buffer))))))

(defun text-document/did-close (buffer)
  (request:request
   (workspace-client (buffer-workspace buffer))
   (make-instance 'request:text-document-did-close
                  :params (make-instance 'protocol:did-close-text-document-params
                                         :text-document (make-text-document-identifier buffer)))))

;;; publishDiagnostics

;; TODO
;; - tagSupport
;; - versionSupport

(define-attribute diagnostic-error-attribute
  (t :foreground "red" :underline-p t))

(define-attribute diagnostic-warning-attribute
  (t :foreground "orange" :underline-p t))

(define-attribute diagnostic-information-attribute
  (t :foreground "gray" :underline-p t))

(define-attribute diagnostic-hint-attribute
  (t :foreground "yellow" :underline-p t))

(defun diagnostic-severity-attribute (diagnostic-severity)
  (switch (diagnostic-severity :test #'=)
    (protocol:diagnostic-severity.error
     'diagnostic-error-attribute)
    (protocol:diagnostic-severity.warning
     'diagnostic-warning-attribute)
    (protocol:diagnostic-severity.information
     'diagnostic-information-attribute)
    (protocol:diagnostic-severity.hint
     'diagnostic-hint-attribute)))

(defstruct diagnostic
  buffer
  position
  message)

(defun buffer-diagnostic-overlays (buffer)
  (buffer-value buffer 'diagnostic-overlays))

(defun (setf buffer-diagnostic-overlays) (overlays buffer)
  (setf (buffer-value buffer 'diagnostic-overlays) overlays))

(defun clear-diagnostic-overlays (buffer)
  (mapc #'delete-overlay (buffer-diagnostic-overlays buffer))
  (setf (buffer-diagnostic-overlays buffer) '()))

(defun buffer-diagnostic-idle-timer (buffer)
  (buffer-value buffer 'diagnostic-idle-timer))

(defun (setf buffer-diagnostic-idle-timer) (idle-timer buffer)
  (setf (buffer-value buffer 'diagnostic-idle-timer) idle-timer))

(defun overlay-diagnostic (overlay)
  (overlay-get overlay 'diagnostic))

(defun buffer-diagnostics (buffer)
  (mapcar #'overlay-diagnostic (buffer-diagnostic-overlays buffer)))

(defun reset-buffer-diagnostic (buffer)
  (clear-diagnostic-overlays buffer)
  (when-let (timer (buffer-diagnostic-idle-timer buffer))
    (stop-timer timer)
    (setf (buffer-diagnostic-idle-timer buffer) nil)))

(defun point-to-xref-position (point)
  (lem.language-mode::make-xref-position :line-number (line-number-at-point point)
                                         :charpos (point-charpos point)))

(defun highlight-diagnostic (buffer diagnostic)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (let ((range (protocol:diagnostic-range diagnostic)))
      (move-to-lsp-position start (protocol:range-start range))
      (move-to-lsp-position end (protocol:range-end range))
      (let ((overlay (make-overlay start end
                                   (handler-case (protocol:diagnostic-severity diagnostic)
                                     (unbound-slot ()
                                       'diagnostic-error-attribute)
                                     (:no-error (severity)
                                       (diagnostic-severity-attribute severity))))))
        (overlay-put overlay
                     'diagnostic
                     (make-diagnostic :buffer buffer
                                      :position (point-to-xref-position start)
                                      :message (protocol:diagnostic-message diagnostic)))
        (push overlay (buffer-diagnostic-overlays buffer))))))

(defun highlight-diagnostics (params)
  (when-let ((buffer (find-buffer-from-uri (protocol:publish-diagnostics-params-uri params))))
    (reset-buffer-diagnostic buffer)
    (lem-utils:do-sequence (diagnostic (protocol:publish-diagnostics-params-diagnostics params))
      (highlight-diagnostic buffer diagnostic))
    (setf (buffer-diagnostic-idle-timer buffer)
          (start-idle-timer 1000 t #'popup-diagnostic nil "lsp-diagnostic"))))

(defun popup-diagnostic ()
  (dolist (overlay (buffer-diagnostic-overlays (current-buffer)))
    (when (point<= (overlay-start overlay)
                   (current-point)
                   (overlay-end overlay))
      (message "~A" (diagnostic-message (overlay-diagnostic overlay)))
      (return))))

(defun text-document/publish-diagnostics (params)
  (request::do-request-log "textDocument/publishDiagnostics" params :from :server)
  (let ((params (json:coerce-json params 'protocol:publish-diagnostics-params)))
    (send-event (lambda () (highlight-diagnostics params)))))

(define-command lsp-document-diagnostics () ()
  (when-let ((diagnostics (buffer-diagnostics (current-buffer))))
    (lem.sourcelist:with-sourcelist (sourcelist "*Diagnostics*")
      (dolist (diagnostic diagnostics)
        (lem.sourcelist:append-sourcelist
         sourcelist
         (lambda (point)
           (insert-string point (buffer-filename (diagnostic-buffer diagnostic))
                          :attribute 'lem.sourcelist:title-attribute)
           (insert-string point ":")
           (insert-string point
                          (princ-to-string (lem.language-mode::xref-position-line-number
                                            (diagnostic-position diagnostic)))
                          :attribute 'lem.sourcelist:position-attribute)
           (insert-string point ":")
           (insert-string point
                          (princ-to-string (lem.language-mode::xref-position-charpos
                                            (diagnostic-position diagnostic)))
                          :attribute 'lem.sourcelist:position-attribute)
           (insert-string point ":")
           (insert-string point (diagnostic-message diagnostic)))
         (let ((diagnostic diagnostic))
           (lambda (set-buffer-fn)
             (funcall set-buffer-fn (diagnostic-buffer diagnostic))
             (lem.language-mode:move-to-xref-location-position
              (buffer-point (diagnostic-buffer diagnostic))
              (diagnostic-position diagnostic)))))))))

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
        ((typep contents 'protocol::marked-string)
         (marked-string-to-string contents))
        ;; MarkedString[]
        ((json:json-array-p contents)
         (with-output-to-string (out)
           (lem-utils:do-sequence (content contents)
             (write-string (marked-string-to-string content)
                           out))))
        ;; MarkupContent
        ((typep contents 'protocol:markup-content)
         (protocol:markup-content-value contents))
        (t
         "")))))

(defun provide-hover-p (workspace)
  (handler-case (protocol:server-capabilities-hover-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/hover (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-hover-p workspace)
      (let ((result
              (request:request
               (workspace-client workspace)
               (make-instance 'request:hover-request
                              :params (apply #'make-instance
                                             'protocol:hover-params
                                             (make-text-document-position-arguments point))))))
        (when result
          (hover-to-string result))))))

(define-command lsp-hover () ()
  (check-connection)
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
  (handler-case (protocol:server-capabilities-completion-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/completion (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-completion-p workspace)
      (convert-completion-response
       (request:request
        (workspace-client workspace)
        (make-instance 'request:completion-request
                       :params (apply #'make-instance
                                      'protocol:completion-params
                                      (make-text-document-position-arguments point))))))))

(defun completion-with-trigger-character (c)
  (declare (ignore c))
  (check-connection)
  (lem.language-mode::complete-symbol))

;;; signatureHelp

(define-attribute signature-help-active-parameter-attribute
  (t :underline-p t))

(defun provide-signature-help-p (workspace)
  (handler-case (protocol:server-capabilities-signature-help-provider
                 (workspace-server-capabilities workspace))
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
      (lem-utils:do-sequence ((signature index) (protocol:signature-help-signatures signature-help))
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
              (when (and (plusp (length parameters))
                         (< active-parameter (length parameters)))
                ;; TODO: labelが[number, number]の場合に対応する
                (let ((label (protocol:parameter-information-label
                              (elt parameters
                                   (if (<= 0 active-parameter (1- (length parameters)))
                                       active-parameter
                                       0)))))
                  (when (stringp label)
                    (with-point ((p point))
                      (line-start p)
                      (when (search-forward p label)
                        (with-point ((start p))
                          (character-offset start (- (length label)))
                          (put-text-property start p
                                             :attribute 'signature-help-active-parameter-attribute)))))))))
          (insert-character point #\space)
          (insert-character point #\newline)
          (handler-case (protocol:signature-information-documentation signature)
            (unbound-slot () nil)
            (:no-error (documentation)
              (if (typep documentation 'protocol:markup-content)
                  (insert-string point (protocol:markup-content-value documentation))
                  (insert-string point documentation))))))
      (buffer-start (buffer-point buffer))
      (message-buffer buffer))))

(defun text-document/signature-help (point &optional signature-help-context)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-signature-help-p workspace)
      (let ((result (request:request
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
  (text-document/signature-help
   (current-point)
   (make-instance 'protocol:signature-help-context
                  :trigger-kind protocol:signature-help-trigger-kind.trigger-character
                  :trigger-character (string character)
                  :is-retrigger (json:json-false)
                  #|:active-signature-help|#)))

(define-command lsp-signature-help () ()
  (check-connection)
  (text-document/signature-help (current-point)
                                (make-instance 'protocol:signature-help-context
                                               :trigger-kind protocol:signature-help-trigger-kind.invoked
                                               :is-retrigger (json:json-false))))

;;; declaration

(defun provide-declaration-p (workspace)
  (handler-case (protocol:server-capabilities-declaration-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/declaration (point)
  (declare (ignore point))
  ;; TODO: goplsが対応していなかったので後回し
  nil)

;;; definition

(defun provide-definition-p (workspace)
  (handler-case (protocol:server-capabilities-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun definition-location-to-content (file location)
  (when-let* ((buffer (find-file-buffer file))
              (point (buffer-point buffer))
              (range (protocol:location-range location)))
    (with-point ((start point)
                 (end point))
      (move-to-lsp-position start (protocol:range-start range))
      (move-to-lsp-position end (protocol:range-end range))
      (line-start start)
      (line-end end)
      (points-to-string start end))))

(defgeneric convert-location (location)
  (:method ((location protocol:location))
    ;; TODO: end-positionも使い、定義位置への移動後のハイライトをstart/endの範囲にする
    (let* ((start-position (protocol:range-start (protocol:location-range location)))
           (end-position (protocol:range-end (protocol:location-range location)))
           (uri (protocol:location-uri location))
           (file (lem-lsp-utils/uri:uri-to-pathname uri)))
      (declare (ignore end-position))
      (when (uiop:file-exists-p file)
        (lem.language-mode:make-xref-location
         :filespec file
         :position (lem.language-mode::make-position
                    (1+ (protocol:position-line start-position))
                    (protocol:position-character start-position))
         :content (definition-location-to-content file location)))))
  (:method ((location protocol:location-link))
    (error "locationLink is unsupported")))

(defun convert-definition-response (value)
  (remove nil
          (cond ((typep value 'protocol:location)
                 (list (convert-location value)))
                ((json:json-array-p value)
                 ;; TODO: location-link
                 (map 'list #'convert-location value))
                (t
                 nil))))

(defun text-document/definition (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-definition-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'request:definition
                      :params (apply #'make-instance
                                     'protocol:definition-params
                                     (make-text-document-position-arguments point)))
       :then (lambda (response)
               (funcall then (convert-definition-response response)))))))

(defun find-definitions (point)
  (check-connection)
  (text-document/definition point #'lem.language-mode:display-xref-locations))

;;; type definition

(defun provide-type-definition-p (workspace)
  (handler-case (protocol:server-capabilities-type-definition-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-type-definition-response (value)
  (convert-definition-response value))

(defun text-document/type-definition (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-type-definition-p workspace)
      (async-request (workspace-client workspace)
                     (make-instance 'request:type-definition
                                    :params (apply #'make-instance
                                                   'protocol:type-definition-params
                                                   (make-text-document-position-arguments point)))
                     :then (lambda (response)
                             (funcall then (convert-type-definition-response response)))))))

(define-command lsp-type-definition () ()
  (check-connection)
  (text-document/type-definition (current-point) #'lem.language-mode:display-xref-locations))

;;; implementation

(defun provide-implementation-p (workspace)
  (handler-case (protocol:server-capabilities-implementation-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun convert-implementation-response (value)
  (convert-definition-response value))

(defun text-document/implementation (point then)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-implementation-p workspace)
      (async-request (workspace-client workspace)
                     (make-instance 'request:implementation
                                    :params (apply #'make-instance
                                                   'protocol:type-definition-params
                                                   (make-text-document-position-arguments point)))
                     :then (lambda (response)
                             (funcall then (convert-implementation-response response)))))))

(define-command lsp-implementation () ()
  (check-connection)
  (text-document/implementation (current-point)
                                #'lem.language-mode:display-xref-locations))

;;; references

(defun provide-references-p (workspace)
  (handler-case (protocol:server-capabilities-references-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun xref-location-to-content (location)
  (when-let*
      ((buffer (find-file-buffer (lem.language-mode:xref-location-filespec location) :temporary t))
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

(defun text-document/references (point then &optional include-declaration)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-references-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'request:references
                      :params (apply #'make-instance
                                     'protocol:reference-params
                                     :context (make-instance 'protocol:reference-context
                                                             :include-declaration (json:to-json-boolean
                                                                                   include-declaration))
                                     (make-text-document-position-arguments point)))
       :then (lambda (response)
               (funcall then (convert-references-response response)))))))

(defun find-references (point)
  (check-connection)
  (text-document/references point
                            #'lem.language-mode:display-xref-references))

;;; document highlights

(define-attribute document-highlight-text-attribute
  (t :background "yellow4"))

(defun provide-document-highlight-p (workspace)
  (handler-case (protocol:server-capabilities-document-highlight-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defvar *document-highlight-overlays* '())

(defun clear-document-highlight-overlays ()
  (mapc #'delete-overlay *document-highlight-overlays*))

(defun display-document-highlights (buffer document-highlights)
  (clear-document-highlight-overlays)
  (with-point ((start (buffer-point buffer))
               (end (buffer-point buffer)))
    (lem-utils:do-sequence (document-highlight document-highlights)
      (let* ((range (protocol:document-highlight-range document-highlight)))
        (move-to-lsp-position start (protocol:range-start range))
        (move-to-lsp-position end (protocol:range-end range))
        (push (make-overlay start end 'document-highlight-text-attribute)
              *document-highlight-overlays*)))))

(defun text-document/document-highlight (point)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-document-highlight-p workspace)
      (async-request
       (workspace-client workspace)
       (make-instance 'request:document-highlight
                      :params (apply #'make-instance
                                     'protocol:document-highlight-params
                                     (make-text-document-position-arguments point)))
       :then (lambda (value)
               (display-document-highlights (point-buffer point)
                                            value)
               (redraw-display))))))

(defun document-highlight-calls-timer ()
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (text-document/document-highlight (current-point))))

(define-command lsp-document-highlight () ()
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (check-connection)
    (text-document/document-highlight (current-point))))

(defvar *document-highlight-idle-timer* nil)

(defun enable-document-highlight-idle-timer ()
  (unless *document-highlight-idle-timer*
    (setf *document-highlight-idle-timer*
          (start-idle-timer 500 t #'document-highlight-calls-timer nil
                            "lsp-document-highlight"))))

(define-condition lsp-after-executing-command (after-executing-command) ())
(defmethod handle-signal ((condition lsp-after-executing-command))
  (when (mode-active-p (current-buffer) 'lsp-mode)
    (clear-document-highlight-overlays)))

;;; document symbols

;; TODO
;; - position順でソートする

(define-attribute symbol-kind-file-attribute
  (t :foreground "snow1"))

(define-attribute symbol-kind-module-attribute
  (t :foreground "firebrick"))

(define-attribute symbol-kind-namespace-attribute
  (t :foreground "dark orchid"))

(define-attribute symbol-kind-package-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-class-attribute
  (t :foreground "bisque2"))

(define-attribute symbol-kind-method-attribute
  (t :foreground "MediumPurple2"))

(define-attribute symbol-kind-property-attribute
  (t :foreground "MistyRose4"))

(define-attribute symbol-kind-field-attribute
  (t :foreground "azure3"))

(define-attribute symbol-kind-constructor-attribute
  (t :foreground "LightSkyBlue3"))

(define-attribute symbol-kind-enum-attribute
  (t :foreground "LightCyan4"))

(define-attribute symbol-kind-interface-attribute
  (t :foreground "gray78"))

(define-attribute symbol-kind-function-attribute
  (t :foreground "LightSkyBlue"))

(define-attribute symbol-kind-variable-attribute
  (t :foreground "LightGoldenrod"))

(define-attribute symbol-kind-constant-attribute
  (t :foreground "yellow2"))

(define-attribute symbol-kind-string-attribute
  (t :foreground "green"))

(define-attribute symbol-kind-number-attribute
  (t :foreground "yellow"))

(define-attribute symbol-kind-boolean-attribute
  (t :foreground "honeydew3"))

(define-attribute symbol-kind-array-attribute
  (t :foreground "red"))

(define-attribute symbol-kind-object-attribute
  (t :foreground "PeachPuff4"))

(define-attribute symbol-kind-key-attribute
  (t :foreground "lime green"))

(define-attribute symbol-kind-null-attribute
  (t :foreground "gray"))

(define-attribute symbol-kind-enum-membe-attribute
  (t :foreground "PaleTurquoise4"))

(define-attribute symbol-kind-struct-attribute
  (t :foreground "turquoise4"))

(define-attribute symbol-kind-event-attribute
  (t :foreground "aquamarine1"))

(define-attribute symbol-kind-operator-attribute
  (t :foreground "SeaGreen3"))

(define-attribute symbol-kind-type-attribute
  (t :foreground "moccasin"))

(defun preview-symbol-kind-colors ()
  (let* ((buffer (make-buffer "symbol-kind-colors"))
         (point (buffer-point buffer)))
    (dolist (attribute
             (list 'symbol-kind-file-attribute
                   'symbol-kind-module-attribute
                   'symbol-kind-namespace-attribute
                   'symbol-kind-package-attribute
                   'symbol-kind-class-attribute
                   'symbol-kind-method-attribute
                   'symbol-kind-property-attribute
                   'symbol-kind-field-attribute
                   'symbol-kind-constructor-attribute
                   'symbol-kind-enum-attribute
                   'symbol-kind-interface-attribute
                   'symbol-kind-function-attribute
                   'symbol-kind-variable-attribute
                   'symbol-kind-constant-attribute
                   'symbol-kind-string-attribute
                   'symbol-kind-number-attribute
                   'symbol-kind-boolean-attribute
                   'symbol-kind-array-attribute
                   'symbol-kind-object-attribute
                   'symbol-kind-key-attribute
                   'symbol-kind-null-attribute
                   'symbol-kind-enum-membe-attribute
                   'symbol-kind-struct-attribute
                   'symbol-kind-event-attribute
                   'symbol-kind-operator-attribute
                   'symbol-kind-type-attribute))
      (insert-string point (string-downcase attribute) :attribute attribute)
      (insert-character point #\newline))))

(defun provide-document-symbol-p (workspace)
  (handler-case (protocol:server-capabilities-document-symbol-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun symbol-kind-to-string-and-attribute (symbol-kind)
  (switch (symbol-kind :test #'=)
    (protocol:symbol-kind.file
     (values "File" 'symbol-kind-file-attribute))
    (protocol:symbol-kind.module
     (values "Module" 'symbol-kind-module-attribute))
    (protocol:symbol-kind.namespace
     (values "Namespace" 'symbol-kind-namespace-attribute))
    (protocol:symbol-kind.package
     (values "Package" 'symbol-kind-package-attribute))
    (protocol:symbol-kind.class
     (values "Class" 'symbol-kind-class-attribute))
    (protocol:symbol-kind.method
     (values "Method" 'symbol-kind-method-attribute))
    (protocol:symbol-kind.property
     (values "Property" 'symbol-kind-property-attribute))
    (protocol:symbol-kind.field
     (values "Field" 'symbol-kind-field-attribute))
    (protocol:symbol-kind.constructor
     (values "Constructor" 'symbol-kind-constructor-attribute))
    (protocol:symbol-kind.enum
     (values "Enum" 'symbol-kind-enum-attribute))
    (protocol:symbol-kind.interface
     (values "Interface" 'symbol-kind-interface-attribute))
    (protocol:symbol-kind.function
     (values "Function" 'symbol-kind-function-attribute))
    (protocol:symbol-kind.variable
     (values "Variable" 'symbol-kind-variable-attribute))
    (protocol:symbol-kind.constant
     (values "Constant" 'symbol-kind-constant-attribute))
    (protocol:symbol-kind.string
     (values "String" 'symbol-kind-string-attribute))
    (protocol:symbol-kind.number
     (values "Number" 'symbol-kind-number-attribute))
    (protocol:symbol-kind.boolean
     (values "Boolean" 'symbol-kind-boolean-attribute))
    (protocol:symbol-kind.array
     (values "Array" 'symbol-kind-array-attribute))
    (protocol:symbol-kind.object
     (values "Object" 'symbol-kind-object-attribute))
    (protocol:symbol-kind.key
     (values "Key" 'symbol-kind-key-attribute))
    (protocol:symbol-kind.null
     (values "Null" 'symbol-kind-null-attribute))
    (protocol:symbol-kind.enum-member
     (values "EnumMember" 'symbol-kind-enum-member-attribute))
    (protocol:symbol-kind.struct
     (values "Struct" 'symbol-kind-struct-attribute))
    (protocol:symbol-kind.event
     (values "Event" 'symbol-kind-event-attribute))
    (protocol:symbol-kind.operator
     (values "Operator" 'symbol-kind-operator-attribute))
    (protocol:symbol-kind.type-parameter
     (values "TypeParameter" 'symbol-kind-type-attribute))))

(define-attribute document-symbol-detail-attribute
  (t :foreground "gray"))

(defun append-document-symbol-item (sourcelist buffer document-symbol nest-level)
  (let ((selection-range (protocol:document-symbol-selection-range document-symbol))
        (range (protocol:document-symbol-range document-symbol)))
    (lem.sourcelist:append-sourcelist
     sourcelist
     (lambda (point)
       (multiple-value-bind (kind-name attribute)
           (symbol-kind-to-string-and-attribute (protocol:document-symbol-kind document-symbol))
         (insert-string point (make-string (* 2 nest-level) :initial-element #\space))
         (insert-string point (format nil "[~A]" kind-name) :attribute attribute)
         (insert-character point #\space)
         (insert-string point (protocol:document-symbol-name document-symbol))
         (insert-string point " ")
         (when-let (detail (handler-case (protocol:document-symbol-detail document-symbol)
                             (unbound-slot () nil)))
           (insert-string point detail :attribute 'document-symbol-detail-attribute))))
     (lambda (set-buffer-fn)
       (funcall set-buffer-fn buffer)
       (let ((point (buffer-point buffer)))
         (move-to-lsp-position point (protocol:range-start selection-range))))
     :highlight-overlay-function (lambda (point)
                                   (with-point ((start point)
                                                (end point))
                                     (make-overlay
                                      (move-to-lsp-position start (protocol:range-start range))
                                      (move-to-lsp-position end (protocol:range-end range))
                                      'lem.sourcelist::jump-highlight)))))
  (lem-utils:do-sequence
      (document-symbol
       (handler-case (protocol:document-symbol-children document-symbol)
         (unbound-slot () nil)))
    (append-document-symbol-item sourcelist buffer document-symbol (1+ nest-level))))

(defun display-document-symbol-response (buffer value)
  (lem.sourcelist:with-sourcelist (sourcelist "*Document Symbol*")
    (lem-utils:do-sequence (item value)
      (append-document-symbol-item sourcelist buffer item 0))))

(defun text-document/document-symbol (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-document-symbol-p workspace)
      (request:request
       (workspace-client workspace)
       (make-instance 'request:document-symbol
                      :params (make-instance
                               'protocol:document-symbol-params
                               :text-document (make-text-document-identifier buffer)))))))

(define-command lsp-document-symbol () ()
  (check-connection)
  (display-document-symbol-response
   (current-buffer)
   (text-document/document-symbol (current-buffer))))

;;; code action
;; TODO
;; - codeAction.diagnostics
;; - codeAction.isPreferred

(defun provide-code-action-p (workspace)
  (handler-case (protocol:server-capabilities-code-action-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun execute-command (workspace command)
  ;; TODO
  ;; レスポンスを見てなんらかの処理をする必要がある
  ;; この機能はgoplsで使われる事が今のところないので動作テストをできていない
  (request:request
   (workspace-client workspace)
   (make-instance 'request:execute-command
                  :params (make-instance 'protocol:execute-command-params
                                         :command (protocol:command-command command)
                                         :arguments (protocol:command-arguments command)))))

(defun execute-code-action (workspace code-action)
  (handler-case (protocol:code-action-edit code-action)
    (unbound-slot () nil)
    (:no-error (workspace-edit)
      (apply-workspace-edit workspace-edit)))
  (handler-case (protocol:code-action-command code-action)
    (unbound-slot () nil)
    (:no-error (command)
      (execute-command workspace command))))

(defun convert-code-actions (code-actions workspace)
  (let ((items '()))
    (lem-utils:do-sequence (code-action code-actions)
      (push (context-menu:make-item :label (protocol:code-action-title code-action)
                                    :callback (curry #'execute-code-action workspace code-action))
            items))
    (nreverse items)))

(defun text-document/code-action (point)
  (flet ((point-to-line-range (point)
           (with-point ((start point)
                        (end point))
             (line-start start)
             (line-end end)
             (make-lsp-range start end))))
    (when-let ((workspace (get-workspace-from-point point)))
      (when (provide-code-action-p workspace)
        (request:request
         (workspace-client workspace)
         (make-instance
          'request:code-action
          :params (make-instance
                   'protocol:code-action-params
                   :text-document (make-text-document-identifier (point-buffer point))
                   :range (point-to-line-range point)
                   :context (make-instance 'protocol:code-action-context
                                           :diagnostics (json:json-array)))))))))

(define-command lsp-code-action () ()
  (check-connection)
  (let ((response (text-document/code-action (current-point)))
        (workspace (buffer-workspace (current-buffer))))
    (cond ((typep response 'protocol:command)
           (execute-command workspace response))
          ((and (json:json-array-p response)
                (not (length= response 0)))
           (context-menu:display-context-menu
            (convert-code-actions response
                                  workspace)))
          (t
           (message "No suggestions from code action")))))

;;; formatting

(defun provide-formatting-p (workspace)
  (handler-case (protocol:server-capabilities-document-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun make-formatting-options (buffer)
  (make-instance
   'protocol:formatting-options
   :tab-size (variable-value 'tab-width :buffer buffer)
   :insert-spaces (not (variable-value 'indent-tabs-mode :buffer buffer))
   :trim-trailing-whitespace t
   :insert-final-newline t
   :trim-final-newlines t))

(defun text-document/formatting (buffer)
  (when-let ((workspace (buffer-workspace buffer)))
    (when (provide-formatting-p workspace)
      (apply-text-edits
       buffer
       (request:request
        (workspace-client workspace)
        (make-instance 'request:document-formatting
                       :params (make-instance
                                'protocol:document-formatting-params
                                :text-document (make-text-document-identifier buffer)
                                :options (make-formatting-options buffer))))))))

(define-command lsp-document-format () ()
  (check-connection)
  (text-document/formatting (current-buffer)))

;;; range formatting

;; WARNING: goplsでサポートされていないので動作未確認

(defun provide-range-formatting-p (workspace)
  (handler-case (protocol:server-capabilities-document-range-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/range-formatting (start end)
  (when (point< end start) (rotatef start end))
  (let ((buffer (point-buffer start)))
    (when-let ((workspace (buffer-workspace buffer)))
      (when (provide-range-formatting-p workspace)
        (apply-text-edits
         buffer
         (request:request
          (workspace-client workspace)
          (make-instance 'request:document-range-formatting
                         :params (make-instance
                                  'protocol:document-range-formatting-params
                                  :text-document (make-text-document-identifier buffer)
                                  :range (make-lsp-range start end)
                                  :options (make-formatting-options buffer)))))))))

(define-command lsp-document-range-format (start end) ("r")
  (check-connection)
  (text-document/range-formatting start end))

;;; onTypeFormatting

;; TODO
;; - バッファの初期化時にtext-document/on-type-formattingを呼び出すフックを追加する

(defun provide-on-type-formatting-p (workspace)
  (handler-case (protocol:server-capabilities-document-on-type-formatting-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/on-type-formatting (point typed-character)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-on-type-formatting-p workspace)
      (when-let ((response
                  (with-jsonrpc-error ()
                    (request:request
                     (workspace-client workspace)
                     (make-instance 'request:document-on-type-formatting
                                    :params (apply #'make-instance
                                                   'protocol:document-on-type-formatting-params
                                                   :ch typed-character
                                                   :options (make-formatting-options (point-buffer point))
                                                   (make-text-document-position-arguments point)))))))
        (apply-text-edits (point-buffer point) response)))))

;;; rename

;; TODO
;; - prepareSupport

(defun provide-rename-p (workspace)
  (handler-case (protocol:server-capabilities-rename-provider
                 (workspace-server-capabilities workspace))
    (unbound-slot () nil)))

(defun text-document/rename (point new-name)
  (when-let ((workspace (get-workspace-from-point point)))
    (when (provide-rename-p workspace)
      (when-let ((response
                  (with-jsonrpc-error ()
                    (request:request
                     (workspace-client workspace)
                     (make-instance 'request:rename
                                    :params (apply #'make-instance
                                                   'protocol:rename-params
                                                   :new-name new-name
                                                   (make-text-document-position-arguments point)))))))
        (apply-workspace-edit response)))))

(define-command lsp-rename (new-name) ("sNew name: ")
  (check-connection)
  (text-document/rename (current-point) new-name))

;;;
(define-command lsp-restart-server () ()
  (when-let ((spec (buffer-language-spec (current-buffer))))
    (kill-server-process spec)
    (ensure-lsp-buffer (current-buffer))))

;;;
(defmacro define-language-spec ((spec-name major-mode) &body initargs)
  `(progn
     (register-language-spec ',major-mode ',spec-name)
     ,(when (mode-hook major-mode)
        `(add-hook ,(mode-hook major-mode) 'lsp-mode))
     (defclass ,spec-name (spec) ()
       (:default-initargs ,@initargs))))

(define-language-spec (go-spec lem-go-mode:go-mode)
  :language-id "go"
  :root-uri-patterns '("go.mod")
  :command (lambda (port) `("gopls" "serve" "-port" ,(princ-to-string port)))
  :mode :tcp)

(define-language-spec (js-spec lem-js-mode:js-mode)
  :language-id "javascript"
  :root-uri-patterns '("package.json" "tsconfig.json")
  :command '("typescript-language-server" "--stdio")
  :mode :stdio)

(define-language-spec (rust-spec lem-rust-mode:rust-mode)
  :language-id "rust"
  :root-uri-patterns '("Cargo.toml")
  :command '("rls")
  :mode :stdio)

(defun find-dart-bin-path ()
  (multiple-value-bind (output error-output status)
      (uiop:run-program '("which" "dart")
                        :output :string
                        :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop status)
        (namestring
         (uiop:pathname-directory-pathname
          (string-right-trim '(#\newline) output)))
        nil)))

(defun find-dart-language-server ()
  (let ((program-name "analysis_server.dart.snapshot"))
    (when-let (path (find-dart-bin-path))
      (let ((result
              (string-right-trim
               '(#\newline)
               (uiop:run-program (list "find" path "-name" program-name)
                                 :output :string))))
        (when (search program-name result)
          result)))))

(define-language-spec (dart-spec lem-dart-mode:dart-mode)
  :language-id "dart"
  :root-uri-patterns '("pubspec.yaml")
  :mode :stdio)

(defmethod spec-command ((spec dart-spec))
  (if-let ((lsp-path (find-dart-language-server)))
    (list "dart" lsp-path "--lsp")
    (editor-error "dart language server not found")))

(defmethod spec-initialization-options ((spec dart-spec))
  (json:make-json "onlyAnalyzeProjectsWithOpenFiles" (json:json-true)
                  "suggestFromUnimportedLibraries" (json:json-true)))

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
- [X] documentHighlight
- [X] documentSymbol
- [X] codeAction
- [ ] codeLens
- [ ] codeLens resolve
- [ ] documentLink
- [ ] documentLink resolve
- [ ] documentColor
- [ ] colorPresentation
- [X] formatting
- [X] rangeFormatting
- [X] onTypeFormatting
- [X] rename
- [ ] prepareRename
- [ ] foldingRange
- [ ] selectionRange

TODO
- partialResult
- workDoneProgress
|#
