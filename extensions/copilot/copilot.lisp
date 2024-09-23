(defpackage :lem-copilot
  (:use :cl :lem)
  (:local-nicknames (:copilot :lem-copilot/internal)))
(in-package :lem-copilot)

(defmethod copilot:copilot-root ()
  (merge-pathnames "copilot/" (lem-home)))

(defvar *agent* nil)

(defun setup-agent ()
  (let ((agent (copilot:run-agent)))
    (copilot:connect agent)
    (copilot:initialize agent)
    (copilot:set-editor-info agent)
    agent))

(defun agent ()
  (unless *agent*
    (setf *agent* (setup-agent)))
  *agent*)

(defun enable-copilot-p ()
  (config :copilot))

(defun enable-copilot ()
  (setf (config :copilot) t))


(define-command copilot-install-server () ()
  (let* ((buffer (make-buffer "*copilot-install-server*"))
         (command (list "npm"
                        "-g"
                        "--prefix"
                        (namestring (copilot:copilot-root))
                        "install"
                        "copilot-node-server@1.27.0")))
    (erase-buffer buffer)
    (pop-to-buffer buffer)
    (with-point ((point (buffer-point buffer) :left-inserting))
      (with-open-stream (output (make-editor-output-stream point))
        (format output "~{~A ~}~%" command)
        (redraw-display)
        (uiop:run-program command
                          :output output
                          :error-output output)))))

(defun installed-copilot-server-p ()
  (uiop:file-exists-p (copilot:copilot-path)))


;;; login
(defun make-verification-buffer (user-code verification-uri)
  (let* ((buffer (make-buffer "*GitHub Copilot Verification*" :temporary t))
         (point (buffer-point buffer)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string point
                   (format nil
                           "Code: ~A (Copied to clipboard) ~%please paste it into your browser.~%~A~2%"
                           user-code
                           verification-uri))
    (insert-string point "Authenticate... (Close this window with Escape or C-g.)")
    (buffer-start point)
    buffer))

(defvar *login-message* nil)

(defun start-login (user-code verification-uri)
  (setf *login-message* (display-popup-message
                         (make-verification-buffer user-code verification-uri)
                         :style '(:gravity :center)
                         :timeout nil))
  (add-hook *editor-abort-hook* 'abort-login))

(defun abort-login ()
  (delete-login-message)
  (remove-hook *editor-abort-hook* 'abort-login))

(defun delete-login-message ()
  (when *login-message*
    (delete-popup-message *login-message*)
    (setf *login-message* nil)))

(define-command copilot-login () ()
  (setf *agent* nil)
  (let* ((agent (agent))
         (response (copilot:sign-in-initiate agent))
         (status (gethash "status" response))
         (user-code (gethash "userCode" response))
         (verification-uri (gethash "verificationUri" response))
         (user (gethash "user" response)))
    (when (equal status "AlreadySignedIn")
      (editor-error "Already sign in as ~A" user))
    (copy-to-clipboard user-code)
    (start-login user-code verification-uri)
    (open-external-file verification-uri)
    (redraw-display)
    (let ((finished nil))
      (copilot:sign-in-confirm
       agent
       user-code
       :callback (lambda (response)
                   (send-event (lambda ()
                                 (assert (equal "OK" (gethash "status" response)))
                                 (show-message (format nil "Authenticated as ~A" (gethash "user" response))
                                               :style '(:gravity :center))
                                 (delete-login-message)
                                 (setf finished t)
                                 (redraw-display))))
       :error-callback (lambda (response)
                         (send-event (lambda ()
                                       (setf finished t)
                                       (show-message (copilot:pretty-json response)
                                                     :style '(:gravity :center))
                                       (redraw-display)))))
      (handler-bind ((editor-abort (lambda (c)
                                     (declare (ignore c))
                                     (delete-login-message))))

        (loop :until finished
              :do (sit-for 1)))
      (enable-copilot))))


;;; utils
(defvar *language-id-map* (make-hash-table :test 'eq))

(defun register-language-id (mode language-id)
  (setf (gethash mode *language-id-map*) language-id))

(defun buffer-language-id (buffer)
  (gethash (buffer-major-mode buffer) *language-id-map* "text"))

(defun buffer-uri (buffer)
  (lem-lsp-mode::buffer-uri buffer))

(defun buffer-version (buffer)
  (buffer-value buffer 'version 0))

(defun (setf buffer-version) (version buffer)
  (setf (buffer-value buffer 'version) version))

(defun buffer-last-version (buffer)
  (buffer-value buffer 'last-version))

(defun (setf buffer-last-version) (last-version buffer)
  (setf (buffer-value buffer 'last-version) last-version))

(defun buffer-update-version-p (buffer)
  (not (equal (buffer-version buffer)
              (buffer-last-version buffer))))

(defun point-to-lsp-position (point)
  (copilot:hash "line" (1- (line-number-at-point point))
                "character" (point-charpos point)))

(defun move-to-lsp-position (point position)
  (move-to-line point (1+ (gethash "line" position)))
  (line-offset point 0 (gethash "character" position)))

(defun text-document-params (buffer)
  (list :uri (buffer-uri buffer)
        :language-id (buffer-language-id buffer)
        :version (buffer-version buffer)
        :text (buffer-text buffer)))

(defun notify-text-document/did-open (buffer)
  (apply #'copilot:text-document/did-open
         (agent)
         (text-document-params buffer)))

(defun notify-text-document/did-close (buffer)
  (copilot:text-document/did-close (agent) :uri (buffer-uri buffer)))

(defun notify-text-document/did-focus (buffer)
  (copilot:text-document/did-focus (agent) :uri (buffer-uri buffer)))

(defun notify-text-document/did-change (buffer content-changes)
  (let ((version (incf (buffer-version buffer))))
    (copilot:text-document/did-change (agent)
                                      :uri (buffer-uri buffer)
                                      :version version
                                      :content-changes content-changes)))


;;; copilot-mode
(define-minor-mode copilot-mode
    (:name "Copilot"
     :enable-hook 'copilot-mode-on
     :disable-hook 'copilot-mode-off))

(defun copilot-mode-on ()
  (unless (installed-copilot-server-p)
    (copilot-install-server))
  (unless *agent*
    (handler-case (copilot-login) (editor-error ())))
  (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (add-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
  (add-hook *window-show-buffer-functions* 'on-window-show-buffer)
  (add-hook *switch-to-window-hook* 'on-switch-to-window)
  (add-hook *post-command-hook* 'on-post-command)
  (notify-text-document/did-open (current-buffer)))

(defun copilot-mode-off ()
  (remove-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (remove-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
  (remove-hook *window-show-buffer-functions* 'on-window-show-buffer)
  (remove-hook *switch-to-window-hook* 'on-switch-to-window))

(defun copilot-mode-p (buffer)
  (mode-active-p buffer 'copilot-mode))

(defun on-kill-buffer (buffer)
  (when (copilot-mode-p buffer)
    (notify-text-document/did-close buffer)))

(defun before-change-arg-to-content-change (point arg)
  (etypecase arg
    (string
     (let ((position (point-to-lsp-position point)))
       (copilot:hash "range" (copilot:hash "start" position
                                           "end" position)
                     "text" arg)))
    (integer
     (with-point ((end point))
       (character-offset end arg)
       (copilot:hash "range" (copilot:hash "start" (point-to-lsp-position point)
                                           "end" (point-to-lsp-position end))
                     "text" "")))))

(defun on-before-change (point arg)
  (let ((buffer (point-buffer point)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-change buffer
                                       (vector (before-change-arg-to-content-change point
                                                                                    arg))))))

(defun on-window-show-buffer (window)
  (let ((buffer (window-buffer window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun on-switch-to-window (previous-window current-window)
  (declare (ignore previous-window))
  (let ((buffer (window-buffer current-window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun on-post-command ()
  (when (and (copilot-mode-p (current-buffer))
             (buffer-update-version-p (current-buffer)))
    (copilot-complete)))


;;; complete
(defun show-completion (display-text)
  (let ((lem-lsp-mode:*inhibit-highlight-diagnotics* t))
    (unwind-protect
         (progn
           (buffer-undo-boundary (current-buffer))
           (save-excursion
             (insert-string (current-point)
                            display-text
                            :attribute (make-attribute :foreground "dim gray")))
           (loop :for v := (sit-for 10)
                 :while (eq v :timeout)
                 :finally (if (equal (princ-to-string v) "Tab")
                              (return-from show-completion t)
                              (error 'editor-abort :message nil))))
      (buffer-undo (current-point))
      nil)))

(defun replace-with-completion (point completion)
  (let* ((range (gethash "range" completion)))
    (with-point ((start point)
                 (end point))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (delete-between-points start end)
      (insert-string start (gethash "text" completion)))))

(defun show-and-apply-completion (completions)
  (let ((completion (first completions)))
    (copilot:notify-shown (agent) (gethash "uuid" completion))
    (when (show-completion (gethash "displayText" completion))
      (replace-with-completion (current-point) completion))))

(defun compute-relative-path (buffer)
  (if (buffer-filename buffer)
      (enough-namestring (buffer-filename buffer)
                         (lem-core/commands/project:find-root (buffer-filename buffer)))
      ""))

(defun make-doc (point)
  (let ((buffer (point-buffer point)))
    (copilot:hash "version" (buffer-version buffer)
                  "source" (buffer-text buffer)
                  "tabSize" (variable-value 'tab-width :default point)
                  "indentSize" 4
                  "insertSpaces" (if (variable-value 'indent-tabs-mode :default point)
                                     'yason:true
                                     'yason:false)
                  "path" (buffer-filename buffer)
                  "uri" (buffer-uri buffer)
                  "relativePath" (compute-relative-path buffer)
                  "languageId" (buffer-language-id buffer)
                  "position" (point-to-lsp-position point))))

(defun get-completions (point)
  (let ((buffer (point-buffer point)))
    (setf (buffer-last-version buffer) (buffer-version buffer))
    (copilot:get-completions
     (agent)
     :doc (make-doc point)
     :callback (lambda (response)
                 (send-event (lambda ()
                               (defparameter $get-completions-response response)
                               (alexandria:when-let (completions (gethash "completions" response))
                                 (show-and-apply-completion completions)
                                 (redraw-display)))))
     :error-callback (lambda (response)
                       (send-event (lambda ()
                                     (show-message (copilot:pretty-json response))))))))

(define-command copilot-complete () ()
  (get-completions (current-point)))


;;; test
(define-command test/copilot-document () ()
  (let ((response (copilot::request (agent)
                                    "testing/getDocument"
                                    (copilot:hash "uri" (buffer-uri (current-buffer))))))
    (show-message (copilot:pretty-json response))
    (assert (equal (gethash "text" response)
                   (buffer-text (current-buffer))))))

(defun enable-copilot-mode ()
  (when (enable-copilot-p)
    (copilot-mode t)))

(defmacro define-language (mode (&key (language-id (alexandria:required-argument :language-id))))
  `(progn
     (add-hook ,(mode-hook-variable mode) 'enable-copilot-mode)
     (register-language-id ',mode ,language-id)))

(define-language lem-js-mode:js-mode (:language-id "javascript"))
(define-language lem-rust-mode:rust-mode (:language-id "rust"))
(define-language lem-go-mode:go-mode (:language-id "go"))
(define-language lem-lisp-mode:lisp-mode (:language-id "lisp"))
(define-language lem-markdown-mode:markdown-mode (:language-id "markdown"))
(define-language lem-c-mode:c-mode (:language-id "c"))
(define-language lem-css-mode:css-mode (:language-id "css"))
(define-language lem-dart-mode:dart-mode (:language-id "dart"))
(define-language lem-json-mode:json-mode (:language-id "json"))
(define-language lem-lua-mode:lua-mode (:language-id "lua"))
(define-language lem-nim-mode:nim-mode (:language-id "nim"))
(define-language lem-ocaml-mode:ocaml-mode (:language-id "ocaml"))
(define-language lem-python-mode:python-mode (:language-id "python"))
(define-language lem-scala-mode:scala-mode (:language-id "scala"))
(define-language lem-scheme-mode:scheme-mode (:language-id "scheme"))
(define-language lem-sql-mode:sql-mode (:language-id "sql"))
(define-language lem-terraform-mode:terraform-mode (:language-id "hcl"))
(define-language lem-typescript-mode:typescript-mode (:language-id "typescript"))
(define-language lem-xml-mode:xml-mode (:language-id "xml"))
(define-language lem-yaml-mode:yaml-mode (:language-id "yaml"))
