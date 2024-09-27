(defpackage :lem-copilot
  (:use :cl :lem)
  (:local-nicknames (:copilot :lem-copilot/internal)))
(in-package :lem-copilot)

(define-attribute suggestion-attribute
  (t :foreground "dark gray"))

(define-attribute cycling-attribute
  (t :foreground "green"))

(define-condition already-sign-in (editor-error) ())

(defmethod copilot:copilot-root ()
  (merge-pathnames "copilot/" (lem-home)))

(defvar *agent* nil)

(defun setup-agent ()
  (let ((agent (copilot:run-agent)))
    (add-hook *exit-editor-hook*
              (lambda ()
                (async-process:delete-process (copilot::agent-process agent))))
    (copilot:connect agent)
    (copilot:initialize agent)
    (copilot:initialized agent)
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
                        "copilot-node-server@1.40.0")))
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

(defun reset-buffers ()
  (dolist (buffer (remove-if-not #'copilot-mode-p (buffer-list)))
    (setf (buffer-version buffer) 0)
    (notify-text-document/did-open buffer)))

(define-command copilot-restart () ()
  (async-process:delete-process (lem-copilot/internal::agent-process lem-copilot::*agent*))
  (setf *agent* nil)
  (handler-case (copilot-login) (already-sign-in ()))
  (reset-buffers)
  (message "copilot restarted"))

(defmethod copilot:copilot-dead ()
  (display-popup-message (format nil
                                 "~{~A~^~%~}"
                                 '("Copilot has issued a warning. "
                                   "If it does not work properly, please execute `M-x copilot-restart`."
                                   ""
                                   "To view the copilot log, execute `M-x test/copilot-log`."))
                         :style '(:gravity :top)
                         :timeout 10)
  #+(or)
  (copilot-restart))


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
  (unless (installed-copilot-server-p)
    (copilot-install-server))
  (setf *agent* nil)
  (let* ((agent (agent))
         (response (copilot:sign-in-initiate agent))
         (status (gethash "status" response))
         (user-code (gethash "userCode" response))
         (verification-uri (gethash "verificationUri" response))
         (user (gethash "user" response)))
    (when (equal status "AlreadySignedIn")
      (error 'already-sign-in :message (format nil "Already sign in as ~A" user)))
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

(defun buffer-completions-cache (buffer)
  (buffer-value buffer 'completions-cache))

(defun (setf buffer-completions-cache) (completions-cache buffer)
  (setf (buffer-value buffer 'completions-cache) completions-cache))

(defun buffer-showing-suggestions-p (buffer)
  (buffer-value buffer 'showing-suggestions-p))

(defun (setf buffer-showing-suggestions-p) (showing-suggestions-p buffer)
  (setf (buffer-value buffer 'showing-suggestions-p) showing-suggestions-p))

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
     :keymap *copilot-mode-keymap*
     :enable-hook 'copilot-mode-on
     :disable-hook 'copilot-mode-off))

(define-key *copilot-mode-keymap* "M-n" 'copilot-next-suggestion)
(define-key *copilot-mode-keymap* "M-p" 'copilot-previous-suggestion)

(defun copilot-mode-on ()
  (unless (installed-copilot-server-p)
    (copilot-install-server)
    (reset-buffers))
  (unless *agent*
    (handler-case (copilot-login) (already-sign-in ())))
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

(defun on-post-command ()
  (cancel-inline-completion))

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

(defvar *inhibit-did-change-notification* nil)

(defun on-before-change (point arg)
  (let ((buffer (point-buffer point)))
    (when (and (copilot-mode-p buffer)
               (not *inhibit-did-change-notification*))
      (notify-text-document/did-change
       buffer
       (vector (before-change-arg-to-content-change point arg))))))

(defun on-window-show-buffer (window)
  (let ((buffer (window-buffer window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun on-switch-to-window (previous-window current-window)
  (declare (ignore previous-window))
  (let ((buffer (window-buffer current-window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defvar *delay-complete* 100)
(defvar *complete-timer* nil)

(defmethod execute :after ((mode copilot-mode) (command self-insert) argument)
  (cond (*delay-complete*
         (if *complete-timer*
             (stop-timer *complete-timer*)
             (setf *complete-timer* (make-idle-timer 'copilot-complete :name "Copilot Complete")))
         (start-timer *complete-timer* *delay-complete* :repeat nil))
        (t
         (copilot-complete))))


;;; complete
(defvar *inline-completion-request* nil)
(defvar *completion-canceled* nil)

(defvar *copilot-completion-keymap* (make-keymap :name "Copilot Completion"))

(define-key *copilot-completion-keymap* "Tab" 'copilot-accept-suggestion)
(define-key *copilot-completion-keymap* 'copilot-next-suggestion 'copilot-next-suggestion)
(define-key *copilot-completion-keymap* 'copilot-previous-suggestion 'copilot-previous-suggestion)

(defun find-copilot-completion-command (key)
  (lookup-keybind key
                  :keymaps (append (lem-core::all-keymaps)
                                   (list *copilot-completion-keymap*))))

(defun search-preffix (str1 str2)
  (loop :for i :from 0
        :for c1 :across str1
        :for c2 :across str2
        :while (char= c1 c2)
        :finally (return i)))

(defun replace-with-inline-completion (point item)
  (let ((range (gethash "range" item)))
    (with-point ((start point :left-inserting)
                 (end point :right-inserting))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (let* ((insert-text (gethash "insertText" item))
             (text (points-to-string start end))
             (pos (search-preffix text insert-text)))
        (character-offset start pos)
        (delete-between-points start end)
        (insert-string end
                       (subseq insert-text pos))))))

(defun preview-inline-completion-item (point item &key additional-text)
  (let ((range (gethash "range" item)))
    (with-point ((start point :left-inserting)
                 (end point :left-inserting))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (let* ((insert-text (gethash "insertText" item))
             (text (points-to-string start end))
             (pos (search-preffix text insert-text)))
        (character-offset start pos)
        (delete-between-points start end)
        (setf (buffer-showing-suggestions-p (point-buffer point)) t)
        (insert-string end
                       (subseq insert-text pos)
                       :attribute 'suggestion-attribute)
        (when additional-text
          (insert-string end " ")
          (insert-string end additional-text
                         :attribute 'cycling-attribute))))))

(defun unshow-inline-completion (point)
  (when (buffer-showing-suggestions-p (point-buffer point))
    (setf (buffer-showing-suggestions-p (point-buffer point)) nil)
    (let ((*inhibit-did-change-notification* t))
      (buffer-undo point))))

(defun elt-safety (items index)
  (if (<= (length items) index)
      (elt items (1- (length items)))
      (elt items index)))

(defun show-inline-completion (point items &key (index 0) cycling)
  (let ((item (elt-safety items index))
        (buffer (point-buffer point))
        (*inhibit-did-change-notification* t))
    (lem-lsp-mode::reset-buffer-diagnostic buffer)
    (buffer-undo-boundary buffer)
    (save-excursion
      (preview-inline-completion-item point
                                      item
                                      :additional-text (when cycling
                                                         (format nil
                                                                 "[~D/~D]"
                                                                 (1+ index)
                                                                 (length items)))))
    (loop :for v := (sit-for 10)
          :while (eq v :timeout)
          :finally (return-from show-inline-completion v))))

(defun prompt-inline-completion (point items &key (index 0) cycling)
  (when items
    (let ((buffer (point-buffer point))
          (key (show-inline-completion point items :index index :cycling cycling)))
      (case (find-copilot-completion-command key)
        (copilot-accept-suggestion
         (read-key)
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (replace-with-inline-completion point (elt-safety items index))
         (redraw-display))
        (copilot-next-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind copilot:+trigger-kind.invoked+
                            :index (mod (1+ index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (copilot-previous-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind copilot:+trigger-kind.invoked+
                            :index (mod (1- index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (self-insert
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (self-insert 1 (insertion-key-p (read-key)))
         (inline-completion point))
        (otherwise
         (unshow-inline-completion point)
         (error 'editor-abort :message nil))))))

(defun inline-completion (point &key (trigger-kind 2) (index 0) cycling show-loading-spinner)
  (setf *completion-canceled* nil)
  (let* ((buffer (point-buffer point))
         (spinner (when show-loading-spinner
                    (lem/loading-spinner:start-loading-spinner :line :point point)))
         (request
           (copilot:text-document/inline-completion
            (agent)
            :callback (lambda (response)
                        (send-event (lambda ()
                                      (when spinner
                                        (lem/loading-spinner:stop-loading-spinner spinner))
                                      (unshow-inline-completion point)
                                      (unless *completion-canceled*
                                        (prompt-inline-completion (buffer-point buffer)
                                                                  (gethash "items" response)
                                                                  :index index
                                                                  :cycling cycling)))))
            :error-callback (lambda (&rest args)
                              (declare (ignore args))
                              (unshow-inline-completion point)
                              (send-event (lambda ()
                                            (when spinner
                                              (lem/loading-spinner:stop-loading-spinner spinner)))))
            :uri (buffer-uri buffer)
            :position (point-to-lsp-position point)
            :insert-spaces (if (variable-value 'indent-tabs-mode
                                               :default buffer)
                               'yason:true
                               'yason:false)
            :tab-size (variable-value 'tab-width :default buffer)
            :trigger-kind trigger-kind)))
    (setf *inline-completion-request* request)))

(defun cancel-inline-completion ()
  (unshow-inline-completion (current-point))
  (when *inline-completion-request*
    (copilot:$/cancel-request (agent) (jsonrpc:request-id *inline-completion-request*))
    (setf *inline-completion-request* nil
          *completion-canceled* t)))

(define-command copilot-complete () ()
  (inline-completion (current-point)))

(define-command copilot-accept-suggestion () ()
  ;; dummy command
  )

(define-command copilot-next-suggestion () ()
  ;; dummy command
  )

(define-command copilot-previous-suggestion () ()
  ;; dummy command
  )


;;;
(defun enable-copilot-mode ()
  (when (and (enable-copilot-p)
             (not (buffer-temporary-p (current-buffer))))
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
(define-language lem-swift-mode:swift-mode (:language-id "swift"))


;;; test
(define-command test/copilot-document () ()
  (let ((response (copilot::request (agent)
                                    "testing/getDocument"
                                    (copilot:hash "uri" (buffer-uri (current-buffer))))))
    (show-message (copilot:pretty-json response))
    (assert (equal (gethash "text" response)
                   (buffer-text (current-buffer))))))

(define-command test/copilot-log () ()
  (let* ((buffer (make-buffer "*copilot-log*" :enable-undo-p nil)))
    (erase-buffer buffer)
    (with-open-stream (stream (make-buffer-output-stream (buffer-point buffer)))
      (copilot::write-log stream))
    (pop-to-buffer buffer)))
