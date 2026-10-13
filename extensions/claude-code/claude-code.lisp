(uiop:define-package :lem-claude-code
  (:use :cl
        :lem)
  (:local-nicknames (:claude-code :lem-claude-code/claude-code-sdk)))
(in-package :lem-claude-code)

(define-attribute claude-response-attribute
  (:dark :foreground "white" :bold t)
  (:light :foreground "black" :bold t))

(define-attribute claude-tool-use-attribute
  (:dark :foreground "white" :bold t)
  (:light :foreground "black" :bold t))

(define-attribute prompt-attribute
  (t :foreground "dim gray"))

(define-major-mode claude-code-output-mode ()
    (:name "Claude Code"
     :keymap *claude-code-output-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t)
  (setf (variable-value 'wrap-line-character :buffer (current-buffer)) #\space))

(define-key *claude-code-output-mode-keymap* "Return" 'claude-code-press-button)

(defun make-content (string &optional attribute)
  (let ((string (str:pad-left 10 string)))
    (if attribute
        (lem/buffer/line:make-content :string string :attributes `((0 ,(length string) ,attribute)))
        (lem/buffer/line:make-content :string string))))

(defmethod lem:compute-left-display-area-content ((mode claude-code-output-mode) buffer point)
  (or (case (lem/interactive-mode:prompt-region-p point)
        (:first-line (make-content " You: " 'prompt-attribute))
        (:continuing-line (make-content "   ")))
      (when (output-first-line-p (lem/interactive-mode::session buffer) point)
        (make-content " Claude: " 'claude-response-attribute))
      (make-content "")))

(defmethod lem-core:compute-wrap-left-area-content ((mode claude-code-output-mode) left-side-width left-side-characters)
  (if (< 0 left-side-width)
      (list (lem-core::make-object-with-type
             (make-string left-side-characters :initial-element #\space)
             nil
             (lem-core::char-type #\space)))
      nil))

(define-major-mode claude-code-query-mode ()
    (:name "Claude Code Query")
  (setf (variable-value 'highlight-line :buffer (current-buffer)) nil))

(defmethod lem/interactive-mode:prompt (session (mode claude-code-query-mode))
  (values " > "
          (make-attribute :bold t)))

(defclass context (lem/interactive-mode:context)
  ((session-id :initarg :session-id
               :initform nil
               :accessor context-session-id)
   (output-log :initform '()
               :accessor context-output-log)
   (tool-map :initform (make-hash-table :test 'equal)
             :reader context-tool-map)
   (output-first-line-points :initform '()
                             :accessor context-output-first-line-points)))

(defun debug-log (text)
  (let ((buffer (make-buffer "*Claude Code Output*")))
    (insert-string (buffer-end-point buffer)
                   text)))

(defun session-id (session)
  (let ((context (lem/interactive-mode:session-context session)))
    (context-session-id context)))

(defun update-session-id (session session-id)
  (let ((context (lem/interactive-mode:session-context session)))
    (setf (context-session-id context)
          session-id)))

(defun do-output-log (session value)
  (let ((context (lem/interactive-mode:session-context session)))
    (push value (context-output-log context))))

(defun to-json-string (object)
  (yason:with-output-to-string* (:stream-symbol out)
    (yason:encode object out)))

(defun string-match (expected strings)
  (member expected (uiop:ensure-list strings) :test #'string=))

(defun render-tool-use-content (session content point &key is-open)
  (let* ((content-start (copy-point point :right-inserting))
         (content-end (copy-point point :right-inserting)))
    (let ((tool-use-id (gethash "id" content))
          (context (lem/interactive-mode:session-context session)))
      (lem/button:insert-button point
                                (if is-open
                                    (lem:icon-string "down-pointing-triangle")
                                    (lem:icon-string "right-pointing-triangle"))
                                (lambda ()
                                  (save-excursion
                                    (with-buffer-read-only (point-buffer content-start) nil
                                      (delete-between-points content-start content-end)
                                      (with-point ((point content-start :left-inserting))
                                        (render-tool-use-content session content point :is-open (not is-open)))
                                      (delete-point content-start)
                                      (delete-point content-end)))))
      (insert-string point (gethash "name" content) :attribute 'claude-tool-use-attribute)
      (insert-string point "(")
      (loop :for key :being :the :hash-keys :of (gethash "input" content) :using (:hash-value value)
            :for first := t :then nil
            :do (unless first
                  (insert-string point ", "))
                (insert-string point key)
                (insert-string point ": ")
                (insert-string point (to-json-string value)))
      (insert-string point ")")
      (when is-open
        (insert-character point #\newline)
        (let ((content (gethash tool-use-id (context-tool-map context))))
          (render-tool-result-content session content point :is-open t)))
      (move-point content-end point))))

(defun render-tool-result-content (session content point &key is-open)
  (let ((context (lem/interactive-mode:session-context session))
        (content-text (gethash "content" content))
        (tool-use-id (gethash "tool_use_id" content)))
    (setf (gethash tool-use-id (context-tool-map context)) content)
    (when is-open
      (lem-lisp-mode/internal::insert-escape-sequence-string point content-text)
      (insert-character point #\newline))))

(defun output-first-line-p (session point)
  (let ((context (lem/interactive-mode:session-context session)))
    (dolist (p (context-output-first-line-points context))
      (when (same-line-p p point)
        (return t)))))

(defun save-point (session stream)
  (let ((context (lem/interactive-mode:session-context session)))
    (push (copy-point (lem-core::buffer-stream-point stream) :right-inserting)
          (context-output-first-line-points context))))

(defun render-claude-code-message (session value)
  (with-buffer-read-only (lem/interactive-mode::session-output-buffer session) nil
    (lem/interactive-mode:with-output-stream (stream session)
      (fresh-line stream)
      (alexandria:switch ((gethash "type" value) :test #'string-match)
        ("result"
         (lem/interactive-mode:stop-loading session))
        ('("assistant" "user")
         (dolist (content (gethash "content" (gethash "message" value)))
           (alexandria:switch ((gethash "type" content) :test #'string=)
             ("text"
              (save-point session stream)
              (lem/interactive-mode:with-attribute
                  (session 'claude-response-attribute)
                (write-line (gethash "text" content) stream)))
             ("tool_use"
              (save-point session stream)
              (render-tool-use-content session content (lem-core::buffer-stream-point stream)))
             ("tool_result"
              (render-tool-result-content session content (lem-core::buffer-stream-point stream)))))))
      (fresh-line stream))))

(define-command claude-code-press-button () ()
  (let ((button (lem/button:button-at (current-point))))
    (when button (lem/button:button-action button))))

(defun handle-response (session value)
  (do-output-log session value)
  (debug-log (with-output-to-string (out)
               (yason:encode value out)
               (terpri out)))
  (update-session-id session (gethash "session_id" value))
  (render-claude-code-message session value))

(defmethod lem/interactive-mode:execute-input (session (mode claude-code-query-mode) input)
  (unless (alexandria:emptyp input)
    (lem/interactive-mode:start-loading session "Responding...")
    (claude-code:query
     input
     :callback (lambda (value)
                 (send-event (lambda ()
                               (handle-response session value))))
     :error-callback (lambda (e)
                       (lem/interactive-mode:stop-loading session)
                       (error e))
     :options (claude-code:make-options
               :resume (session-id session)
               :permission-mode "acceptEdits"))))

(define-command claude-code () ()
  (let ((context (make-instance 'context)))
    (lem/interactive-mode:run :buffer-name "*Claude Code*"
                              :input-buffer-mode 'claude-code-query-mode
                              :output-buffer-mode 'claude-code-output-mode
                              :copy-prompt-to-output-buffer nil
                              :input-text-attribute-in-output-buffer 'prompt-attribute
                              :context context)))
