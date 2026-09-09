;; https://docs.anthropic.com/ja/docs/claude-code/sdk

(uiop:define-package :lem-claude-code/claude-code-sdk
  (:use :cl
        :alexandria)
  (:export :make-options
           :query
           :receive))
(in-package :lem-claude-code/claude-code-sdk)

(defstruct (claude-code-options (:constructor make-options))
  allowed-tools
  max-thinking-tokens
  system-prompt
  append-system-prompt
  mcp-servers
  permission-mode
  continue-conversation
  resume
  max-turns
  disallowed-tools
  model
  permission-prompt-tool-name
  cwd
  settings
  add-dirs)

(defun json-encode (object &key (encode #'yason:encode))
  (with-output-to-string (out) (funcall encode object out)))

(defun construct-command (prompt options)
  `("claude"
    "--output-format" "stream-json"
    "--verbose"
    "--print" ,prompt
    ,@(when (claude-code-options-system-prompt options)
        (list "--system-prompt" (claude-code-options-system-prompt options)))
    ,@(when (claude-code-options-append-system-prompt options)
        (list "--append-system-prompt" (claude-code-options-append-system-prompt options)))
    ,@(when (claude-code-options-allowed-tools options)
        (list "--allowedTools" (format nil "~{~A~^,~}" (claude-code-options-allowed-tools options))))
    ,@(when (claude-code-options-max-turns options)
        (list "--max-turns" (write-to-string (claude-code-options-max-turns options))))
    ,@(when (claude-code-options-disallowed-tools options)
        (list "--disallowedTools"
              (format nil "~{~A~^,~}" (claude-code-options-disallowed-tools options))))
    ,@(when (claude-code-options-model options)
        (list "--model" (claude-code-options-model options)))
    ,@(when (claude-code-options-permission-prompt-tool-name options)
        (list "--permission-prompt-tool" (claude-code-options-permission-prompt-tool-name options)))
    ,@(when (claude-code-options-permission-mode options)
        (list "--permission-mode" (claude-code-options-permission-mode options)))
    ,@(when (claude-code-options-continue-conversation options)
        (list "--continue"))
    ,@(when (claude-code-options-resume options)
        (list "--resume" (claude-code-options-resume options)))
    ,@(when (claude-code-options-settings options)
        (list "--settings" (claude-code-options-settings options)))
    ,@(when (claude-code-options-add-dirs options)
        (mapcan (lambda (dir) (list "--add-dir" (princ-to-string dir)))
                (claude-code-options-add-dirs options)))
    ,@(when (claude-code-options-mcp-servers options)
        (cond
          ((consp (claude-code-options-mcp-servers options))
           (list "--mcp-config"
                 (let ((yason:*parse-object-as-alist* t))
                   (json-encode
                    (acons "mcpServers"
                           (claude-code-options-mcp-servers options)
                           nil)
                    :encode #'yason:encode-alist))))
          (t
           (list "--mcp-config" (princ-to-string (claude-code-options-mcp-servers options))))))))

(defstruct session
  process
  thread)

(defun receive-message (process)
  (check-type process async-process::process)
  (let ((buffer ""))
    (loop
      (let ((data (async-process:process-receive-output process)))
        (unless data
          (return (values buffer nil)))
        (setf buffer (concatenate 'string buffer data))
        (handler-case (yason:parse buffer)
          (error ()
            ;; If there is a parse error, the input is incomplete.
            )
          (:no-error (value &rest *)
            (return (values value t)))))
      (sleep 0.1)
      :while (async-process:process-alive-p process))))

(defun query (prompt &key (options (make-options)) callback error-callback)
  (let* ((process
           (async-process:create-process
            (construct-command prompt options))))
    (make-session
     :process process
     :thread (bt2:make-thread
              (lambda ()
                (handler-case
                    (loop
                      :do (multiple-value-bind (value success) (receive-message process)
                            (unless success
                              (error "claude code is exit: ~A" value))
                            (when callback
                              (funcall callback value))
                            (when (equal "result" (gethash "type" value))
                              (return)))
                      :while (async-process:process-alive-p process))
                  (error (e)
                    (if error-callback
                        (funcall error-callback e)
                        (error e)))))
              :name "Claude Code thread"))))

;; example
(eval-when ()
  (let ((session (query "hello")))
    (loop :for message := (receive session)
          :until (equal "result" (gethash "type" message))
          :do (print message))))

#+(or)
(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (alexandria:hash-table-alist object) stream)))
