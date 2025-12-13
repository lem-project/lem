(in-package :lem-mcp-server)

;;; Command Execution Tools

;; command_list - List available commands
(define-mcp-tool "command_list" (prefix)
  (:description "List available editor commands"
   :input-schema (("type" . "object")
                  ("properties" . (("prefix" . (("type" . "string")
                                                ("description" . "Optional prefix to filter commands")))))
                  ("required" . ())))
  (let ((commands (all-command-names)))
    (when prefix
      (setf commands
            (remove-if-not (lambda (name)
                             (and (>= (length name) (length prefix))
                                  (string-equal prefix name :end2 (length prefix))))
                           commands)))
    (with-output-to-string (s)
      (yason:encode (sort (copy-list commands) #'string<) s))))

;; command_execute - Execute a command
(define-mcp-tool "command_execute" (command-name args)
  (:description "Execute an editor command by name"
   :input-schema (("type" . "object")
                  ("properties" . (("command_name" . (("type" . "string")
                                                      ("description" . "Name of the command to execute")))
                                   ("args" . (("type" . "array")
                                              ("description" . "Optional arguments for the command")
                                              ("items" . (("type" . "string")))))))
                  ("required" . ("command_name"))))
  (let ((command (get-command command-name)))
    (unless command
      (mcp-error +invalid-params+
                 (format nil "Command not found: ~A" command-name)))
    (handler-case
        (progn
          (if args
              (apply #'call-command command nil args)
              (call-command command nil))
          (format nil "Command executed: ~A" command-name))
      (error (e)
        (mcp-error +server-error+
                   (format nil "Command execution failed: ~A" e))))))

;; command_info - Get information about a command
(define-mcp-tool "command_info" (command-name)
  (:description "Get information about a command"
   :input-schema (("type" . "object")
                  ("properties" . (("command_name" . (("type" . "string")
                                                      ("description" . "Name of the command")))))
                  ("required" . ("command_name"))))
  (let ((command (get-command command-name)))
    (unless command
      (mcp-error +invalid-params+
                 (format nil "Command not found: ~A" command-name)))
    (with-output-to-string (s)
      (yason:encode
       `(("name" . ,command-name)
         ("exists" . t))
       s))))

;; eval_expression - Evaluate a Lisp expression (for advanced users)
(define-mcp-tool "eval_expression" (expression package-name)
  (:description "Evaluate a Lisp expression in the editor"
   :input-schema (("type" . "object")
                  ("properties" . (("expression" . (("type" . "string")
                                                    ("description" . "Lisp expression to evaluate")))
                                   ("package_name" . (("type" . "string")
                                                      ("description" . "Package name for evaluation (default: lem)")))))
                  ("required" . ("expression"))))
  (let* ((pkg-name (or package-name "LEM"))
         (pkg (find-package (string-upcase pkg-name))))
    (unless pkg
      (mcp-error +invalid-params+
                 (format nil "Package not found: ~A" pkg-name)))
    (handler-case
        (let* ((*package* pkg)
               (form (read-from-string expression))
               (result (eval form)))
          (format nil "~S" result))
      (error (e)
        (mcp-error +server-error+
                   (format nil "Evaluation error: ~A" e))))))
