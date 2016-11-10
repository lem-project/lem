(in-package :lem)

(export '(describe-key
          describe-bindings
          execute-command
          apropos-command))

(define-key *global-keymap* (kbd "C-x ?") 'describe-key)
(define-command describe-key () ()
  (message "describe-key: ")
  (redraw-display)
  (let* ((key (read-key-sequence))
         (cmd (find-keybind key)))
    (message "describe-key: ~a ~a"
             (kbd-to-string key)
             (command-name cmd))))

(defun describe-bindings-internal (s name keymap &optional first-p)
  (unless first-p
    (princ C-L s)
    (terpri s))
  (let ((column-width 16))
    (princ name s)
    (terpri s)
    (format s "~va~a~%" column-width "key" "binding")
    (format s "~va~a~%" column-width "---" "-------")
    (keymap-flatten-map keymap
                        (lambda (kbd command)
                          (format s "~va~a~%"
                                  column-width
                                  (kbd-to-string kbd)
                                  (symbol-name command))))
    (terpri s)))

(define-command describe-bindings () ()
  (info-popup (get-buffer-create "*bindings*")
              #'(lambda (s)
                  (describe-bindings-internal s
                                              "Major Mode Bindings"
                                              (mode-keymap (major-mode))
                                              t)
                  (describe-bindings-internal s
                                              "Global Bindings"
                                              *global-keymap*)
                  (dolist (mode (buffer-minor-modes))
                    (describe-bindings-internal s
                                                (mode-name mode)
                                                (mode-keymap mode))))))

(define-key *global-keymap* (kbd "M-x") 'execute-command)
(define-command execute-command (arg) ("P")
  (let* ((name (minibuf-read-line
                (if arg
                    (format nil "~D M-x " arg)
                    "M-x ")
                ""
                (lambda (str)
                  (if (find #\- str)
                      (completion-hypheen str (all-command-names))
                      (completion str (all-command-names))))
                'exist-command-p))
         (cmd (find-command name)))
    (if cmd
        (funcall cmd arg)
        (message "invalid command"))))

(define-command apropos-command (str) ("sApropos: ")
  (info-popup (get-buffer-create "*Apropos*")
              #'(lambda (out)
                  (dolist (name (all-command-names))
                    (when (search str name)
                      (describe (string-to-command name) out))))))
