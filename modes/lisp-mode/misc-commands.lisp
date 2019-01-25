(in-package :lem-lisp-mode)

(define-key *lisp-mode-keymap* "C-c C-q" 'lisp-quickload)

(define-command lisp-quickload (system-name)
    ((list (prompt-for-symbol-name "System: " (buffer-package (current-buffer)))))
  (check-connection)
  (eval-with-transcript `(ql:quickload ,(string system-name))))
