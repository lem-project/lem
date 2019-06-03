(in-package :lem-scheme-mode)

(define-major-mode scheme-repl-mode scheme-mode
    (:name "scheme-repl"
     :keymap *scheme-repl-mode-keymap*)
  (unless (eq (scheme-process-buffer) (current-buffer))
    (message "No connection for repl. Did you mean 'start-scheme-repl' command?")))

(define-key *scheme-repl-mode-keymap* "Return" 'scheme-eval-or-newline)

(define-command start-scheme-repl () ()
  (scheme-run-process)
  (setf (current-window) (pop-to-buffer (scheme-process-buffer))))

(define-command scheme-eval-or-newline () ()
  (cond
    ((and (eq (scheme-process-buffer) (current-buffer))
          (point< *scheme-last-input-point* (current-point))
          (repl-paren-correspond-p (current-point)))
     (scheme-eval-region *scheme-last-input-point* (current-point)))
    (t
     (insert-character (current-point) #\newline))))

(defun repl-paren-correspond-p (point)
  (with-point ((start *scheme-last-input-point*))
    (let ((state (parse-partial-sexp start point)))
      (>= 0 (pps-state-paren-depth state)))))

