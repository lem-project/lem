(in-package :lem-scheme-mode)

(define-major-mode scheme-repl-mode scheme-mode
    (:name "scheme-repl"
     :keymap *scheme-repl-mode-keymap*)
  (cond
    ((eq (scheme-process-buffer) (current-buffer))
     (setf (variable-value 'enable-syntax-highlight) nil)
     (lem.listener-mode:listener-mode t)
     (lem.listener-mode:listener-update-point)
     ;; disable listener-mode functions
     (setf (variable-value 'lem.listener-mode:listener-set-prompt-function)
           (lambda (point) point)
           (variable-value 'lem.listener-mode:listener-check-input-function)
           (lambda (point))
           (variable-value 'lem.listener-mode:listener-execute-function)
           (lambda (point string)))
     ;; overwrite listener-mode keymap
     (scheme-repl-input-mode t))
    (t
     (editor-error "No connection for repl. Did you mean 'start-scheme-repl' command?"))))

(define-minor-mode scheme-repl-input-mode
    (:name "scheme-repl-input"
     :keymap *scheme-repl-input-mode-keymap*))
(define-key *scheme-repl-input-mode-keymap* "Return" 'scheme-eval-or-newline)

(define-command start-scheme-repl () ()
  (scheme-run-process)
  (setf (current-window) (pop-to-buffer (scheme-process-buffer))))

(define-command scheme-switch-to-repl-buffer () ()
  (start-scheme-repl))

(define-command scheme-eval-or-newline () ()
  (cond
    ((and (eq (scheme-process-buffer) (current-buffer))
          (point<= (lem.listener-mode::listener-start-point (current-buffer))
                   (current-point))
          (repl-paren-correspond-p (current-point)))
     (let ((str (points-to-string
                 (lem.listener-mode::listener-start-point (current-buffer))
                 (current-point))))
       (lem.history:add-history (lem.listener-mode::%listener-history) str)
       (scheme-run-process-and-output-newline)
       (scheme-send-input str)))
    (t
     (insert-character (current-point) #\newline))))

(defun repl-paren-correspond-p (point)
  (with-point ((start (lem.listener-mode::listener-start-point
                       (point-buffer point))))
    (let ((state (parse-partial-sexp start point)))
      (and (not (member (lem-base::pps-state-type state)
                        '(:string :fence :block-string :block-comment)))
           (>= 0 (pps-state-paren-depth state))))))

