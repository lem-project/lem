(in-package :lem-scheme-mode)

(defvar *scheme-process* nil)
(defvar *newline-delay-flag* nil)

(defun scheme-process-buffer ()
  (or (get-buffer "*scheme-process*")
      (let ((buffer (make-buffer "*scheme-process*")))
        (change-buffer-mode buffer 'scheme-repl-mode)
        buffer)))

(defun scheme-output-string (string)
  (let ((buffer (scheme-process-buffer)))
    (insert-string (buffer-end-point buffer) string)
    (buffer-end (buffer-point buffer))
    (move-point (lem.listener-mode::listener-start-point buffer)
                (buffer-point buffer))))

(defun scheme-output-callback (string)
  (let ((buffer (scheme-process-buffer)))
    (scheme-output-string string)
    (when *newline-delay-flag*
      (setf *newline-delay-flag* nil)
      (scheme-output-string (string #\newline)))
    (let ((window (pop-to-buffer buffer)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window))
      (redraw-display))))

(defun scheme-run-process ()
  (when (and *scheme-process*
             (not (lem-process:process-alive-p *scheme-process*)))
    (let ((buffer (scheme-process-buffer)))
      (scheme-output-string
       (format nil "~%;; Scheme process was aborted. Restarting...~%~%"))
      (lem-process:delete-process *scheme-process*)
      (setf *scheme-process* nil)))
  (cond
    ((not *scheme-process*)
     (setf *scheme-process* (lem-process:run-process
                             *scheme-run-command*
                             :name "scheme"
                             :output-callback #'scheme-output-callback))
     t)
    (t nil)))

(defun scheme-run-process-and-output-newline ()
  "run scheme process and output newline for repl"
  (if (scheme-run-process)
      (setf *newline-delay-flag* t)
      (scheme-output-string (string #\newline))))

(define-command scheme-kill-process () ()
  (cond
    (*scheme-process*
     (lem-process:delete-process *scheme-process*)
     (setf *scheme-process* nil)
     (scheme-output-callback
      (format nil "~%;; Scheme process was killed.~%~%")))
    (t
     (editor-error  "Scheme process doesn't exist."))))

(defun scheme-send-input (string)
  (lem-process:process-send-input *scheme-process*
                                  (format nil "~A~%" string)))

(define-command scheme-eval-last-expression (p) ("P")
  (declare (ignore p))
  (with-point ((start (current-point))
               (end   (current-point)))
    (form-offset start -1)
    (scheme-run-process-and-output-newline)
    (scheme-send-input (points-to-string start end))))

(define-command scheme-eval-region (start end) ("r")
  (scheme-run-process-and-output-newline)
  (scheme-send-input (points-to-string start end)))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *scheme-process*
              (lem-process:delete-process *scheme-process*))))
