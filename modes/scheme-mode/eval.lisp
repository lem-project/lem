(in-package :lem-scheme-mode)

(defvar *scheme-process* nil)

(defun scheme-process-buffer ()
  (or (get-buffer "*scheme-process*")
      (let ((buffer (make-buffer "*scheme-process*")))
        (change-buffer-mode buffer 'scheme-mode)
        (setf (variable-value 'enable-syntax-highlight :buffer buffer) nil)
        buffer)))

(defun scheme-output-callback (string)
  (let ((buffer (scheme-process-buffer)))
    (insert-string (buffer-end-point buffer) string)
    (let ((window (pop-to-buffer buffer)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window))
      (redraw-display))))

(defun scheme-run-process ()
  (when (and *scheme-process*
             (not (lem-process:process-alive-p *scheme-process*)))
    (let ((buffer (scheme-process-buffer)))
      (insert-string (buffer-end-point buffer)
                     (format nil "~%;; Scheme process was aborted. Restarting...~%~%"))
      (lem-process:delete-process *scheme-process*)
      (setf *scheme-process* nil)))
  (unless *scheme-process*
    (setf *scheme-process* (lem-process:run-process
                            *scheme-run-command*
                            :name "scheme"
                            :output-callback #'scheme-output-callback))))

(defun scheme-send-input (string)
  (let ((buffer (scheme-process-buffer)))
    (when (eq buffer (current-buffer))
      ;; output newline like repl
      (insert-character (buffer-end-point buffer) #\newline)))
  (lem-process:process-send-input *scheme-process*
                                  (format nil "~A~%" string)))

(define-command scheme-eval-last-expression (p) ("P")
  (declare (ignore p))
  (with-point ((start (current-point))
               (end   (current-point)))
    (form-offset start -1)
    (scheme-run-process)
    (scheme-send-input (points-to-string start end))))

(define-command scheme-eval-region (start end) ("r")
  (scheme-run-process)
  (scheme-send-input (points-to-string start end)))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *scheme-process*
              (lem-process:delete-process *scheme-process*))))
