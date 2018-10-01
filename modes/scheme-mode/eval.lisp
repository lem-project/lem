(in-package :lem-scheme-mode)

(defvar *scheme-process* nil)

(defun scheme-output-callback (string)
  (let ((buffer (make-buffer "*scheme-process*")))
    (insert-string (buffer-end-point buffer) string)
    (let ((window (pop-to-buffer buffer)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window))
      (redraw-display))))

(defun scheme-run-process ()
  (unless *scheme-process*
    (setf *scheme-process* (lem-process:run-process
                            *scheme-run-command*
                            :name "scheme"
                            :output-callback #'scheme-output-callback))))

(defun scheme-send-input (string)
  (lem-process:process-send-input *scheme-process* string))

(define-command scheme-eval-last-expression (p) ("P")
  (declare (ignore p))
  (with-point ((start (current-point))
               (end   (current-point)))
    (form-offset start -1)
    (scheme-run-process)
    (scheme-send-input (format nil "~A~%" (points-to-string start end)))))

(define-command scheme-eval-region (start end) ("r")
  (scheme-run-process)
  (scheme-send-input (format nil "~A~%" (points-to-string start end))))

(add-hook *exit-editor-hook*
          (lambda ()
            (when *scheme-process*
              (lem-process:delete-process *scheme-process*))))
