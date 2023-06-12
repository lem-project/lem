(defpackage :lem-lisp-mode/eval
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/eval)

(define-attribute eval-error-attribute
  (t :foreground "white" :background "dark red"))

(define-attribute eval-value-attribute
  (t :foreground "sky blue" :bold t))

(define-key *lisp-mode-keymap* "C-x C-e" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "M-Return" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "C-Return" 'lisp-eval-at-point)

(defun fold-one-line-message (message)
  (let ((pos (position #\newline message)))
    (if (not pos)
        message
        (format nil "~A..." (subseq message 0 pos)))))

(defun buffer-eval-result-overlays (buffer)
  (buffer-value buffer 'eval-result-overlays))

(defun (setf buffer-eval-result-overlays) (value buffer)
  (setf (buffer-value buffer 'eval-result-overlays) value))

(defun clear-eval-results (buffer)
  (mapc #'remove-eval-result-overlay
        (buffer-eval-result-overlays buffer)))

(defun remove-touch-overlay (start end old-len)
  (declare (ignore old-len))
  (remove-eval-result-overlay-between start end))

(defun remove-eval-result-overlay (overlay)
  (delete-overlay overlay)
  (delete-overlay (overlay-get overlay 'relation-overlay))
  (alexandria:removef (buffer-eval-result-overlays (overlay-buffer overlay))
                      overlay))

(defun remove-eval-result-overlay-between (start end)
  (let ((buffer (point-buffer start)))
    (dolist (ov (buffer-eval-result-overlays buffer))
      (unless (or (point< end (overlay-start ov))
                  (point< (overlay-end ov) start))
        (delete-overlay ov)
        (delete-overlay (overlay-get ov 'relation-overlay))
        (alexandria:removef (buffer-eval-result-overlays buffer)
                            ov)))))

(defun start-eval-spinner (start end)
  (lem/loading-spinner:start-loading-spinner :region :start start :end end))

(defun display-spinner-message (spinner &optional message is-error)
  (lem/loading-spinner:with-line-spinner-points (start end spinner)
    (let ((popup-overlay 
            (make-overlay start 
                          end
                          (if is-error
                              'eval-error-attribute
                              'eval-value-attribute)))
          (background-overlay
            (make-overlay start
                          end
                          (make-attribute :underline "white")))
         (buffer (point-buffer start)))
      (overlay-put popup-overlay 'relation-overlay background-overlay)
      (overlay-put popup-overlay :display-line-end t)
      (overlay-put popup-overlay :display-line-end-offset 1)
      (overlay-put popup-overlay :text (fold-one-line-message message))
      (push popup-overlay (buffer-eval-result-overlays buffer))
      (add-hook (variable-value 'after-change-functions :buffer buffer)
                'remove-touch-overlay))))

(defun eval-region (buffer)
  buffer)

(defun eval-last-expression (point)
  (with-point ((start point)
               (end point))
    (skip-whitespace-backward end)
    (form-offset start -1)
    (remove-eval-result-overlay-between start end)
    (let ((spinner (start-eval-spinner start end))
          (string (points-to-string start end)))
      (lem-lisp-mode/internal::with-remote-eval
          (`(micros:interactive-eval ,string))
        (lambda (value)
          (alexandria:destructuring-ecase value
            ((:ok result)
             (lem/loading-spinner:stop-loading-spinner spinner)
             (display-spinner-message spinner result nil))
            ((:abort condition)
             (lem/loading-spinner:stop-loading-spinner spinner)
             (display-spinner-message spinner condition t))))))))

(define-command lisp-eval-at-point () ()
  (check-connection)
  (if (buffer-mark-p (current-buffer))
      (eval-region (current-buffer))
      (eval-last-expression (current-point))))
