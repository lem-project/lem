(in-package :lem)

(export '(*typeout-mode-keymap*
          pop-up-typeout-window))

(define-minor-mode typeout-mode
    (:name "typeout"
     :keymap *typeout-mode-keymap*))

(define-key *typeout-mode-keymap* "q" 'quit-window)
(define-key *typeout-mode-keymap* "Space" 'next-page)
(define-key *typeout-mode-keymap* "Backspace" 'previous-page)

(defun pop-up-typeout-window (buffer fn &key focus erase (read-only t))
  (let ((window (display-buffer buffer)))
    (with-current-window window
      (with-buffer-read-only buffer nil
        (when erase
          (erase-buffer buffer))
        (typeout-mode t)
        (when fn
          (save-excursion
            (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
              (funcall fn out)))))
      (when read-only
        (setf (buffer-read-only-p buffer) t)))
    (when focus
      (setf (current-window) window))
    window))
