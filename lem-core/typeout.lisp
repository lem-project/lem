(in-package :lem)

(export '(*typeout-mode-keymap*
          typeout-mode
          pop-up-typeout-window))

(define-minor-mode typeout-mode
    (:name "typeout"
	   :keymap *typeout-mode-keymap*))

(define-key *typeout-mode-keymap* "q" 'quit-window)

(defun pop-up-typeout-window (buffer fn &key focus erase)
  (let ((window (display-buffer buffer)))
    (with-current-window window
      (with-buffer-read-only buffer nil
        (when erase
          (erase-buffer buffer))
        (typeout-mode t)
        (when fn
          (save-excursion
            (with-open-stream (out (make-buffer-output-stream (buffer-end-point buffer)))
              (funcall fn out))))))
    (when focus
      (setf (current-window) window))
    window))
