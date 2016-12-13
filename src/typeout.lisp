(in-package :lem)

(export '(*typeout-mode-keymap*
          typeout-mode
          pop-up-typeout-window))

(define-minor-mode typeout-mode
  (:name "typeout"
   :keymap *typeout-mode-keymap*))

(define-key *typeout-mode-keymap* (kbd "q") 'dismiss-typeout-window)

(define-command dismiss-typeout-window () ()
  (quit-window (current-window) t))

(defun pop-up-typeout-window (buffer fn &key focus erase)
  (let ((window (display-buffer buffer)))
    (with-current-window window
      (with-buffer-read-only buffer nil
        (when erase
          (buffer-erase buffer))
        (typeout-mode t)
        (when fn
          (with-open-stream (out (make-buffer-output-stream buffer (current-point)))
            (funcall fn out)))))
    (when focus
      (setf (current-window) window))
    window))
