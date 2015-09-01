(in-package :lem)

(defvar *info-mode-keymap*
  (make-keymap "info" nil *global-keymap*))

(define-major-mode info-mode
  (:name "info-mode"
   :keymap *info-mode-keymap*
   :syntax-table (make-syntax-table))
  (buffer-disable-undo (window-buffer)))

(define-key *info-mode-keymap* (kbd "q") 'info-quit)
(define-command info-quit () ()
  (let ((buffer (current-buffer)))
    (when (buffer-get buffer :popup)
      (delete-current-window))
    (kill-buffer (buffer-name buffer))
    t))

(defun info-popup (buffer &optional fn)
  (let ((one-window-p (one-window-p)))
    (setq *current-window*
          (popup buffer fn
                 :goto-bob-p t
                 :erase-p t))
    (info-mode)
    (buffer-put buffer :popup one-window-p)))

(defun info-popup-string (buffer-name string)
  (info-popup
   (get-buffer-create buffer-name)
   (lambda ()
     (insert-string string))))
