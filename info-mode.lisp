(in-package :lem)

(defvar *info-mode-keymap*
  (make-keymap "info" nil *global-keymap*))

(define-major-mode info-mode
  (:name "info-mode"
   :keymap *info-mode-keymap*
   :syntax-table (make-syntax-table))
  (buffer-disable-undo (window-buffer))
  (setf (buffer-read-only-p (window-buffer)) t))

(define-key *info-mode-keymap* (kbd "q") 'info-quit)
(define-command info-quit () ()
  (let ((buffer (current-buffer)))
    (when (buffer-get buffer :popup)
      (delete-current-window))
    (kill-buffer (buffer-name buffer))
    t))

(defun info-popup (buffer &optional fn (focus-set-p t))
  (let ((one-window-p (or (buffer-get buffer :popup)
                          (one-window-p))))
    (with-buffer-read-only buffer nil
      (let ((window
             (popup buffer fn
                   :goto-bob-p t
                   :erase-p t)))
        (when focus-set-p
          (setq *current-window* window))))
    (info-mode)
    (buffer-put buffer :popup one-window-p)))
