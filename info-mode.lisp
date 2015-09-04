(in-package :lem)

(defvar *info-mode-keymap*
  (make-keymap "info" nil *global-keymap*))

(define-minor-mode info-mode
  :name "info-mode"
  :keymap *info-mode-keymap*)

(define-key *info-mode-keymap* (kbd "q") 'info-quit)
(define-command info-quit () ()
  (let ((buffer (current-buffer)))
    (when (buffer-get buffer :popup)
      (delete-current-window))
    (kill-buffer (buffer-name buffer))
    t))

(defun info-popup-closure (mode)
  #'(lambda (buffer fn focus-set-p)
      (let ((one-window-p (or (buffer-get buffer :popup)
                              (one-window-p)))
            window)
        (with-buffer-read-only buffer nil
          (setq window
                (popup buffer fn
                       :goto-bob-p t
                       :erase-p t))
          (when focus-set-p
            (setq *current-window* window)))
        (let ((*current-window* window))
          (info-mode)
          (when mode (funcall mode))
          (buffer-put buffer :popup one-window-p)))))

(defun info-popup (buffer &optional fn (focus-set-p t))
  (funcall (info-popup-closure nil)
           buffer fn focus-set-p))
