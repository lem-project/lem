;; -*- mode:lisp; package:lem -*-

(in-package :lem)

(export '(listener-mode
          *listener-mode-kemap*
          listener-start
          listener-reset-prompt
          listener-return
          listener-prev-input
          listener-next-input
          listener-reset-interactive))

(defvar %listener-marker-indicator (gensym))
(defmacro %listener-marker ()
  `(get-bvar %listener-marker-indicator))

(defvar %listener-history-indicator (gensym))
(defmacro %listener-history ()
  `(get-bvar %listener-history-indicator))

(define-minor-mode listener-mode
  (:name "listener"
   :keymap-var *listener-mode-keymap*)
  (setf (get-bvar :enable-syntax-highlight) nil)
  (unless (%listener-history)
    (setf (%listener-history)
          (make-history))))

(defun listener-start (buffer-name mode)
  (let ((buffer (get-buffer-create buffer-name)))
    (select-window (pop-to-buffer buffer))
    (funcall mode)
    (listener-reset-prompt)))

(defun listener-update-marker ()
  (when (%listener-marker)
    (delete-marker (%listener-marker)))
  (setf (%listener-marker) (make-marker-current-point)))

(defun listener-reset-prompt ()
  (end-of-buffer)
  (unless (bolp)
    (insert-newline 1))
  (insert-string
   (format nil "~a> "
           (funcall (get-bvar :listener-get-prompt-function))))
  (put-attribute (make-point (window-cur-linum) 0)
                 (make-point (window-cur-linum) (window-cur-col))
                 (make-attr :bold-p t :color :blue))
  (buffer-undo-boundary (window-buffer))
  (listener-update-marker))

(define-key *listener-mode-keymap* (kbd "C-m") 'listener-return)
(define-command listener-return () ()
  (end-of-buffer)
  (let ((end (point)))
    (if (not (funcall (get-bvar :listener-check-confirm-function)))
        (insert-newline)
        (let ((start (marker-point (%listener-marker))))
          (unless (point< start end)
            (listener-reset-prompt)
            (return-from listener-return t))
          (let ((str (region-string start end)))
            (add-history (%listener-history) str)
            (end-of-buffer)
            (insert-newline)
            (listener-update-marker)
            (funcall (get-bvar :listener-confirm-function) str)))))
  t)

(define-key *listener-mode-keymap* (kbd "M-p") 'listener-prev-input)
(define-command listener-prev-input () ()
  (multiple-value-bind (str win)
      (prev-history (%listener-history))
    (let ((start (marker-point (%listener-marker)))
          (end (point-max)))
      (delete-region start end)
      (when win (insert-string str))
      (setf (marker-point (%listener-marker)) start))))

(define-key *listener-mode-keymap* (kbd "M-n") 'listener-next-input)
(define-command listener-next-input () ()
  (multiple-value-bind (str win)
      (next-history (%listener-history))
    (let ((start (marker-point (%listener-marker)))
          (end (point-max)))
      (delete-region start end)
      (when win (insert-string str))
      (setf (marker-point (%listener-marker)) start))))

(define-key *listener-mode-keymap* (kbd "M-r") 'listener-reset-interactive)
(define-command listener-reset-interactive (arg) ("P")
  (when arg (delete-region (point-min) (point-max)))
  (listener-reset-prompt)
  t)
