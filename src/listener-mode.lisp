(in-package :cl-user)
(defpackage :lem.listener-mode
  (:use :cl :lem :lem.util)
  (:export :listener-mode
           :*listener-mode-keymap*
           :listener-start-point
           :listener-start
           :listener-reset-prompt
           :listener-return
           :listener-prev-input
           :listener-next-input
           :listener-reset-interactive))
(in-package :lem.listener-mode)

(defvar *prompt-attribute* (make-attribute "blue" nil :bold-p t))

(defvar %listener-marker-indicator (gensym))
(defmacro %listener-marker ()
  `(get-bvar %listener-marker-indicator))

(defvar %listener-history-indicator (gensym))
(defmacro %listener-history ()
  `(get-bvar %listener-history-indicator))

(define-minor-mode listener-mode
  (:name "listener"
   :keymap *listener-mode-keymap*)
  (setf (get-bvar :enable-syntax-highlight) nil)
  (unless (%listener-history)
    (setf (%listener-history)
          (make-history))))

(defun listener-start-point ()
  (marker-point (%listener-marker)))

(defun listener-start (buffer-name mode)
  (let ((buffer (get-buffer-create buffer-name)))
    (setf (current-window) (pop-to-buffer buffer))
    (funcall mode)
    (listener-reset-prompt buffer)))

(defun listener-update-marker (&optional point)
  (when (%listener-marker)
    (delete-marker (%listener-marker)))
  (setf (%listener-marker)
        (if point
            (make-marker (current-buffer) point :name "listener")
            (make-marker-current-point :name "listener"))))

(defun listener-reset-prompt (&optional (buffer (current-buffer)))
  (let ((cur-marker (lem::buffer-point-marker buffer)))
    (lem::buffer-end cur-marker)
    (unless (lem::start-line-p cur-marker)
      (lem::insert-char-at cur-marker #\newline 1)
      (lem::buffer-end cur-marker))
    (lem::insert-string-at cur-marker
                           (princ-to-string
                            (funcall
                             (get-bvar :listener-get-prompt-function))))
    (lem::with-marker ((start-marker cur-marker)
                       (end-marker (lem::line-end cur-marker)))
      (lem::put-text-property start-marker end-marker :attribute *prompt-attribute*)
      (lem::put-text-property start-marker end-marker 'lem.property:read-only t)
      (lem::put-text-property (lem::character-offset (copy-marker end-marker :temporary) -1)
                              end-marker
                              'lem.property:field-separator t))
    (buffer-undo-boundary buffer)
    (listener-update-marker)))

(define-key *listener-mode-keymap* (kbd "C-m") 'listener-return)
(define-command listener-return () ()
  (point-set (point-max))
  (let ((end (current-point)))
    (if (not (funcall (get-bvar :listener-check-confirm-function)))
        (insert-newline)
        (let ((start (listener-start-point)))
          (unless (point< start end)
            (listener-reset-prompt)
            (return-from listener-return t))
          (let ((str (region-string start end)))
            (add-history (%listener-history) str)
            (point-set (point-max))
            (insert-newline)
            (listener-update-marker)
            (funcall (get-bvar :listener-confirm-function) str)))))
  t)

(define-key *listener-mode-keymap* (kbd "M-p") 'listener-prev-input)
(define-command listener-prev-input () ()
  (multiple-value-bind (str win)
      (prev-history (%listener-history))
    (let ((start (listener-start-point))
          (end (point-max)))
      (delete-region start end)
      (when win (insert-string str))
      (setf (marker-point (%listener-marker)) start))))

(define-key *listener-mode-keymap* (kbd "M-n") 'listener-next-input)
(define-command listener-next-input () ()
  (multiple-value-bind (str win)
      (next-history (%listener-history))
    (let ((start (listener-start-point))
          (end (point-max)))
      (delete-region start end)
      (when win (insert-string str))
      (setf (marker-point (%listener-marker)) start))))

(define-key *listener-mode-keymap* (kbd "M-r") 'listener-reset-interactive)
(define-command listener-reset-interactive (arg) ("P")
  (when arg
    (let ((*inhibit-read-only* t))
      (delete-region (point-min) (point-max))))
  (listener-reset-prompt)
  t)

(define-key *listener-mode-keymap* (kbd "C-c C-u") 'listener-clear-input)
(define-command listener-clear-input () ()
  (delete-region (listener-start-point)
                 (point-max)))
