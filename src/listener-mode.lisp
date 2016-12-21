(in-package :cl-user)
(defpackage :lem.listener-mode
  (:use :cl :lem :lem.util)
  (:export :listener-mode
           :*listener-mode-keymap*
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
  (copy-marker (%listener-marker) :temporary))

(defun listener-start (buffer-name mode)
  (let ((buffer (get-buffer-create buffer-name)))
    (setf (current-window) (pop-to-buffer buffer))
    (funcall mode)
    (listener-reset-prompt buffer)))

(defun listener-update-marker (&optional (point (current-marker)))
  (when (%listener-marker)
    (delete-marker (%listener-marker)))
  (setf (%listener-marker)
        (if point
            (copy-marker point :right-inserting)
            (copy-marker (current-marker) :right-inserting))))

(defun listener-reset-prompt (&optional (buffer (current-buffer)))
  (let ((cur-marker (lem::buffer-point-marker buffer)))
    (lem::buffer-end cur-marker)
    (unless (lem::start-line-p cur-marker)
      (lem::insert-char-at cur-marker #\newline 1)
      (lem::buffer-end cur-marker))
    (lem::insert-string-at cur-marker
                           (lem.text-property:make-text-property
                            (princ-to-string
                             (funcall
                              (get-bvar :listener-get-prompt-function)))
                            :attribute *prompt-attribute*
                            'lem.property:read-only t
                            'lem.property:field-separator t))
    (lem::buffer-end cur-marker)
    (buffer-undo-boundary buffer)
    (listener-update-marker)))

(define-key *listener-mode-keymap* (kbd "C-m") 'listener-return)
(define-command listener-return () ()
  (lem::with-marker ((point (lem::buffer-end (current-marker)) :left-inserting))
    (if (not (funcall (get-bvar :listener-check-confirm-function) point))
        (lem::insert-char-at point #\newline)
        (let ((start (listener-start-point)))
          (unless (marker< start point)
            (listener-reset-prompt)
            (return-from listener-return t))
          (let ((str (lem::points-to-string start point)))
            (add-history (%listener-history) str)
            (lem::buffer-end point)
            (lem::insert-char-at point #\newline)
            (listener-update-marker)
            (funcall (get-bvar :listener-confirm-function) point str)))))
  t)

(define-key *listener-mode-keymap* (kbd "M-p") 'listener-prev-input)
(define-command listener-prev-input () ()
  (multiple-value-bind (str win)
      (prev-history (%listener-history))
    (let ((start (listener-start-point))
          (end (lem::buffers-end (current-buffer))))
      (lem::delete-between-points start end)
      (when win (lem::insert-string-at start str))
      (lem::move-point (%listener-marker) start))))

(define-key *listener-mode-keymap* (kbd "M-n") 'listener-next-input)
(define-command listener-next-input () ()
  (multiple-value-bind (str win)
      (next-history (%listener-history))
    (let ((start (listener-start-point))
          (end (lem::buffers-end (current-marker))))
      (lem::delete-between-points start end)
      (when win (lem::insert-string-at start str))
      (lem::move-point (%listener-marker) start))))

(define-key *listener-mode-keymap* (kbd "M-r") 'listener-reset-interactive)
(define-command listener-reset-interactive (arg) ("P")
  (when arg
    (let ((*inhibit-read-only* t))
      (buffer-erase (current-buffer))))
  (listener-reset-prompt)
  t)

(define-key *listener-mode-keymap* (kbd "C-c C-u") 'listener-clear-input)
(define-command listener-clear-input () ()
  (lem::delete-between-points (listener-start-point)
                              (lem::buffers-end (current-buffer))))
