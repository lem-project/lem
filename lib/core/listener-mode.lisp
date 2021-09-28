(defpackage :lem.listener-mode
  (:use :cl :lem)
  (:export :listener-prompt-attribute
           :*listener-mode-keymap*
           :prompt-end-point
           :listener-start
           :change-prompt-end-point
           :listener-reset-prompt
           :listener-return
           :listener-prev-input
           :listener-next-input
           :listener-reset-interactive
           :listener-set-prompt-function
           :listener-check-input-function
           :listener-execute-function
           :clear-listener)
  #+sbcl
  (:lock t))
(in-package :lem.listener-mode)

(define-attribute listener-prompt-attribute
  (:light :foreground "blue" :bold-p t)
  (:dark :foreground "cyan" :bold-p t))

(define-editor-variable listener-prompt-attribute 'listener-prompt-attribute)

(defun prompt-end-point (buffer)
  (buffer-value buffer '%prompt-end-point))

(defun set-prompt-end-point (buffer point)
  (setf (buffer-value buffer '%prompt-end-point) point))

(define-editor-variable listener-set-prompt-function)
(define-editor-variable listener-check-input-function)
(define-editor-variable listener-execute-function)

(define-editor-variable listener-store)

(define-minor-mode listener-mode
    (:name "listener"
     :keymap *listener-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) nil)
  (unless (variable-value 'listener-store)
    (setf (variable-value 'listener-store)
          (make-instance '<listener>
                         :history (lem.history:make-history))))
  (unless (prompt-end-point (current-buffer))
    (change-prompt-end-point (current-point))))

(define-key *listener-mode-keymap* "Return" 'listener-return)
(define-key *listener-mode-keymap* "M-p" 'listener-prev-input)
(define-key *listener-mode-keymap* "M-n" 'listener-next-input)
(define-key *listener-mode-keymap* "M-r" 'listener-previous-matching-input)
(define-key *listener-mode-keymap* "C-c M-o" 'listener-clear-buffer)
(define-key *listener-mode-keymap* "C-c C-u" 'listener-clear-input)

(defun current-listener-history ()
  (let ((listener (variable-value 'listener-store :buffer (current-buffer))))
    (listener-history listener)))

(defun default-switch-to-buffer (buffer)
  (setf (current-window) (pop-to-buffer buffer)))

(defun listener-start (buffer-name mode &key (switch-to-buffer-function 'default-switch-to-buffer))
  (let ((buffer (make-buffer buffer-name)))
    (funcall switch-to-buffer-function buffer)
    (funcall mode)
    (listener-reset-prompt buffer)))

(defun change-prompt-end-point (point)
  (check-type point point)
  (let ((buffer (point-buffer point)))
    (when (prompt-end-point buffer)
      (delete-point (prompt-end-point buffer)))
    (set-prompt-end-point buffer
                          (copy-point point :right-inserting))))

(defun listener-reset-prompt (&optional (buffer (current-buffer)) (fresh-line t))
  (let ((cur-point (buffer-point buffer)))
    (buffer-end cur-point)
    (when fresh-line
      (unless (start-line-p cur-point)
        (insert-character cur-point #\newline 1)
        (buffer-end cur-point)))
    (let ((point (funcall (variable-value 'listener-set-prompt-function
                                          :buffer buffer)
                          cur-point)))
      (with-point ((s point))
        (line-start s)
        (let ((attribute (variable-value 'listener-prompt-attribute :default buffer)))
          (when attribute
            (put-text-property s point :attribute attribute)))
        (put-text-property s point :read-only t)
        (put-text-property s point :field t)))
    (buffer-end cur-point)
    (buffer-undo-boundary buffer)
    (change-prompt-end-point cur-point)))

(define-command listener-return () ()
  (with-point ((point (buffer-end (current-point)) :left-inserting))
    (if (not (funcall (variable-value 'listener-check-input-function) point))
        (insert-character point #\newline)
        (let ((start (prompt-end-point (current-buffer))))
          (unless (point<= start point)
            (listener-reset-prompt)
            (return-from listener-return t))
          (let ((str (points-to-string start point)))
            (lem.history:add-history (current-listener-history) str)
            (buffer-end point)
            (insert-character point #\newline)
            (change-prompt-end-point (current-point))
            (funcall (variable-value 'listener-execute-function) point str)))))
  t)

(defun %backup-edit-string (history)
  (lem.history:backup-edit-string
   history
   (points-to-string (prompt-end-point (current-buffer))
                     (buffer-end-point (current-buffer)))))

(defun %restore-edit-string (history)
  (multiple-value-bind (str win)
      (lem.history:restore-edit-string history)
    (when win
      (replace-textarea str))))

(defun replace-textarea (str)
  (let ((start (prompt-end-point (current-buffer)))
        (end (buffer-end-point (current-buffer))))
    (save-excursion
      (delete-between-points start end)
      (insert-string start str)
      (move-point (prompt-end-point (current-buffer)) start))
    (buffer-end (current-point))))

(define-command listener-prev-input () ()
  (%backup-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem.history:prev-history (current-listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-next-input () ()
  (%backup-edit-string (current-listener-history))
  (%restore-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem.history:next-history (current-listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-previous-matching-input (regexp)
    ((prompt-for-string "Previous element matching (regexp): "))
  (%backup-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem.history:previous-matching (current-listener-history) regexp)
    (when win
      (replace-textarea str))))

(defun clear-listener (buffer)
  (let ((*inhibit-read-only* t))
    (erase-buffer buffer))
  (listener-reset-prompt buffer))

(define-command listener-clear-buffer () ()
  (clear-listener (current-buffer))
  t)

(define-command listener-clear-input () ()
  (delete-between-points (prompt-end-point (current-buffer))
                         (buffer-end-point (current-buffer))))


(defclass <listener> ()
  ((history :initform (lem.history:make-history)
            :initarg :history
            :accessor listener-history)))
