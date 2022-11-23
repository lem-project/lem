(defpackage :lem.listener-mode
  (:use :cl :lem)
  (:export
   ;; keymap
   :*listener-mode-keymap*
   ;; functions
   :start-listener-mode
   :input-start-point
   :listener-start
   :change-input-start-point
   :refresh-prompt
   :clear-listener
   ;; editor variables
   :listener-prompt-attribute
   :listener-set-prompt-function
   :listener-check-input-function
   :listener-execute-function
   ;; commands
   :listener-mode
   :listener-return
   :listener-previous-input
   :listener-next-input
   :listener-previous-matching-input
   :listener-clear-buffer
   :listener-clear-input)
  #+sbcl
  (:lock t))
(in-package :lem.listener-mode)

(define-attribute listener-prompt-attribute
  (:light :foreground "blue" :bold-p t)
  (:dark :foreground "cyan" :bold-p t))

(define-editor-variable listener-prompt-attribute 'listener-prompt-attribute)

(defun input-start-point (buffer)
  (buffer-value buffer '%input-start-point))

(defun set-input-start-point (buffer point)
  (setf (buffer-value buffer '%input-start-point) point))

(defun listener-history (buffer)
  (buffer-value buffer '%listener-history))

(defun (setf listener-history) (history buffer)
  (setf (buffer-value buffer '%listener-history) history))

(define-editor-variable listener-set-prompt-function)
(define-editor-variable listener-check-input-function)
(define-editor-variable listener-execute-function)

(define-minor-mode listener-mode
    (:name "listener"
     :keymap *listener-mode-keymap*))

(define-key *listener-mode-keymap* "Return" 'listener-return)
(define-key *listener-mode-keymap* "M-p" 'listener-previous-input)
(define-key *listener-mode-keymap* "M-n" 'listener-next-input)
(define-key *listener-mode-keymap* "M-r" 'listener-previous-matching-input)
(define-key *listener-mode-keymap* "C-c M-o" 'listener-clear-buffer)
(define-key *listener-mode-keymap* "C-c C-u" 'listener-clear-input)

(defun start-listener-mode (&optional history-pathname)
  (listener-mode t)
  (setf (variable-value 'enable-syntax-highlight) nil)
  (unless (listener-history (current-buffer))
    (setf (listener-history (current-buffer))
          (lem/common/history:make-history :pathname history-pathname))
    (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer))
              'save-history))
  (add-hook *exit-editor-hook* 'save-all-histories)
  (unless (input-start-point (current-buffer))
    (change-input-start-point (current-point))))

(defun listener-buffer-p (buffer)
  (mode-active-p buffer 'listener-mode))

(defun save-history (buffer)
  (assert (listener-buffer-p buffer))
  (lem/common/history:save-file (listener-history buffer)))

(defun all-listener-buffers ()
  (remove-if-not #'listener-buffer-p (buffer-list)))

(defun save-all-histories ()
  (mapc #'save-history (all-listener-buffers)))

(defun current-listener-history ()
  (listener-history (current-buffer)))

(defun default-switch-to-buffer (buffer)
  (setf (current-window) (pop-to-buffer buffer)))

(defun listener-start (buffer-name mode &key (switch-to-buffer-function 'default-switch-to-buffer))
  (let ((buffer (make-buffer buffer-name)))
    (funcall switch-to-buffer-function buffer)
    (funcall mode)
    (refresh-prompt buffer)))

(defun change-input-start-point (point)
  (check-type point point)
  (let ((buffer (point-buffer point)))
    (when (input-start-point buffer)
      (delete-point (input-start-point buffer)))
    (set-input-start-point buffer
                          (copy-point point :right-inserting))))

(defun write-prompt (point)
  (let ((buffer (point-buffer point)))
    (funcall (variable-value 'listener-set-prompt-function
                             :buffer buffer)
             point)
    (with-point ((s point))
      (line-start s)
      (let ((attribute (variable-value 'listener-prompt-attribute :default buffer)))
        (when attribute
          (put-text-property s point :attribute attribute)))
      (put-text-property s point :read-only t)
      (put-text-property s point :field t))))

(defun refresh-prompt (&optional (buffer (current-buffer)) (fresh-line t))
  (let ((point (buffer-point buffer)))
    (buffer-end point)
    (when fresh-line
      (unless (start-line-p point)
        (insert-character point #\newline 1)
        (buffer-end point)))
    (write-prompt point)
    (buffer-end point)
    (buffer-undo-boundary buffer)
    (change-input-start-point point)))

(define-command listener-return () ()
  (with-point ((point (buffer-end (current-point)) :left-inserting))
    (if (not (funcall (variable-value 'listener-check-input-function) point))
        (insert-character point #\newline)
        (let ((start (input-start-point (current-buffer))))
          (unless (point<= start point)
            (refresh-prompt)
            (return-from listener-return))
          (let ((str (points-to-string start point)))
            (lem/common/history:add-history (current-listener-history) str)
            (buffer-end point)
            (insert-character point #\newline)
            (change-input-start-point (current-point))
            (funcall (variable-value 'listener-execute-function) point str))))))

(defun %backup-edit-string (history)
  (lem/common/history:backup-edit-string
   history
   (points-to-string (input-start-point (current-buffer))
                     (buffer-end-point (current-buffer)))))

(defun %restore-edit-string (history)
  (multiple-value-bind (str win)
      (lem/common/history:restore-edit-string history)
    (when win
      (replace-textarea str))))

(defun replace-textarea (str)
  (let ((start (input-start-point (current-buffer)))
        (end (buffer-end-point (current-buffer))))
    (save-excursion
      (delete-between-points start end)
      (insert-string start str)
      (move-point (input-start-point (current-buffer)) start))
    (buffer-end (current-point))))

(define-command listener-previous-input () ()
  (%backup-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem/common/history:previous-history (current-listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-next-input () ()
  (%backup-edit-string (current-listener-history))
  (%restore-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem/common/history:next-history (current-listener-history))
    (when win
      (replace-textarea str))))

(define-command listener-previous-matching-input (regexp)
    ((prompt-for-string "Previous element matching (regexp): "))
  (%backup-edit-string (current-listener-history))
  (multiple-value-bind (str win)
      (lem/common/history:previous-matching (current-listener-history) regexp)
    (when win
      (replace-textarea str))))

(defun clear-listener (buffer)
  (let ((*inhibit-read-only* t))
    (erase-buffer buffer))
  (refresh-prompt buffer))

(define-command listener-clear-buffer () ()
  (clear-listener (current-buffer)))

(define-command listener-clear-input () ()
  (delete-between-points (input-start-point (current-buffer))
                         (buffer-end-point (current-buffer))))
