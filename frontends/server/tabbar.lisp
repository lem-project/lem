(defpackage :lem/tabbar
  (:use :cl :lem :lem/button)
  (:export :tabbar-active-tab-attribute
           :tabbar-attribute
           :tabbar-background-attribute
           :tabbar)
  #+sbcl
  (:lock t))
(in-package :lem/tabbar)

(define-attribute tabbar-active-tab-attribute
  (t :foreground "black" :background "dark gray"))

(define-attribute tabbar-attribute
  (t :foreground "white" :background "gray"))

(define-attribute tabbar-background-attribute
  (t :underline t))

(defclass tabbar-window (header-window)
  ((buffer
    :initarg :buffer
    :accessor tabbar-buffer)
   (prev-buffer-list
    :initform '()
    :accessor tabbar-prev-buffer-list)
   (prev-current-buffer
    :initform nil
    :accessor tabbar-prev-current-buffer)
   (prev-display-width
    :initform 0
    :accessor tabbar-prev-display-width)))

(defvar *tabbar* nil)

(defun tabbar-init ()
  (let ((buffer (make-buffer "*tabbar*" :temporary t :enable-undo-p nil)))
    (change-class buffer 'html-buffer :html "")
    (setf *tabbar* (make-instance 'tabbar-window :buffer buffer))))

(defun tabbar-require-update ()
  (block exit
    (unless (eq (current-buffer) (tabbar-prev-current-buffer *tabbar*))
      (return-from exit t))
    (unless (eq (buffer-list) (tabbar-prev-buffer-list *tabbar*))
      (return-from exit t))
    (unless (= (display-width) (tabbar-prev-display-width *tabbar*))
      (return-from exit t))
    nil))

(defmethod window-redraw ((window tabbar-window) force)
  (when (or force (tabbar-require-update))
    (lem-server::change-view-to-html
     window
     (with-output-to-string (out)
       (format out "<div class='lem-editor__tabbar'>~%")
       (loop :for buffer :in (buffer-list)
             :do (if (eq buffer (current-buffer))
                     (format out
                             "<button class=lem-editor__tabbar-button active>~A</button>~%"
                             (buffer-name buffer))
                     (format out
                             "<button class=lem-editor__tabbar-button>~A</button>~%"
                             (buffer-name buffer))))
       (format out "</div>")))
    (setf (tabbar-prev-buffer-list *tabbar*) (buffer-list))
    (setf (tabbar-prev-current-buffer *tabbar*) (current-buffer))
    (setf (tabbar-prev-display-width *tabbar*) (display-width))
    (call-next-method)))

(defun tabbar-clear-cache ()
  (setf (tabbar-buffer *tabbar*) nil)
  (setf (tabbar-prev-buffer-list *tabbar*) '())
  (setf (tabbar-prev-current-buffer *tabbar*) nil)
  (setf (tabbar-prev-display-width *tabbar*) 0))

(defun tabbar-off ()
  (when (and (variable-value 'tabbar :global)
             *tabbar*)
    (tabbar-clear-cache)
    (delete-window *tabbar*)
    (setf *tabbar* nil)))

(defun tabbar-on ()
  (unless (variable-value 'tabbar :global)
    (tabbar-init)))

(define-editor-variable tabbar nil ""
  (lambda (value)
    (if value
        (tabbar-on)
        (tabbar-off))))

(define-command toggle-tabbar () ()
  (setf (variable-value 'tabbar :global)
        (not (variable-value 'tabbar :global))))

;(define-key *global-keymap* "Shift-PageDown" 'tabbar-next)
;(define-key *global-keymap* "Shift-PageUp" 'tabbar-prev)

(define-command tabbar-next (n) (:universal)
  (let ((p (buffer-point (tabbar-buffer *tabbar*))))
    (dotimes (_ n)
      (forward-button p))
    (let ((button (button-at p)))
      (when button
        (move-point p (button-end button))
        (character-offset p -1)
        (button-action button)))))

(define-command tabbar-prev (n) (:universal)
  (let ((p (buffer-point (tabbar-buffer *tabbar*))))
    (dotimes (_ n)
      (backward-button p))
    (let ((button (button-at p)))
      (when button
        (button-action button)))))

(defun enable-tabbar ()
  (setf (variable-value 'tabbar :global) t))
