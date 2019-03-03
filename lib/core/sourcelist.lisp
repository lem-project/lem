(defpackage :lem.sourcelist
  (:use :cl :lem)
  (:export :title-attribute
           :position-attribute
           :with-sourcelist
           :append-jump-function
           :append-sourcelist
           :jump-highlighting))
(in-package :lem.sourcelist)

(define-attribute jump-highlight
  (t :background "cyan"))

(define-attribute title-attribute
  (:light :foreground "blue")
  (:dark :foreground "cyan"))

(define-attribute position-attribute
  (:light :foreground "dark red")
  (:dark :foreground "red"))

(defvar *sourcelist-point*)
(defvar *current-sourcelist-buffer* nil)

(defstruct sourcelist
  buffer-name
  temp-point
  (elements (make-array 0 :adjustable t :fill-pointer 0))
  (index -1))

(defun get-sourcelist (buffer)
  (buffer-value buffer 'sourcelist))

(defun (setf get-sourcelist) (sourcelist buffer)
  (setf (buffer-value buffer 'sourcelist) sourcelist))

(defun call-with-sourcelist (buffer-name function focus read-only-p enable-undo-p)
  (let ((buffer (make-buffer buffer-name :read-only-p read-only-p :enable-undo-p enable-undo-p))
        (sourcelist (make-sourcelist :buffer-name buffer-name)))
    (with-buffer-read-only buffer nil
      (let ((*inhibit-read-only* t))
        (erase-buffer buffer)
        (with-point ((*sourcelist-point* (buffer-point buffer) :left-inserting))
          (funcall function sourcelist))
        (buffer-start (buffer-point buffer))
        (change-buffer-mode buffer 'sourcelist-mode t)
        (if focus
            (setf (current-window) (display-buffer buffer))
            (display-buffer buffer))
        (setf (variable-value 'truncate-lines :buffer buffer) nil)
        (setf (get-sourcelist buffer) sourcelist)
        (setf *current-sourcelist-buffer* buffer)))))

(defmacro with-sourcelist ((var buffer-name &key focus (read-only-p t) (enable-undo-p nil)) &body body)
  `(call-with-sourcelist ,buffer-name
                         (lambda (,var)
                           ,@body)
                         ,focus
                         ,read-only-p
                         ,enable-undo-p))

(defun append-jump-function (sourcelist start end jump-function)
  (put-text-property start end
                     'sourcelist
                     (length (sourcelist-elements sourcelist)))
  (vector-push-extend jump-function (sourcelist-elements sourcelist)))

(defun append-sourcelist (sourcelist write-function jump-function)
  (let ((point *sourcelist-point*))
    (with-point ((start-point point :right-inserting))
      (funcall write-function point)
      (insert-character point #\newline)
      (when jump-function
        (append-jump-function sourcelist
                              start-point
                              point
                              jump-function)))))

(defun jump-highlighting (&optional (point (current-point)))
  (with-point ((start point)
               (end point))
    (let ((overlay (make-overlay (back-to-indentation start) (line-end end)
                                 'jump-highlight)))
      (start-timer 300 nil (lambda ()
                             (delete-overlay overlay)) nil "jump-highlighting"))))

(defun jump-current-element (index sourcelist)
  (funcall (aref (sourcelist-elements sourcelist)
                 index)
           (let ((buffer-name (sourcelist-buffer-name sourcelist)))
             (lambda (buffer)
               (with-point ((p (buffer-point buffer)))
                 (let ((sourcelist-window
                         (car (get-buffer-windows (get-buffer buffer-name)))))
                   (unless sourcelist-window
                     (let ((sourcelist-buffer (get-buffer buffer-name)))
                       (setf sourcelist-window
                             (display-buffer sourcelist-buffer))))
                   (if (eq (current-window) sourcelist-window)
                       (setf (current-window) (pop-to-buffer buffer))
                       (switch-to-buffer buffer))
                   (move-point (buffer-point buffer) p))))))
  (jump-highlighting))

(define-key *global-keymap* "C-x n" 'sourcelist-next)
(define-key *global-keymap* "C-x C-n" 'sourcelist-next)
(define-command sourcelist-next () ()
  (when *current-sourcelist-buffer*
    (alexandria:when-let ((sourcelist (get-sourcelist *current-sourcelist-buffer*)))
      (when (< (1+ (sourcelist-index sourcelist))
               (length (sourcelist-elements sourcelist)))
        (jump-current-element
         (incf (sourcelist-index sourcelist))
         sourcelist)))))

(define-key *global-keymap* "C-x p" 'sourcelist-previous)
(define-key *global-keymap* "C-x C-p" 'sourcelist-previous)
(define-command sourcelist-previous () ()
  (when *current-sourcelist-buffer*
    (alexandria:when-let ((sourcelist (get-sourcelist *current-sourcelist-buffer*)))
      (when (<= 0 (1- (sourcelist-index sourcelist)))
        (jump-current-element
         (decf (sourcelist-index sourcelist))
         sourcelist)))))

(define-minor-mode sourcelist-mode
    (:name "sourcelist"
     :keymap *sourcelist-mode-keymap*))

(define-key *sourcelist-mode-keymap* "Return" 'sourcelist-jump)
(define-key *sourcelist-mode-keymap* "q" 'quit-window)

(define-command sourcelist-jump () ()
  (alexandria:when-let ((sourcelist (get-sourcelist (current-buffer)))
                        (index (text-property-at (current-point) 'sourcelist)))
    (jump-current-element (setf (sourcelist-index sourcelist) index)
                          sourcelist)))
