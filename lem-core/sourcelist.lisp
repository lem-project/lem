(defpackage :lem.sourcelist
  (:use :cl :lem)
  (:export :with-sourcelist
           :append-jump-function
           :append-sourcelist))
(in-package :lem.sourcelist)

(defvar *sourcelist-point*)
(defvar *current-sourcelist* nil)

(defstruct sourcelist
  buffer-name
  temp-point
  (elements (make-array 0 :adjustable t :fill-pointer 0))
  (index -1))

(defun call-with-sourcelist (buffer-name function focus)
  (let ((buffer (make-buffer buffer-name :read-only-p t :enable-undo-p nil))
        (sourcelist (make-sourcelist :buffer-name buffer-name)))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-point ((*sourcelist-point* (buffer-point buffer) :left-inserting))
        (funcall function sourcelist))
      (buffer-start (buffer-point buffer))
      (change-buffer-mode buffer 'sourcelist-mode t)
      (if focus
          (setf (current-window) (display-buffer buffer))
          (display-buffer buffer))
      (setf (variable-value 'truncate-lines :buffer buffer) nil)
      (setf *current-sourcelist* sourcelist))))

(defmacro with-sourcelist ((var buffer-name &key focus) &body body)
  `(call-with-sourcelist ,buffer-name
                         (lambda (,var)
                           ,@body)
                         ,focus))

(defun append-jump-function (sourcelist start end jump-function)
  (put-text-property start end 'sourcelist jump-function)
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

(defun jump-current-element ()
  (funcall (aref (sourcelist-elements *current-sourcelist*)
                 (sourcelist-index *current-sourcelist*))
           (let ((buffer-name (sourcelist-buffer-name *current-sourcelist*)))
             (lambda (buffer)
               (let ((sourcelist-window
                       (car (get-buffer-windows (get-buffer buffer-name)))))
                 (unless sourcelist-window
                   (let ((sourcelist-buffer (get-buffer buffer-name)))
                     (setf sourcelist-window
                           (display-buffer sourcelist-buffer))))
                 (if (eq (current-window) sourcelist-window)
                     (setf (current-window) (pop-to-buffer buffer))
                     (switch-to-buffer buffer)))))))

(define-key *global-keymap* "C-x n" 'sourcelist-next)
(define-key *global-keymap* "C-x C-n" 'sourcelist-next)
(define-command sourcelist-next () ()
  (when *current-sourcelist*
    (when (< (1+ (sourcelist-index *current-sourcelist*))
             (length (sourcelist-elements *current-sourcelist*)))
      (incf (sourcelist-index *current-sourcelist*))
      (jump-current-element))))

(define-key *global-keymap* "C-x p" 'sourcelist-previous)
(define-key *global-keymap* "C-x C-p" 'sourcelist-previous)
(define-command sourcelist-previous () ()
  (when *current-sourcelist*
    (when (<= 0 (1- (sourcelist-index *current-sourcelist*)))
      (decf (sourcelist-index *current-sourcelist*))
      (jump-current-element))))

(define-minor-mode sourcelist-mode
    (:name "sourcelist"
	   :keymap *sourcelist-mode-keymap*))

(define-key *sourcelist-mode-keymap* "C-m" 'sourcelist-jump)
(define-key *sourcelist-mode-keymap* "q" 'quit-window)

(define-command sourcelist-jump () ()
  (let ((jump-function (text-property-at (current-point) 'sourcelist)))
    (when jump-function
      (funcall jump-function
               (lambda (buffer)
                 (with-point ((p (buffer-point buffer)))
                   (setf (current-window) (pop-to-buffer buffer))
                   (move-point (buffer-point buffer) p)))))))
