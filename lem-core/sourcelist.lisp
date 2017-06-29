(defpackage :lem.sourcelist
  (:use :cl :lem)
  (:export :with-sourcelist
           :append-jump-function
           :append-sourcelist
           :jump-highlighting))
(in-package :lem.sourcelist)

(define-attribute jump-highlight
  (t :background "cyan"))

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

(defun jump-highlighting ()
  (let ((point (current-point)))
    (with-point ((start point)
                 (end point))
      (let ((overlay (make-overlay (back-to-indentation start) (line-end end)
                                   'jump-highlight)))
        (start-timer 300 nil (lambda ()
                               (delete-overlay overlay)))))))

(defun jump-current-element (index)
  (funcall (aref (sourcelist-elements *current-sourcelist*)
                 index)
           (let ((buffer-name (sourcelist-buffer-name *current-sourcelist*)))
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
  (when *current-sourcelist*
    (when (< (1+ (sourcelist-index *current-sourcelist*))
             (length (sourcelist-elements *current-sourcelist*)))
      (jump-current-element
       (incf (sourcelist-index *current-sourcelist*))))))

(define-key *global-keymap* "C-x p" 'sourcelist-previous)
(define-key *global-keymap* "C-x C-p" 'sourcelist-previous)
(define-command sourcelist-previous () ()
  (when *current-sourcelist*
    (when (<= 0 (1- (sourcelist-index *current-sourcelist*)))
      (jump-current-element
       (decf (sourcelist-index *current-sourcelist*))))))

(define-minor-mode sourcelist-mode
    (:name "sourcelist"
	   :keymap *sourcelist-mode-keymap*))

(define-key *sourcelist-mode-keymap* "C-m" 'sourcelist-jump)
(define-key *sourcelist-mode-keymap* "q" 'quit-window)

(define-command sourcelist-jump () ()
  (let ((index (text-property-at (current-point) 'sourcelist)))
    (when index
      (jump-current-element
       (setf (sourcelist-index *current-sourcelist*) index)))))
