(defpackage :lem.sourcelist
  (:use :cl :lem)
  (:export :title-attribute
           :position-attribute
           :with-sourcelist
           :append-jump-function
           :append-sourcelist
           :jump-highlighting)
  #+sbcl
  (:lock t))
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
  (buffer nil :read-only t :type buffer)
  temp-point
  (elements (make-array 0 :adjustable t :fill-pointer 0))
  (index -1))

(defstruct jump
  get-location-function
  get-highlight-overlay-function)

(defun get-sourcelist (buffer)
  (buffer-value buffer 'sourcelist))

(defun (setf get-sourcelist) (sourcelist buffer)
  (setf (buffer-value buffer 'sourcelist) sourcelist))

(defun call-with-sourcelist (buffer-name function focus read-only-p enable-undo-p)
  (let* ((buffer (make-buffer buffer-name :read-only-p read-only-p :enable-undo-p enable-undo-p))
         (sourcelist (make-sourcelist :buffer buffer)))
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
        (setf (variable-value 'line-wrap :buffer buffer) nil)
        (setf (get-sourcelist buffer) sourcelist)
        (setf *current-sourcelist-buffer* buffer)))))

(defmacro with-sourcelist ((var buffer-name &key focus (read-only-p t) (enable-undo-p nil))
                           &body body)
  `(call-with-sourcelist ,buffer-name
                         (lambda (,var)
                           ,@body)
                         ,focus
                         ,read-only-p
                         ,enable-undo-p))

(defun append-jump (sourcelist jump)
  (vector-push-extend jump (sourcelist-elements sourcelist)))

(defun put-sourcelist-index (sourcelist start end)
  (put-text-property start end
                     'sourcelist
                     (length (sourcelist-elements sourcelist))))

(defun append-jump-function (sourcelist start end jump-function)
  (put-sourcelist-index sourcelist start end)
  (append-jump sourcelist (make-jump :get-location-function jump-function)))

(defun append-sourcelist (sourcelist write-function jump-function
                          &key highlight-overlay-function)
  (let ((point *sourcelist-point*))
    (with-point ((start-point point :right-inserting))
      (funcall write-function point)
      (unless (start-line-p point)
        (insert-character point #\newline))
      (when jump-function
        (put-sourcelist-index sourcelist start-point point)
        (append-jump sourcelist
                     (make-jump :get-location-function jump-function
                                :get-highlight-overlay-function highlight-overlay-function))))))

(defun get-highlight-overlay-default (point)
  (with-point ((start point)
               (end point))
    (make-overlay (back-to-indentation start)
                  (line-end end)
                  'jump-highlight)))

(defun jump-highlighting (&optional (point (current-point)) jump)
  (let ((overlay
          (funcall (alexandria:if-let
                       ((fn (and jump (jump-get-highlight-overlay-function jump))))
                     fn
                     #'get-highlight-overlay-default)
                   point)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :name "jump-highlighting")
                 300)))

(defun jump-current-element (index sourcelist)
  (let ((jump (aref (sourcelist-elements sourcelist) index)))
    (funcall (jump-get-location-function jump)
             (let ((buffer-name (sourcelist-buffer sourcelist)))
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
    (jump-highlighting (current-point) jump)))

(define-key *global-keymap* "C-x n" 'sourcelist-next)
(define-key *global-keymap* "C-x C-n" 'sourcelist-next)
(define-key *global-keymap* "M-N" 'sourcelist-next)
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
(define-key *global-keymap* "M-P" 'sourcelist-previous)
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
(define-key *sourcelist-mode-keymap* "q" 'quit-active-window)

(define-command sourcelist-jump () ()
  (alexandria:when-let ((sourcelist (get-sourcelist (current-buffer)))
                        (index (text-property-at (current-point) 'sourcelist)))
    (jump-current-element (setf (sourcelist-index sourcelist) index)
                          sourcelist)))
