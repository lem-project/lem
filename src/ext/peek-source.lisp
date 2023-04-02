(defpackage :lem/peek-source
  (:use :cl :lem)
  (:export :filename-attribute
           :position-attribute
           :with-collecting-sources
           :with-appending-source
           :collector-buffer
           :get-move-function
           :show-matched-line)
  #+sbcl
  (:lock t))
(in-package :lem/peek-source)

(define-attribute filename-attribute
  (:light :foreground "blue")
  (:dark :foreground "cyan"))

(define-attribute position-attribute
  (:light :foreground "dark red")
  (:dark :foreground "red"))

(defvar *collector*)

(defclass collector ()
  ((buffer :initarg :buffer
           :reader collector-buffer)
   (count :initform 0
          :accessor collector-count)))

(defvar *peek-window*)
(defvar *source-window*)
(defvar *parent-window*)

(define-minor-mode peek-source-mode
    (:name "Peek"
     :keymap *peek-source-keymap*)
  (setf (lem::not-switchable-buffer-p (current-buffer)) t))

(define-key *peek-source-keymap* "Return" 'peek-source-select)

(defclass peek-window (floating-window) ())
(defclass source-window (floating-window) ())

(defmethod lem::%delete-window :before ((peek-window peek-window))
  (finalize-peek-source))

(defmethod lem::compute-window-list ((current-window peek-window))
  (list *peek-window* *source-window*))

(defmethod lem::compute-window-list ((current-window source-window))
  (list *source-window* *peek-window*))

(defun finalize-peek-source ()
  (finalize-highlight-overlays)
  (setf (current-window) *parent-window*)
  (delete-window *source-window*))

(defun set-move-function (start end move-function)
  (put-text-property start end 'move-function move-function))

(defun get-move-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point 'move-function)))

(defun make-two-side-by-side-windows (buffer)
  (let* ((x-margin 4)
         (y-margin 2)
         (width (- (floor (display-width) 2) 2 x-margin))
         (height (- (display-height) 2 (* 2 y-margin)))
         (peek-window (make-instance 'peek-window
                                     :buffer buffer
                                     :x (+ 1 x-margin)
                                     :y (+ 1 y-margin)
                                     :width width
                                     :height height
                                     :use-border t))
         (source-window (make-instance 'source-window
                                       :buffer (make-buffer "*source*" :temporary t :enable-undo-p nil)
                                       :x (+ (window-x peek-window) (window-width peek-window) 2)
                                       :y (+ 1 y-margin)
                                       :width width
                                       :height height
                                       :use-border t)))
    (list peek-window source-window)))

(defun display (collector)
  (destructuring-bind (peek-window source-window)
      (make-two-side-by-side-windows (collector-buffer collector))

    (setf *parent-window* (current-window))
    (setf *peek-window* peek-window)
    (setf *source-window* source-window)

    (setf (current-window) peek-window)
    (peek-source-mode t)

    (show-matched-line)))

(defun make-peek-source-buffer ()
  (let ((buffer (make-buffer "*peek-source*" :temporary t :enable-undo-p t)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    buffer))

(defun call-with-collecting-sources (function)
  (let ((*collector* (make-instance 'collector :buffer (make-peek-source-buffer))))
    (funcall function *collector*)
    (unless (zerop (collector-count *collector*))
      (display *collector*))))

(defmacro with-collecting-sources ((collector) &body body)
  `(call-with-collecting-sources (lambda (,collector)
                                   (declare (ignorable ,collector))
                                   ,@body)))

(defun call-with-appending-source (insert-function move-function)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (funcall insert-function point)
      (unless (start-line-p point)
        (insert-string point (string #\newline) :read-only t))
      (set-move-function start point move-function))
    (incf (collector-count *collector*))))

(defmacro with-appending-source ((point &key move-function) &body body)
  `(call-with-appending-source (lambda (,point) ,@body)
                               ,move-function))

;;;
(define-attribute match-line-attribute
  (t :background "#444444"))

(defun get-matched-point ()
  (alexandria:when-let* ((move (get-move-function (buffer-point (window-buffer *peek-window*))))
                         (point (funcall move)))
    point))

(defun show-matched-line ()
  (alexandria:when-let (point (get-matched-point))
    (let* ((point (copy-point point :temporary))
           (buffer (point-buffer point)))
      (with-current-window *source-window*
        (switch-to-buffer buffer nil nil)
        (update-highlight-overlay point)
        (move-point (buffer-point buffer) point)
        (window-see (current-window))))))

(defmethod execute :after ((mode peek-source-mode) command argument)
  (when (eq (current-window) *peek-window*)
    (show-matched-line)))

(define-command peek-source-select () ()
  (alexandria:when-let ((point (get-matched-point)))
    (let ((line (line-number-at-point point)))
      (peek-source-quit)
      (switch-to-buffer (point-buffer point))
      (move-to-line (current-point) line))))

(define-command peek-source-quit () ()
  (setf (current-window) *parent-window*)
  (start-timer
   (make-idle-timer (lambda ()
                      (delete-window *peek-window*)
                      (delete-window *source-window*)))
   0))

;;;
(defvar *highlight-overlays* '())

(defun set-highlight-overlay (point)
  (let ((overlay (make-overlay point point (ensure-attribute 'match-line-attribute))))
    (push overlay *highlight-overlays*)
    (overlay-put overlay :display-line t)
    (setf (buffer-value (point-buffer point) 'highlight-overlay) overlay)))

(defun get-highlight-overlay (point)
  (buffer-value (point-buffer point) 'highlight-overlay))

(defun update-highlight-overlay (point)
  (let ((overlay (get-highlight-overlay point)))
    (cond (overlay
           (move-point (overlay-start overlay) point)
           (move-point (overlay-end overlay) point))
          (t
           (set-highlight-overlay point)))))

(defun finalize-highlight-overlays ()
  (dolist (overlay *highlight-overlays*)
    (buffer-unbound (overlay-buffer overlay) 'highlight-overlay)
    (delete-overlay overlay))
  (setf *highlight-overlays* '()))
