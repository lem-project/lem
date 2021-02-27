(defpackage :lem.popup-window
  (:use :cl :lem)
  (:export :get-focus-item
           :apply-print-spec))
(in-package :lem.popup-window)

(defconstant +border-size+ 1)
(defconstant +min-width+   3)
(defconstant +min-height+  1)

(defvar *extra-right-margin* 0)
(defvar *extra-width-margin* 0)

(defvar *menu-buffer* nil)
(defvar *menu-window* nil)
(defvar *focus-overlay* nil)
(defvar *print-spec* nil)
(defvar *action-callback* nil)
(defvar *focus-attribute* nil)
(defvar *non-focus-attribute* nil)

(defgeneric adjust-for-redrawing (gravity popup-window)
  (:method (gravity popup-window)))

(defgeneric compute-popup-window-position (gravity source-window width height))

(defclass gravity (cl-singleton-mixin:singleton-class) ())
(defclass gravity-top (gravity) ())
(defclass gravity-topright (gravity) ())
(defclass gravity-cursor (gravity) ())
(defclass gravity-follow-cursor (gravity-cursor) ())

(defun ensure-gravity (gravity)
  (if (typep gravity 'gravity)
      gravity
      (ecase gravity
        (:top (make-instance 'gravity-top))
        (:topright (make-instance 'gravity-topright))
        (:cursor (make-instance 'gravity-cursor))
        (:follow-cursor (make-instance 'gravity-follow-cursor)))))

(defmethod adjust-for-redrawing ((gravity gravity-follow-cursor) popup-window)
  (multiple-value-bind (x y width height)
      (compute-popup-window-position (popup-window-gravity popup-window)
                                     (popup-window-source-window popup-window)
                                     (popup-window-base-width popup-window)
                                     (popup-window-base-height popup-window))
    (lem::window-set-size popup-window width height)
    (lem::window-set-pos popup-window
                         (+ x +border-size+)
                         (+ y +border-size+))))

(defmethod compute-popup-window-position ((gravity gravity-cursor) source-window width height)
  (let* ((b2 (* +border-size+ 2))
         (disp-w (max (- (display-width)  b2 *extra-right-margin*)
                      +min-width+))
         (disp-h (max (- (display-height) b2)
                      +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ (window-x source-window)
               (window-cursor-x source-window)))
         (y (max (min (+ (window-y source-window)
                         (window-cursor-y source-window)
                         1)
                      (1- (display-height)))
                 0))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) disp-h)
      (decf h (- (+ y height) disp-h)))
    (when (< h (min height (floor disp-h 3)))
      (setf h height)
      (decf y (+ height b2 1)))
    (when (< y 0)
      (decf h (- y))
      (setf y 0))
    (when (<= h 0) ; for safety
      (setf y 0)
      (setf h (min height disp-h)))
    ;; calc x and w
    (when (> (+ x width) disp-w)
      (decf x (- (+ x width) disp-w)))
    (when (< x 0)  ; for safety
      (setf x 0)
      (setf w (min width disp-w)))
    (values x y w h)))

(defmethod compute-popup-window-position ((gravity gravity-top) source-window width height)
  (let* ((x (- (floor (display-width) 2)
                      (floor width 2)))
         (y (+ (window-y source-window) 1)))
    (values x y width height)))

(defmethod compute-popup-window-position ((gravity gravity-topright) source-window width height)
  (let* ((b2 (* +border-size+ 2))
         (win-x (window-x source-window))
         (win-y (window-y source-window))
         (win-w (max (- (window-width  source-window) b2 2)
                     +min-width+))
         (win-h (max (- (window-height source-window) b2)
                     +min-height+))
         (width  (max (+ width *extra-width-margin*)
                      +min-width+))
         (height (max height
                      +min-height+))
         (x (+ win-x (- win-w width)))
         (y (+ win-y 1))
         (w width)
         (h height))
    ;; calc y and h
    (when (> (+ y height) (+ win-y win-h))
      (decf h (- (+ y height) (+ win-y win-h))))
    (when (<= h 0)    ; for safety
      (setf y win-y)
      (setf h (min height win-h)))
    ;; calc x and w
    (when (< x win-x) ; for safety
      (setf x win-x)
      (setf w (min width win-w)))
    (values x y w h)))

(defclass popup-window (floating-window)
  ((gravity
    :initarg :gravity
    :reader popup-window-gravity)
   (source-window
    :initarg :source-window
    :reader popup-window-source-window)
   (base-width
    :initarg :base-width
    :reader popup-window-base-width)
   (base-height
    :initarg :base-height
    :reader popup-window-base-height))
  (:default-initargs
   :border +border-size+))

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute
  (t :background "#444" :foreground "white"))

(defmethod window-redraw ((popup-window popup-window) force)
  (adjust-for-redrawing (popup-window-gravity popup-window) popup-window)
  (call-next-method))

(defun popup-window (source-window buffer width height &key destination-window (gravity :cursor))
  (let ((gravity (ensure-gravity gravity)))
    (multiple-value-bind (x y w h)
        (compute-popup-window-position gravity source-window width height)
      (cond (destination-window
             (lem::window-set-size destination-window w h)
             (lem::window-set-pos destination-window
                                  (+ x +border-size+)
                                  (+ y +border-size+))
             destination-window)
            (t
             (make-instance 'popup-window
                            :buffer buffer
                            :x (+ x +border-size+)
                            :y (+ y +border-size+)
                            :width  w
                            :height h
                            :use-modeline-p nil
                            :gravity gravity
                            :source-window source-window
                            :base-width  width
                            :base-height height))))))

(defun quit-popup-window (floating-window)
  (delete-window floating-window))

(defun focus-point ()
  (alexandria:when-let (buffer *menu-buffer*)
    (buffer-point buffer)))

(defun update-focus-overlay (point)
  (when *focus-overlay*
    (delete-overlay *focus-overlay*))
  (when point
    (with-point ((start point)
                 (end point))
      (setf *focus-overlay*
            (make-overlay (line-start start)
                          (line-end end)
                          *focus-attribute*)))))

(defgeneric apply-print-spec (print-spec point item)
  (:method ((print-spec function) point item)
    (let ((string (funcall print-spec item)))
      (insert-string point string))))

(defun fill-background (buffer background-color)
  (with-point ((p (buffer-start-point buffer))
               (start (buffer-start-point buffer)))
    (flet ((put-attribute (start end attribute)
             (put-text-property
              start end
              :attribute (make-attribute
                          :foreground
                          (or (and attribute
                                   (attribute-foreground attribute))
                              (alexandria:when-let
                                  (attribute (ensure-attribute *non-focus-attribute* nil))
                                (attribute-foreground attribute)))
                          :background background-color
                          :bold-p (and attribute
                                       (attribute-bold-p attribute))
                          :underline-p (and attribute
                                            (attribute-underline-p attribute))))))
      (loop
        (let ((start-attribute (ensure-attribute (text-property-at p :attribute) nil)))
          (unless (next-single-property-change p :attribute)
            (put-attribute start (buffer-end-point buffer) start-attribute)
            (return))
          (put-attribute start p start-attribute)
          (move-point start p))))))

(defun create-menu-buffer (items print-spec)
  (let* ((buffer (or *menu-buffer*
                     (make-buffer "*popup menu*" :enable-undo-p nil :temporary t)))
         (point (buffer-point buffer))
         (width 0))
    (erase-buffer buffer)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (with-point ((start point :right-inserting))
      (loop :for (item . continue-p) :on items
            :for linum :from 0
            :do (move-point start point)
                (insert-character point #\space)
                (apply-print-spec print-spec point item)
                (line-end point)
                (put-text-property start point :item item)
                (setf width (max width (+ 1 (point-column point))))
                (when continue-p
                  (insert-character point #\newline))))
    (buffer-start point)
    (update-focus-overlay point)
    (with-point ((p (buffer-start-point buffer) :left-inserting))
      (loop
        :do (move-to-column p width t)
        :while (line-offset p 1)))
    (fill-background buffer
                     (alexandria:when-let
                         (attribute (ensure-attribute *non-focus-attribute* nil))
                       (attribute-background attribute)))
    (setf *menu-buffer* buffer)
    (values buffer width)))

(defun get-focus-item ()
  (alexandria:when-let (p (focus-point))
    (text-property-at (line-start p) :item)))

(defmethod lem-if:display-popup-menu (implementation items
                                      &key action-callback
                                           print-spec
                                           (focus-attribute 'popup-menu-attribute)
                                           (non-focus-attribute 'non-focus-popup-menu-attribute))
  (setf *print-spec* print-spec)
  (setf *action-callback* action-callback)
  (setf *focus-attribute* focus-attribute)
  (setf *non-focus-attribute* non-focus-attribute)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items print-spec)
    (setf *menu-window*
          (popup-window (current-window)
                        buffer
                        width
                        (min 20 (length items))))))

(defmethod lem-if:popup-menu-update (implementation items)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items *print-spec*)
    (update-focus-overlay (buffer-point buffer))
    (popup-window (current-window)
                  buffer
                  width
                  (min 20 (length items))
                  :destination-window *menu-window*)))

(defmethod lem-if:popup-menu-quit (implementation)
  (when *focus-overlay*
    (delete-overlay *focus-overlay*))
  (quit-popup-window *menu-window*)
  (when *menu-buffer*
    (delete-buffer *menu-buffer*)
    (setf *menu-buffer* nil)))

(defun move-focus (function)
  (alexandria:when-let (point (focus-point))
    (funcall function point)
    (window-see *menu-window*)
    (update-focus-overlay point)))

(defmethod lem-if:popup-menu-down (implementation)
  (move-focus
   (lambda (point)
     (unless (line-offset point 1)
       (buffer-start point)))))

(defmethod lem-if:popup-menu-up (implementation)
  (move-focus
   (lambda (point)
     (unless (line-offset point -1)
       (buffer-end point)))))

(defmethod lem-if:popup-menu-first (implementation)
  (move-focus
   (lambda (point)
     (buffer-start point))))

(defmethod lem-if:popup-menu-last (implementation)
  (move-focus
   (lambda (point)
     (buffer-end point))))

(defmethod lem-if:popup-menu-select (implementation)
  (alexandria:when-let ((f *action-callback*)
                        (item (get-focus-item)))
    (funcall f item)))

(defun compute-size-from-buffer (buffer)
  (flet ((compute-height ()
           (buffer-nlines buffer))
         (compute-width ()
           (with-point ((p (buffer-point buffer)))
             (buffer-start p)
             (loop
               :maximize (string-width (line-string p))
               :while (line-offset p 1)))))
    (list (compute-width)
          (compute-height))))

(defun make-popup-buffer (text)
  (let ((buffer (make-buffer "*Popup Message*" :temporary t :enable-undo-p nil)))
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defstruct (popup-parameters (:constructor %make-popup-parameters))
  buffer
  timeout
  size
  gravity
  destination-window)

(defun make-popup-buffer-from-string (string size)
  (let* ((buffer (make-popup-buffer string))
         (size (or size (compute-size-from-buffer buffer))))
    (values buffer size)))

(defun make-popup-parameters (buffer-or-string &rest args &key timeout size gravity destination-window)
  (declare (ignore timeout gravity destination-window))
  (multiple-value-bind (buffer size)
      (etypecase buffer-or-string
        (string
         (make-popup-buffer-from-string buffer-or-string size))
        (buffer
         (values buffer-or-string size)))
    (apply #'%make-popup-parameters
           :buffer buffer
           :size size
           (alexandria:remove-from-plist args :size))))

(defun display-popup-buffer-default-impl (popup-parameters)
  (with-slots (buffer timeout size gravity destination-window) popup-parameters
    (let ((size (or size (compute-size-from-buffer buffer))))
      (destructuring-bind (width height) size
        (delete-popup-message destination-window)
        (let ((window (popup-window (current-window) buffer width height :gravity gravity)))
          (buffer-start (window-view-point window))
          (window-see window)
          (when timeout
            (check-type timeout number)
            (start-timer (round (* timeout 1000))
                         nil
                         (lambda ()
                           (unless (deleted-window-p window)
                             (delete-window window)))))
          window)))))

(defmethod lem-if:display-popup-message (implementation buffer-or-string &key timeout size gravity destination-window)
  (display-popup-buffer-default-impl
   (make-popup-parameters buffer-or-string
                          :timeout timeout
                          :size size
                          :gravity (or gravity :cursor)
                          :destination-window destination-window)))

(defmethod lem-if:delete-popup-message (implementation popup-message)
  (when (and popup-message (not (deleted-window-p popup-message)))
    (delete-window popup-message)))

(defmethod lem::show-message (string)
  (cond ((null string)
         (delete-popup-message (frame-message-window (current-frame)))
         (setf (frame-message-window (current-frame)) nil))
        (t
         (setf (frame-message-window (current-frame))
               (display-popup-message string
                                      :timeout nil
                                      :destination-window (frame-message-window (current-frame))
                                      :gravity :follow-cursor)))))

(defmethod lem::show-message-buffer (buffer)
  (setf (frame-message-window (current-frame))
        (display-popup-message buffer
                               :timeout nil
                               :destination-window (frame-message-window (current-frame))
                               :gravity :follow-cursor)))
