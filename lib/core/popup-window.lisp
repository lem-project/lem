(defpackage :lem.popup-window
  (:use :cl :lem)
  (:export :get-focus-item
           :apply-print-spec))
(in-package :lem.popup-window)

(defvar *menu-buffer* nil)
(defvar *menu-window* nil)
(defvar *focus-overlay* nil)
(defvar *print-spec* nil)
(defvar *action-callback* nil)
(defvar *focus-attribute* nil)
(defvar *non-focus-attribute* nil)

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute
  (t :background "#444" :foreground "white"))

(defun compute-popup-window-position (orig-window)
  (let* ((y (+ (window-y orig-window)
               (window-cursor-y orig-window)
               1))
         (x (+ (window-x orig-window)
               (let ((x (point-column (lem::window-buffer-point orig-window))))
                 (when (<= (window-width orig-window) x)
                   (let ((mod (mod x (window-width orig-window)))
                         (floor (floor x (window-width orig-window))))
                     (setf x (+ mod floor))
                     (incf y floor)))
                 x))))
    (values x y)))

(defun popup-window (orig-window buffer width height &optional dst-window)
  (multiple-value-bind (x y)
      (compute-popup-window-position orig-window)
    (cond
      ((<= (display-height)
           (+ y (min height
                     (floor (display-height) 3))))
       (cond ((>= 0 (- y height))
              (setf y 1)
              (setf height (min height (- (display-height) 1))))
             (t
              (decf y (+ height 1)))))
      ((<= (display-height) (+ y height))
       (setf height (- (display-height) y))))
    (when (<= (display-width) (+ x width))
      (when (< (display-width) width)
        (setf width (display-width)))
      (setf x (- (display-width) width)))
    (cond (dst-window
           (lem::window-set-size dst-window width height)
           (lem::window-set-pos dst-window x y)
           dst-window)
          (t
           (make-floating-window buffer x y width height nil)))))

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
                   *menu-window*)))

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

(define-attribute popup-window-attribute
  (:light :background "gray" :foreground "black")
  (:dark :background "white" :foreground "blue"))

(defvar *popup-message-window* nil)

(defun clear-popup-message ()
  (when *popup-message-window*
    (delete-window *popup-message-window*)
    (setf *popup-message-window* nil)
    (redraw-display*)))

(defun make-popup-buffer (text)
  (let ((buffer (make-buffer "*Popup Message*" :temporary t :enable-undo-p nil)))
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (erase-buffer buffer)
    (let ((p (buffer-point buffer))
          (max-column 0))
      (insert-string p text)
      (buffer-start p)
      (loop :for column := (point-column (line-end p))
            :do (setf max-column (max max-column column))
            :while (line-offset p 1))
      (buffer-start p)
      (loop :do (move-to-column p max-column t)
            :while (line-offset p 1))
      (put-text-property (buffer-start-point buffer)
                         (buffer-end-point buffer)
                         :attribute 'popup-window-attribute)
      (values buffer
              max-column
              (buffer-nlines buffer)))))

(defmethod lem-if:display-popup-message (implementation text timeout)
  (clear-popup-message)
  (multiple-value-bind (buffer width height)
      (make-popup-buffer text)
    (let ((window (popup-window (current-window) buffer width height)))
      (buffer-start (window-view-point window))
      (window-see window)
      (setf *popup-message-window* window)
      (when timeout
        (check-type timeout (integer 0 *))
        (start-timer (* timeout 1000) nil 'clear-popup-message))
      window)))

(defmethod lem-if:display-popup-buffer (implementation buffer width height timeout)
  (clear-popup-message)
  (let ((window (popup-window (current-window) buffer width height)))
    (buffer-start (window-view-point window))
    (window-see window)
    (setf *popup-message-window* window)
    (when timeout
      (check-type timeout (integer 0 *))
      (start-timer (* timeout 1000) nil 'clear-popup-message))
    window))

(defmethod lem-if:delete-popup-message (implementation popup-message)
  (when (windowp popup-message)
    (delete-window popup-message)))
