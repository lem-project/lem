(defpackage :lem.popup-window
  (:use :cl :lem))
(in-package :lem.popup-window)

(defvar *menu-buffer* nil)
(defvar *menu-window* nil)
(defvar *focus-overlay* nil)
(defvar *print-function* nil)
(defvar *action-callback* nil)
(defvar *focus-attribute* nil)
(defvar *non-focus-attribute* nil)

(define-attribute popup-menu-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-popup-menu-attribute
  (t :foreground "black" :background "gray"))

(defun redraw-display* ()
  (when (redraw-after-modifying-floating-window (implementation))
    (redraw-display t)))

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
           (redraw-display*)
           dst-window)
          (t
           (make-floating-window buffer x y width height nil)))))

(defun quit-popup-window (floating-window)
  (delete-window floating-window)
  (redraw-display*))

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

(defun create-menu-buffer (items print-function)
  (let ((buffer (or *menu-buffer*
                    (make-buffer "*popup menu*" :enable-undo-p nil :temporary t)))
        (item-names (mapcar print-function items)))
    (setf *menu-buffer* buffer)
    (erase-buffer buffer)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (let ((point (buffer-point buffer))
          (width (+ 1 (reduce (lambda (max name)
                                (max max (string-width name)))
                              item-names
                              :initial-value 0))))
      (loop :for rest-items :on items
            :for item := (car rest-items)
            :for item-name :in item-names
            :do
            (insert-string point item-name)
            (move-to-column point width t)
            (with-point ((start (line-start (copy-point point :temporary))))
              (put-text-property start point :item item))
            (when (cdr rest-items)
              (insert-character point #\newline)))
      (buffer-start point)
      (put-text-property (buffer-start-point buffer)
                         (buffer-end-point buffer)
                         :attribute *non-focus-attribute*)
      (buffer-start point)
      (update-focus-overlay point)
      (values buffer width))))

(defun get-focus-item ()
  (alexandria:when-let (p (focus-point))
    (text-property-at (line-start p) :item)))

(defmethod lem-if:display-popup-menu (implementation items
                                      &key action-callback
                                           print-function
                                           (focus-attribute 'popup-menu-attribute)
                                           (non-focus-attribute 'non-focus-popup-menu-attribute))
  (setf *print-function* print-function)
  (setf *action-callback* action-callback)
  (setf *focus-attribute* focus-attribute)
  (setf *non-focus-attribute* non-focus-attribute)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items print-function)
    (setf *menu-window*
          (popup-window (current-window)
                         buffer
                         width
                         (min 20 (length items))))))

(defmethod lem-if:popup-menu-update (implementation items)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items *print-function*)
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

(defmethod lem-if:delete-popup-message (implementation popup-message)
  (when (windowp popup-message)
    (delete-window popup-message)
    (redraw-display*)))
