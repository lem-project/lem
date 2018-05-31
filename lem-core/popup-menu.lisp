(defpackage :lem.popup-menu
  (:use :cl :lem))
(in-package :lem.popup-menu)

(defvar *menu-buffer* nil)
(defvar *menu-window* nil)
(defvar *focus-overlay* nil)
(defvar *print-function* nil)
(defvar *action-callback* nil)
(defvar *focus-attribute* nil)
(defvar *non-focus-attribute* nil)

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

(defmethod lem-if:display-popup-menu (implementation items &key action-callback print-function
                                                     focus-attribute non-focus-attribute)
  (setf *print-function* print-function)
  (setf *action-callback* action-callback)
  (setf *focus-attribute* focus-attribute)
  (setf *non-focus-attribute* non-focus-attribute)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items print-function)
    (setf *menu-window*
          (balloon (current-window)
                   buffer
                   width
                   (min 20 (length items))))))

(defmethod lem-if:popup-menu-update (implementation items)
  (multiple-value-bind (buffer width)
      (create-menu-buffer items *print-function*)
    (update-focus-overlay (buffer-point buffer))
    (balloon (current-window)
             buffer
             width
             (min 20 (length items))
             *menu-window*)))

(defmethod lem-if:popup-menu-quit (implementation)
  (when *focus-overlay*
    (delete-overlay *focus-overlay*))
  (quit-balloon *menu-window*)
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
