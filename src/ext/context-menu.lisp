(defpackage :lem/context-menu
  (:use :cl
        :lem
        :lem/multi-column-list)
  (:export :context-menu
           :item
           :display-context-menu)
  #+sbcl
  (:lock t))
(in-package :lem/context-menu)

(defclass item (multi-column-list-item)
  ((label :initarg :label
          :reader item-label)
   (description :initarg :description
                :initform nil
                :reader item-description)
   (callback :initarg :callback
             :reader item-callback)))

(defun make-item (&key label description callback)
  (make-instance 'item
                 :label label
                 :description description
                 :callback callback))

(defclass context-menu (multi-column-list) ()
  (:default-initargs :columns '()))

(defmethod select-item ((component context-menu) item)
  (let ((window (window-parent (lem/multi-column-list::multi-column-list-window component))))
    (quit component)
    (funcall (item-callback item) window)))

(defmethod map-columns ((component context-menu) (item item))
  (list (item-label item) (or (item-description item) "")))

(defun display-context-menu (items)
  (display (make-instance 'context-menu :items items)
           :style '(:gravity :cursor)))

(defmethod lem-if:display-context-menu (implementation context-menu style)
  (display context-menu
           :style style))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select foo")))
                              (make-item :label "bar"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select bar")))
                              (make-item :label "baz"
                                         :callback (lambda (window)
                                                     (declare (ignore window))
                                                     (message "select baz"))))))
