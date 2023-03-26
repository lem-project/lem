(defpackage :lem/context-menu
  (:use :cl
        :lem
        :lem/multi-column-list)
  (:export :make-item
           :display-context-menu)
  #+sbcl
  (:lock t))
(in-package :lem/context-menu)

(defclass item (multi-column-list-item)
  ((label :initarg :label
          :reader item-label)
   (description :initarg :description
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
  )

(defmethod row-values ((component context-menu) (item item))
  (list (item-label item) (or (item-description item) "")))

(defun display-context-menu (items)
  (display (make-instance 'context-menu :items items)))

(define-command test-context-menu () ()
  (display-context-menu (list (make-item :label "foo" :callback (lambda () (message "select foo")))
                              (make-item :label "bar" :callback (lambda () (message "select bar")))
                              (make-item :label "baz" :callback (lambda () (message "select baz"))))))
