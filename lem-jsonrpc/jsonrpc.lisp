(defpackage :lem-jsonrpc
  (:use :cl :lem))
(in-package :lem-jsonrpc)

(defvar *view-id-counter* 0)
(defvar *display-width* 80)
(defvar *display-height* 24)

(defvar *main-thread*)
(defvar *editor-thread*)
(defvar *server*)

(defstruct view
  (id (incf *view-id-counter*))
  x
  y
  width
  height
  use-modeline)

(defun bool (x) (if x 'yason:true 'yason:false))

(defmethod yason:encode ((attribute lem::attribute) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "foreground" (attribute-foreground attribute))
      (yason:encode-object-element "background" (attribute-background attribute))
      (yason:encode-object-element "reverse" (bool (attribute-reverse-p attribute)))
      (yason:encode-object-element "bold" (bool (attribute-bold-p attribute)))
      (yason:encode-object-element "underline" (bool (attribute-underline-p attribute))))))

(let ((lock (bt:make-lock)))
  (defun dbg (x)
    (bt:with-lock-held (lock)
      (with-open-file (out "~/log"
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (write-string x out)
        (terpri out)))
    x))

(defun params (&rest args)
  (alexandria:plist-hash-table args))

(defun notify (method argument)
  (dbg (format nil "~A:~A"
               method
               (with-output-to-string (*standard-output*)
                 (yason:encode argument))))
  (let ((jsonrpc/connection:*connection*
          (jsonrpc/transport/interface:transport-connection
           (jsonrpc/class:jsonrpc-transport *server*))))
    (jsonrpc:notify *server* method argument)))

(defmethod lem::interface-invoke ((implementation (eql :jsonrpc)) function)
  (setf *main-thread* (bt:current-thread))
  (setf *editor-thread*
        (funcall function
                 (lambda () (sleep 2))))
  (setf *server* (jsonrpc:make-server))
  (jsonrpc:expose *server* "input" 'input-callback)
  (dbg "server-listen")
  (jsonrpc:server-listen *server* :mode :stdio))

(defmethod lem::interface-display-background-mode ((implementation (eql :jsonrpc)))
  :dark)

(defmethod lem::interface-update-foreground ((implementation (eql :jsonrpc)) color-name)
  (notify "update-foreground" color-name))

(defmethod lem::interface-update-background ((implementation (eql :jsonrpc)) color-name)
  (notify "update-background" color-name))

(defmethod lem::interface-display-width ((implementation (eql :jsonrpc)))
  *display-width*)

(defmethod lem::interface-display-height ((implementation (eql :jsonrpc)))
  *display-height*)

(defmethod lem::interface-make-view
    ((implementation (eql :jsonrpc)) x y width height use-modeline)
  (make-view :x x :y y :width width :height height :use-modeline use-modeline))

(defmethod lem::interface-delete-view ((implementation (eql :jsonrpc)) view)
  (values))

(defmethod lem::interface-clear ((implementation (eql :jsonrpc)) view)
  (notify "clear"
          (params "x" (view-x view)
                  "y" (view-y view)
                  "width" (view-width view)
                  "height" (view-height view))))

(defmethod lem::interface-set-view-size ((implementation (eql :jsonrpc)) view width height)
  (setf (view-width view) width
        (view-height view) height))

(defmethod lem::interface-set-view-pos ((implementation (eql :jsonrpc)) view x y)
  (setf (view-x view) x
        (view-y view) y))

(defun put-line-text (view x y text attribute)
  (notify "put-line-text"
          (params "x" (+ (view-x view) x)
                  "y" (+ (view-y view) y)
                  "text" text
                  "attribute" (ensure-attribute attribute nil))))

(defmethod lem::interface-print ((implementation (eql :jsonrpc)) view x y string attribute)
  (put-line-text view x y string attribute))

(defmethod lem::interface-print-modeline
    ((implementation (eql :jsonrpc)) view x y string attribute)
  (put-line-text view x (+ y (view-height view) -1) string attribute))

(defmethod lem::interface-clear-eol ((implementation (eql :jsonrpc)) view x y)
  (notify "clear"
          (params "x" (+ x (view-x view))
                  "y" (+ y (view-y view))
                  "width" (- (view-width view) x)
                  "height" 1)))

(defmethod lem::interface-clear-eob ((implementation (eql :jsonrpc)) view x y)
  (assert (= x 0))
  (notify "clear"
          (params "x" (view-x view)
                  "y" (+ (view-y view) y)
                  "width" (view-width view)
                  "height" (- (view-height view) y))))

(defmethod lem::interface-move-cursor ((implementation (eql :jsonrpc)) view x y)
  (notify "move-cursor"
          (params "x" (+ x (view-x view))
                  "y" (+ y (view-y view)))))

(defmethod lem::interface-redraw-view-after ((implementation (eql :jsonrpc)) view focus-window-p)
  )

(defmethod lem::interface-update-display ((implementation (eql :jsonrpc)))
  )


(defmacro define-enum (name &rest vars)
  (declare (ignore name))
  `(progn
     ,@(loop :for v :in vars
             :for n :from 0
             :collect `(defconstant ,v ,n))))

(define-enum ()
  +abort+
  +keycode+)

(defun input-callback (args)
  (let ((kind (gethash "kind" args))
        (value (gethash "value" args)))
    (dbg (format nil "~A:~A~%" kind value))
    (cond ((= kind +abort+)
           (send-abort-event *editor-thread* nil))
          ((= kind +keycode+)
           (send-event (code-char value)))
          (t
           (error "unexpected kind: ~D" kind)))))

(setf lem::*implementation* :jsonrpc)
