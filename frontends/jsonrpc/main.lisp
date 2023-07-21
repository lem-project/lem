(defpackage :lem-jsonrpc
  (:use :cl :lem)
  (:export :notify
           :params
           :define-notification-method))
(in-package :lem-jsonrpc)

(defclass jsonrpc (implementation)
  ()
  (:default-initargs
   :name :jsonrpc
   :redraw-after-modifying-floating-window nil))

(defvar *mode* :stdio)
(defvar *port* 50879)

(defvar *view-id-counter* 0)
(defvar *display-width* 80)
(defvar *display-height* 24)

(defvar *main-thread*)
(defvar *editor-thread*)
(defvar *server*)

(defvar *background-color*)

(defstruct view
  (id (incf *view-id-counter*))
  x
  y
  width
  height
  use-modeline
  kind)

(defun bool (x) (if x 'yason:true 'yason:false))

(defun ensure-rgb (color)
  (let ((v (parse-color color)))
    (if (null v)
        color
        (format nil "#~2,'0X~2,'0X~2,'0X"
                (color-red color)
                (color-green color)
                (color-blue color)))))

(defmethod yason:encode ((attribute attribute) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "foreground" (ensure-rgb (attribute-foreground attribute)))
      (yason:encode-object-element "background" (ensure-rgb (attribute-background attribute)))
      (yason:encode-object-element "reverse" (bool (attribute-reverse attribute)))
      (yason:encode-object-element "bold" (bool (attribute-bold attribute)))
      (yason:encode-object-element "underline" (bool (attribute-underline attribute))))))

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view))
      (yason:encode-object-element "x" (view-x view))
      (yason:encode-object-element "y" (view-y view))
      (yason:encode-object-element "width" (view-width view))
      (yason:encode-object-element "height" (view-height view))
      (yason:encode-object-element "use_modeline" (view-use-modeline view))
      (yason:encode-object-element "kind" (view-kind view)))))

(defparameter *error-log-file* (merge-pathnames "lem-jsonrpc-error-log" (user-homedir-pathname)))

(defun log-error (value)
  (with-open-file (out *error-log-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A~%" value)))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (log-error (with-output-to-string (stream)
                                            (format stream "~A~%" c)
                                            (uiop:print-backtrace :stream stream
                                                                  :condition c))))))
         (progn ,@body))
     (error ())))

(defun params (&rest args)
  (alexandria:plist-hash-table args :test #'equal))

(defvar *notify-output-stream* nil)

(defun notify-log (method argument)
  (when *notify-output-stream*
    (format *notify-output-stream*
            "~A:~A~%"
            method
            (with-output-to-string (out)
              (yason:encode argument
                            (yason:make-json-output-stream out))))))

(defun notify (method argument)
  (notify-log method argument)
  (let ((jsonrpc/connection:*connection*
          (jsonrpc/transport/interface:transport-connection
           (jsonrpc/class:jsonrpc-transport *server*))))
    (jsonrpc:notify *server* method argument)))

(defun resize (width height)
  (setf *display-width* width)
  (setf *display-height* height))

(defun ready (loaded-fn)
  (lambda (params)
    (with-error-handler ()
      (let ((width (gethash "width" params))
            (height (gethash "height" params))
            (foreground (gethash "foreground" params))
            (background (gethash "background" params)))
        (declare (ignore foreground))
        (resize width height)
        (alexandria:when-let (color (parse-color background))
          (setq *background-color* color))
        (funcall loaded-fn)
        (params "width" *display-width*
                "height" *display-height*)))))

(defmethod lem-if:invoke ((implementation jsonrpc) function)
  (with-error-handler ()
    (let ((ready nil))
      (setf *main-thread* (bt:current-thread))
      (setf *editor-thread*
            (funcall function
                     (lambda ()
                       (loop :until ready))))
      (setf *server* (jsonrpc:make-server))
      (jsonrpc:expose *server* "ready" (ready (lambda () (setf ready t))))
      (jsonrpc:expose *server* "input" 'input-callback)
      (log:info "server-listen")
      (apply #'jsonrpc:server-listen *server*
             :mode *mode*
             (if (eq *mode* :tcp) (list :port *port*))))))

(defmethod lem-if:get-background-color ((implementation jsonrpc))
  *background-color*)

(defmethod lem-if:update-foreground ((implementation jsonrpc) color-name)
  (notify "update-foreground" color-name))

(defmethod lem-if:update-background ((implementation jsonrpc) color-name)
  (notify "update-background" color-name))

(defmethod lem-if:display-width ((implementation jsonrpc))
  *display-width*)

(defmethod lem-if:display-height ((implementation jsonrpc))
  *display-height*)

(defmethod lem-if:make-view
    ((implementation jsonrpc) window x y width height use-modeline)
  (with-error-handler ()
    (let ((view (make-view :x x :y y :width width :height height :use-modeline use-modeline
                           :kind (cond ((lem:floating-window-p window)
                                        "popup")
                                       (t
                                        nil)))))
      (notify "make-view" view)
      view)))

(defmethod lem-if:delete-view ((implementation jsonrpc) view)
  (with-error-handler ()
    (notify "delete-view" (params "viewInfo" view))))

(defmethod lem-if:set-view-size ((implementation jsonrpc) view width height)
  (with-error-handler ()
    (setf (view-width view) width
          (view-height view) height)
    (notify "resize-view"
            (params "viewInfo" view
                    "width" width
                    "height" height))))

(defmethod lem-if:set-view-pos ((implementation jsonrpc) view x y)
  (with-error-handler ()
    (setf (view-x view) x
          (view-y view) y)
    (notify "move-view"
            (params "viewInfo" view
                    "x" x
                    "y" y))))

(defmethod lem-if:clear ((implementation jsonrpc) view)
  (with-error-handler ()
    (notify "clear" (params "viewInfo" view))))

(defmethod lem-if:clear-eol ((implementation jsonrpc) view x y)
  (with-error-handler ()
    (notify "clear-eol"
            (params "viewInfo" view "x" x "y" y))))

(defmethod lem-if:clear-eob ((implementation jsonrpc) view x y)
  (with-error-handler ()
    (assert (= x 0))
    (notify "clear-eob" (params "viewInfo" view "x" x "y" y))))

(defun put-params (view x y string attribute)
  (with-error-handler ()
    (params "viewInfo" view
            "x" x
            "y" y
            "text" string
            "textWidth" (string-width string)
            "attribute" (ensure-attribute attribute nil))))

(defmethod lem-if:print ((implementation jsonrpc) view x y string attribute)
  (with-error-handler ()
    (notify "put" (put-params view x y string attribute))))

(defmethod lem-if:print-modeline
    ((implementation jsonrpc) view x y string attribute)
  (with-error-handler ()
    (notify "modeline-put" (put-params view x y string attribute))))

(defmethod move-cursor ((implementation jsonrpc) view x y)
  (with-error-handler ()
    (notify "move-cursor"
            (params "viewInfo" view "x" x "y" y))))

(defmethod lem-if:redraw-view-after ((implementation jsonrpc) view)
  (with-error-handler ()
    (notify "touch" (params "viewInfo" view))))

(defmethod lem-if:update-display ((implementation jsonrpc))
  (with-error-handler ()
    (move-cursor implementation
                 (window-view (current-window))
                 (last-print-cursor-x (current-window))
                 (last-print-cursor-y (current-window)))
    (notify "update-display" nil)))


(defmacro define-enum (name &rest vars)
  (declare (ignore name))
  `(progn
     ,@(loop :for v :in vars
             :for n :from 0
             :collect `(defconstant ,v ,n))))

(define-enum ()
  +abort+
  +keyevent+
  +resize+
  +command+
  +method+)

(defvar *method-table* (make-hash-table :test 'equal))

(defmacro define-notification-method (name params &body body)
  `(setf (gethash ,name *method-table*)
         (lambda (&key ,@params) ,@body)))

(defun convert-keyevent (e)
  (let ((key (gethash "key" e))
        (ctrl (gethash "ctrl" e))
        (meta (gethash "meta" e))
        (super (gethash "super" e))
        (shift (gethash "shift" e)))
    (cond ((string= key " ") (setf key "Space")))
    (make-key :ctrl ctrl
              :meta meta
              :super super
              :shift (if (insertion-key-sym-p key) nil shift)
              :sym key)))

(defun input-callback (args)
  (handler-case
      (let ((kind (gethash "kind" args))
            (value (gethash "value" args)))
        (cond ((= kind +abort+)
               (send-abort-event *editor-thread* nil))
              ((= kind +keyevent+)
               (when value
                 (let ((key (convert-keyevent value)))
                   (send-event key))))
              ((= kind +resize+)
               (resize (gethash "width" value)
                       (gethash "height" value))
               (send-event :resize))
              ((= kind +command+)
               (error "unimplemented"))
              ((= kind +method+)
               (let* ((method (gethash (gethash "method" value) *method-table*))
                      (params (gethash "params" value))
                      (args
                        (loop :for k :being :the :hash-keys :in params :using (hash-value v)
                              :collect (intern (string-upcase k) :keyword)
                              :collect v)))
                 (send-event (lambda () (apply method args)))))
              (t
               (error "unexpected kind: ~D" kind))))
    (error (e)
      (log-error e)
      (log-error (with-output-to-string (stream)
                   (let ((stream (yason:make-json-output-stream stream)))
                     (yason:encode args stream)))))))

(add-hook *exit-editor-hook*
          (lambda ()
            (notify "exit" nil)))

#+(or)
(lem:add-hook lem:*after-init-hook*
              (lambda ()
                (swank:create-server :dont-close t :port 12345)))
