(defpackage :lem-jsonrpc
  (:use :cl :lem)
  (:export :notify
           :params
           :define-notification-method))
(in-package :lem-jsonrpc)

(defclass jsonrpc (implementation)
  ()
  (:default-initargs
   :native-scroll-support t
   :redraw-after-modifying-floating-window nil))

(defparameter *debug* nil)

(defvar *view-id-counter* 0)
(defvar *display-width* 80)
(defvar *display-height* 24)

(defvar *main-thread*)
(defvar *editor-thread*)
(defvar *server*)

(defvar *background-mode*)

(setf *implementation* (make-instance 'jsonrpc))

(when *debug*
  (setq *error-output*
        (open "~/ERROR" :direction :output :if-does-not-exist :create :if-exists :supersede)))

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
  (let ((v (get-rgb-from-color-name color)))
    (if (null v)
        color
        (destructuring-bind (r g b) v
          (format nil "#~2,'0X~2,'0X~2,'0X" r g b)))))

(defmethod yason:encode ((attribute lem::attribute) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "foreground" (ensure-rgb (attribute-foreground attribute)))
      (yason:encode-object-element "background" (ensure-rgb (attribute-background attribute)))
      (yason:encode-object-element "reverse" (bool (attribute-reverse-p attribute)))
      (yason:encode-object-element "bold" (bool (attribute-bold-p attribute)))
      (yason:encode-object-element "underline" (bool (attribute-underline-p attribute))))))

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

(let ((lock (bt:make-lock)))
  (defun dbg (x)
    (when *debug*
      (bt:with-lock-held (lock)
        (with-open-file (out "~/log"
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
          (write-string x out)
          (terpri out))))
    x))

(defmacro with-error-handler (() &body body)
  `(handler-case
       (handler-bind ((error (lambda (c)
                               (dbg (format nil "~%******ERROR******:~%~A~%" c))
                               (uiop:print-backtrace :stream *error-output* :condition c))))
         (progn ,@body))
     (error ())))

(defun params (&rest args)
  (alexandria:plist-hash-table args :test #'equal))

(defun notify (method argument)
  #+(or)
  (dbg (format nil "~A:~A"
               method
               (with-output-to-string (*standard-output*)
                 (yason:encode argument))))
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
        (alexandria:when-let (color (or (get-rgb-from-color-name background) (parse-color background)))
          (destructuring-bind (r g b) color
            (lem::set-display-background-mode (rgb-to-background-mode r g b))))
        (funcall loaded-fn)
        (params "width" *display-width*
                "height" *display-height*)))))

(defmethod lem-if:invoke ((implementation jsonrpc) function)
  ;(swank:create-server :port 10005 :dont-close t)
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
      (dbg "server-listen")
      (jsonrpc:server-listen *server* :mode :stdio))))

(defmethod lem-if:display-background-mode ((implementation jsonrpc))
  *background-mode*)

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
                           :kind (cond ((lem::minibuffer-window-p window)
                                        "minibuffer")
                                       ((lem::floating-window-p window)
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
            "chars" (map 'list
                         (lambda (c)
                           (let* ((octets (babel:string-to-octets (string c)))
                                  (bytes (make-array (1+ (length octets)))))
                             (setf (aref bytes 0) (if (wide-char-p c) 2 1))
                             (replace bytes octets :start1 1)
                             bytes))
                         string)
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

(defmethod lem-if:redraw-view-after ((implementation jsonrpc) view focus-window-p)
  (with-error-handler ()
    (when focus-window-p
      (move-cursor implementation
                   view
                   lem::*cursor-x*
                   lem::*cursor-y*))
    (notify "touch" (params "viewInfo" view))))

(defmethod lem-if:scroll ((implementation jsonrpc) view n)
  (with-error-handler ()
    (notify "scroll"
            (params "viewInfo" view "n" n))))

(defmethod lem-if:update-display ((implementation jsonrpc))
  (with-error-handler ()
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
               (let ((key (convert-keyevent value)))
                 (send-event key)))
              ((= kind +resize+)
               (resize (gethash "width" value)
                       (gethash "height" value))
               (send-event :resize))
              ((= kind +command+)
               (send-event (lambda ()
                             (apply (lem::find-command-symbol (first value))
                                    (rest value))
                             (redraw-display))))
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
      (dbg (format nil "~%******ERROR******:~%~A~%" e)))))

(add-hook *exit-editor-hook*
          (lambda ()
            (notify "exit" nil)))
