(defpackage :lem-jsonrpc
  (:use :cl :lem))
(in-package :lem-jsonrpc)

(defvar *view-id-counter* 0)
(defvar *display-width* 80)
(defvar *display-height* 24)

(defvar *main-thread*)
(defvar *editor-thread*)
(defvar *server*)

(defvar *background-mode*)

(setq *error-output* (open "~/ERROR" :direction :output :if-does-not-exist :create :if-exists :supersede))

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
    ;; (bt:with-lock-held (lock)
    ;;   (with-open-file (out "~/log"
    ;;                        :direction :output
    ;;                        :if-exists :append
    ;;                        :if-does-not-exist :create)
    ;;     (write-string x out)
    ;;     (terpri out)))
    x))

(defmacro with-error-handler (() &body body)
  `(handler-case (progn ,@body)
     (error (c)
       (dbg (format nil "~%******ERROR******:~%~A~%" c)))))

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
        (alexandria:when-let (color (or (get-rgb-from-color-name background) background))
          (destructuring-bind (r g b) color
            (lem::set-display-background-mode (rgb-to-background-mode r g b))))
        (funcall loaded-fn)
        (params "width" *display-width*
                "height" *display-height*)))))

(defmethod lem::interface-invoke ((implementation (eql :jsonrpc)) function)
  (swank:create-server :port 10005 :dont-close t)
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

(defmethod lem::interface-display-background-mode ((implementation (eql :jsonrpc)))
  *background-mode*)

(defmethod lem::interface-update-foreground ((implementation (eql :jsonrpc)) color-name)
  (notify "update-foreground" color-name))

(defmethod lem::interface-update-background ((implementation (eql :jsonrpc)) color-name)
  (notify "update-background" color-name))

(defmethod lem::interface-display-width ((implementation (eql :jsonrpc)))
  *display-width*)

(defmethod lem::interface-display-height ((implementation (eql :jsonrpc)))
  *display-height*)

(defmethod lem::interface-make-view
    ((implementation (eql :jsonrpc)) window x y width height use-modeline)
  (with-error-handler ()
    (let ((view (make-view :x x :y y :width width :height height :use-modeline use-modeline
                           :kind (cond ((lem::minibuffer-window-p window)
                                        "minibuffer")
                                       (t
                                        nil)))))
      (notify "make-view" view)
      view)))

(defmethod lem::interface-delete-view ((implementation (eql :jsonrpc)) view)
  (with-error-handler ()
    (notify "delete-view" (params "viewInfo" view))))

(defmethod lem::interface-set-view-size ((implementation (eql :jsonrpc)) view width height)
  (with-error-handler ()
    (setf (view-width view) width
          (view-height view) height)
    (notify "resize-view"
            (params "viewInfo" view
                    "width" width
                    "height" height))))

(defmethod lem::interface-set-view-pos ((implementation (eql :jsonrpc)) view x y)
  (with-error-handler ()
    (setf (view-x view) x
          (view-y view) y)
    (notify "move-view"
            (params "viewInfo" view
                    "x" x
                    "y" y))))

(defmethod lem::interface-clear ((implementation (eql :jsonrpc)) view)
  (with-error-handler ()
    (notify "clear" (params "viewInfo" view))))

(defmethod lem::interface-clear-eol ((implementation (eql :jsonrpc)) view x y)
  (with-error-handler ()
    (notify "clear-eol"
            (params "viewInfo" view "x" x "y" y))))

(defmethod lem::interface-clear-eob ((implementation (eql :jsonrpc)) view x y)
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

(defmethod lem::interface-print ((implementation (eql :jsonrpc)) view x y string attribute)
  (with-error-handler ()
    (notify "put" (put-params view x y string attribute))))

(defmethod lem::interface-print-modeline
    ((implementation (eql :jsonrpc)) view x y string attribute)
  (with-error-handler ()
    (notify "modeline-put" (put-params view x y string attribute))))

(defmethod lem::interface-move-cursor ((implementation (eql :jsonrpc)) view x y)
  (with-error-handler ()
    (notify "move-cursor"
            (params "viewInfo" view "x" x "y" y))))

(defmethod lem::interface-redraw-view-after ((implementation (eql :jsonrpc)) view focus-window-p)
  (with-error-handler ()
    (when focus-window-p
      (lem::interface-move-cursor implementation
                                  view
                                  lem::*cursor-x*
                                  lem::*cursor-y*))
    (notify "touch" (params "viewInfo" view))))

(defmethod lem::interface-scroll ((implementation (eql :jsonrpc)) view n)
  (with-error-handler ()
    (notify "scroll"
            (params "viewInfo" view "n" n))))

(defmethod lem::interface-update-display ((implementation (eql :jsonrpc)))
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
  +resize+)

(defvar *key-table*
  (alexandria:plist-hash-table
   (append (loop :for code :from (char-code #\a) :to (char-code #\z)
                 :for char := (code-char code)
                 :for lchar := (char-downcase char)
                 :for uchar := (char-upcase char)
                 :collect (string lchar) :collect lchar
                 :collect (string uchar) :collect uchar)
           (list "Backspace" (keyname->keychar "[backspace]")
                 "Tab" #\Tab
                 "Enter" #\Return
                 "PageDown" (keyname->keychar "[npage]")
                 "PageUp" (keyname->keychar "[ppage]")
                 "Home" (keyname->keychar "[home]")
                 "End" (keyname->keychar "[end]")
                 "AllowRight" (keyname->keychar "[right]")
                 "AllowLeft" (keyname->keychar "[left]")
                 "AllowUp" (keyname->keychar "[up]")
                 "AllowDown" (keyname->keychar "[down]")
                 "Insert" (keyname->keychar "[ic]")
                 "Delete" (keyname->keychar "[dc]")
                 "F1" (keyname->keychar "[f1]")
                 "F2" (keyname->keychar "[f2]")
                 "F3" (keyname->keychar "[f3]")
                 "F4" (keyname->keychar "[f4]")
                 "F5" (keyname->keychar "[f5]")
                 "F6" (keyname->keychar "[f6]")
                 "F7" (keyname->keychar "[f7]")
                 "F8" (keyname->keychar "[f8]")
                 "F9" (keyname->keychar "[f9]")
                 "F10" (keyname->keychar "[f10]")
                 "F11" (keyname->keychar "[f11]")
                 "F12" (keyname->keychar "[f12]")))
   :test #'equal))

(defun key-to-char (key)
  (or (gethash key *key-table*)
      (and (= 1 (length key))
           (graphic-char-p (aref key 0))
           (aref key 0))))

(defun convert-keyevent (e)
  (when e
    (let ((keys '()))
      (let ((key (gethash "key" e))
            ;(shift (gethash "shift" e))
            (ctrl (gethash "ctrl" e))
            (meta (gethash "meta" e))
            ;(super (gethash "super" e))
            )
        ;(dbg (format nil "key: ~A, ctrl: ~A, meta: ~A" key ctrl meta))
        (when (or meta (string= key "Escape"))
          (push (keyname->keychar "escape") keys))
        (let ((char (key-to-char key)))
          (when char
            (push (if ctrl
                      (cond ((alpha-char-p char)
                             (code-char (1+ (- (char-code (char-upcase char))
                                               (char-code #\A)))))
                            ((member char '(#\space #\@))
                             (keyname->keychar "C-@"))
                            ((char= char #\\)
                             (keyname->keychar "C-\\"))
                            ((char= char #\_)
                             (keyname->keychar "C-_"))
                            (t
                             char))
                      char)
                  keys))))
      (nreverse keys))))

(defun input-callback (args)
  (handler-case
      (let ((kind (gethash "kind" args))
            (value (gethash "value" args)))
        (cond ((= kind +abort+)
               (send-abort-event *editor-thread* nil))
              ((= kind +keyevent+)
               (let ((keys (convert-keyevent value)))
                 (dolist (char keys)
                   (send-event char))))
              ((= kind +resize+)
               (resize (gethash "width" value)
                       (gethash "height" value))
               (send-event :resize))
              (t
               (error "unexpected kind: ~D" kind))))
    (error (e)
      (dbg (format nil "~%******ERROR******:~%~A~%" e)))))

(setf lem::*implementation* :jsonrpc)
(setf lem::*native-scroll-support* t)
