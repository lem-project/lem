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
  #+(or)
  (dbg (format nil "~A:~A"
               method
               (with-output-to-string (*standard-output*)
                 (yason:encode argument))))
  (let ((jsonrpc/connection:*connection*
          (jsonrpc/transport/interface:transport-connection
           (jsonrpc/class:jsonrpc-transport *server*))))
    (jsonrpc:notify *server* method argument)))

(defun resize (params)
  (let ((width (gethash "width" params))
        (height (gethash "height" params)))
    (setf *display-width* width)
    (setf *display-height* height)))

(defmethod lem::interface-invoke ((implementation (eql :jsonrpc)) function)
  (let ((ready nil))
    (setf *main-thread* (bt:current-thread))
    (setf *editor-thread*
          (funcall function
                   (lambda ()
                     (loop :until ready))))
    (setf *server* (jsonrpc:make-server))
    (jsonrpc:expose *server* "ready"
                    (lambda (params)
                      (resize params)
                      (setf ready t)
                      (params "width" *display-width*
                              "height" *display-height*)))
    (jsonrpc:expose *server* "input" 'input-callback)
    ;(dbg "server-listen")
    (jsonrpc:server-listen *server* :mode :stdio)))

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
  (put-line-text view x (+ y (view-height view)) string attribute))

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

(defun vline (view)
  (loop :with attr := (ensure-attribute 'modeline nil)
        :for y :from 0 :repeat (view-height view)
        :do (put-line-text view -1 y " " attr)))

(defmethod lem::interface-redraw-view-after ((implementation (eql :jsonrpc)) view focus-window-p)
  (when (and (view-use-modeline view)
             (< 0 (view-x view)))
    (vline view))
  (when focus-window-p
    (lem::interface-move-cursor implementation
                                view
                                lem::*cursor-x*
                                lem::*cursor-y*)))

(defmethod lem::interface-update-display ((implementation (eql :jsonrpc)))
  (notify "update-display" nil))

(defmethod lem::interface-scroll ((implementation (eql :jsonrpc)) view n)
  (notify "scroll"
          (params "x" (view-x view)
                  "y" (view-y view)
                  "width" (view-width view)
                  "height" (view-height view)
                  "n" n)))


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
        ;(dbg (format nil "key: ~A~%" key))
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
               (resize value)
               (send-event :resize))
              (t
               (error "unexpected kind: ~D" kind))))
    (error (e)
      (dbg (format nil "~%******ERROR******:~%~A~%" e)))))

(setf lem::*implementation* :jsonrpc)
(setf lem::*native-scroll-support* t)
