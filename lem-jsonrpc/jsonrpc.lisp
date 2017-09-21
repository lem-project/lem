(defpackage :lem-jsonrpc
  (:use :cl :lem))
(in-package :lem-jsonrpc)

(defvar *view-id-counter* 0)
(defvar *display-width* 80)
(defvar *display-height* 24)

(defvar *editor-thread*)
(defvar *server*)

(defstruct view
  (id (incf *view-id-counter*))
  x
  y
  width
  height
  use-modeline)

(defmethod yason:encode ((view view) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "id" (view-id view)))))

(defun view-params (view &rest args)
  (alexandria:plist-hash-table (list* "view" view args)))

(defun notify (method argument)
  (jsonrpc:notify *server* method argument))

(defmethod lem::interface-invoke ((implementation (eql :jsonrpc)) function)
  (setf *server* (jsonrpc:make-server))
  (jsonrpc:server-listen *server* :mode :stdio)
  (jsonrpc:expose *server* "input" 'input-callback)
  (setf *editor-thread* (funcall function)))

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
  (let ((view (make-view :x x :y y :width width :height height :use-modeline use-modeline)))
    (notify "make-view" view)
    view))

(defmethod lem::interface-delete-view ((implementation (eql :jsonrpc)) view)
  (notify "delete-view" (view-params view)))

(defmethod lem::interface-clear ((implementation (eql :jsonrpc)) view)
  (notify "clear" (view-params view)))

(defmethod lem::interface-set-view-size ((implementation (eql :jsonrpc)) view width height)
  (notify "set-view-size" (view-params view "width" width "height" height)))

(defmethod lem::interface-set-view-pos ((implementation (eql :jsonrpc)) view x y)
  (notify "set-view-pos" (view-params view "x" x "y" y)))

(defmethod lem::interface-print ((implementation (eql :jsonrpc)) view x y string attribute)
  (notify "put-line-text" (view-params view "row" y "text" string "attribute" attribute)))

(defmethod lem::interface-print-modeline
    ((implementation (eql :jsonrpc)) view x y string attribute)
  (notify "modeline-put" (view-params view "x" x "y" y "text" string "attribute" attribute)))

(defmethod lem::interface-clear-eol ((implementation (eql :jsonrpc)) view x y)
  (notify "clear-eol" (view-params view "x" x "y" y)))

(defmethod lem::interface-clear-eob ((implementation (eql :jsonrpc)) view x y)
  (notify "clear-eob" (view-params view "x" x "y" y)))

(defmethod lem::interface-move-cursor ((implementation (eql :jsonrpc)) view x y)
  (notify "move-cursor" (view-params view "x" x "y" y)))

;; (defmethod lem::interface-redraw-view-after ((implementation (eql :jsonrpc)) view focus-window-p)
;;   )

;; (defmethod lem::interface-update-display ((implementation (eql :jsonrpc)))
;;   )


(defconstant +abort+ 0)
(defconstant +key+ 1)

(defun input-callback (args)
  (let ((kind (gethash "kind" args))
        (value (gethash "value" args)))
    (ecase kind
      (#.+abort+
       (send-abort-event *editor-thread* nil))
      (#.+key+
       (send-event value)))))

(setf lem::*implementation* :jsonrpc)
