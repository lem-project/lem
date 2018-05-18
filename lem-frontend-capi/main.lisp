(defpackage :lem-capi
  (:use
   :cl
   :lem
   :lem-capi.util
   :lem-capi.lem-pane))
(in-package :lem-capi)

(defclass capi-impl (lem::implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(setf *implementation* (make-instance 'capi-impl))

(defstruct view window x y width height)

(defvar *lem-pane*)
(defvar *editor-thread*)

(defun key-press (self x y gesture-spec)
  (declare (ignore self x y))
  gesture-spec)

(defmethod lem::interface-invoke ((implementation capi-impl) function)
  (with-error-handler ()
    (dbg "interface-invoke")
    (setf *lem-pane* (make-instance 'lem-pane))
    (capi:display
     (make-instance 'capi:interface
                    :auto-menus nil
                    :best-width 800
                    :best-height 600
                    :layout (make-instance 'capi:column-layout
                                           :description (list *lem-pane*))))
    (setf *editor-thread*
          (funcall function
                   nil
                   (lambda (report)
                     (declare (ignore report))
                     (capi:quit-interface *lem-pane*))))))

(defmethod lem::interface-display-background-mode ((implementation capi-impl))
  (with-error-handler ()
    (dbg "display-background-mode")
    :light))

(defmethod lem::interface-update-foreground ((implementation capi-impl) color-name)
  (change-foreground *lem-pane* color-name))

(defmethod lem::interface-update-background ((implementation capi-impl) color-name)
  (change-background *lem-pane* color-name))

(defmethod lem::interface-display-width ((implementation capi-impl))
  (with-error-handler ()
    (dbg "display-width")
    (lem-pane-width *lem-pane*)))

(defmethod lem::interface-display-height ((implementation capi-impl))
  (with-error-handler ()
    (dbg "display-height")
    (lem-pane-height *lem-pane*)))

(defmethod lem::interface-make-view ((implementation capi-impl) window x y width height use-modeline)
  (with-error-handler ()
    (dbg (list "make-view" window x y width height use-modeline))
    (make-view :window window :x x :y y :width width :height height)))

(defmethod lem::interface-delete-view ((implementation capi-impl) view)
  (with-error-handler ()
    (dbg (list "delete-view" view))
    (values)))

(defmethod lem::interface-clear ((implementation capi-impl) view)
  (with-error-handler ()
    (dbg (list "clear" view))
    (draw-rectangle *lem-pane*
                    (view-x view)
                    (view-y view)
                    (1- (view-width view))
                    (view-height view)
                    (capi:simple-pane-background *lem-pane*))))

(defmethod lem::interface-set-view-size ((implementation capi-impl) view width height)
  (with-error-handler ()
    (dbg (list "set-view-size" view width height))
    (setf (view-width view) width)
    (setf (view-height view) height)))

(defmethod lem::interface-set-view-pos ((implementation capi-impl) view x y)
  (with-error-handler ()
    (dbg (list "set-view-pos" view x y))
    (setf (view-x view) x)
    (setf (view-y view) y)))

(defmethod lem::interface-print ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (dbg (list "print" view x y string attribute))
    (draw-text *lem-pane*
               string
               (+ (view-x view) x)
               (+ (view-y view) y)
               (ensure-attribute attribute nil))))

(defmethod lem::interface-print-modeline ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (dbg (list "print-modeline" view x y string attribute))
    (draw-text *lem-pane*
               string
               (+ (view-x view) x)
               (+ (view-y view) (view-height view) y)
               (ensure-attribute attribute nil))))

(defmethod lem::interface-clear-eol ((implementation capi-impl) view x y)
  (with-error-handler ()
    (dbg (list "clear-eol" view x y))
    (draw-rectangle *lem-pane*
                    (+ (view-x view) x)
                    (+ (view-y view) y)
                    (- (view-width view) x)
                    1
                    (capi:simple-pane-background *lem-pane*))))

(defmethod lem::interface-clear-eob ((implementation capi-impl) view x y)
  (with-error-handler ()
    (dbg (list "clear-eob" view x y))
    (when (plusp x)
      (lem::interface-clear-eol implementation view x y)
      (incf y))
    (draw-rectangle *lem-pane*
                    (view-x view)
                    (+ (view-y view) y)
                    (view-width view)
                    (- (view-height view) y)
                    (capi:simple-pane-background *lem-pane*))))

(defmethod lem::interface-move-cursor ((implementation capi-impl) view x y)
  (dbg (list "move-cursor" view x y)))

(defmethod lem::interface-redraw-view-after ((implementation capi-impl) view focus-window-p)
  (with-error-handler ()
    (dbg (list "redraw-view-after" view focus-window-p))
    (when (and (not (floating-window-p (view-window view)))
               (< 0 (view-x view)))
      (draw-rectangle *lem-pane*
                      (view-x view)
                      (view-y view)
                      0.1
                      (1+ (view-height view))
                      :black))))

(defmethod lem::interface-update-display ((implementation capi-impl))
  (dbg "update-display")
  (update-display *lem-pane*))

;(defmethod lem::interface-scroll ((implementation capi-impl) view n)
;  )
(pushnew :lem-capi *features*)

(setf lem::*window-left-margin* 0)
