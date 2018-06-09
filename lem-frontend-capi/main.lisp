(in-package :lem-capi)

(defclass capi-impl (lem:implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
   :redraw-after-modifying-floating-window t))

(setf lem:*implementation* (make-instance 'capi-impl))
(setf lem::*window-left-margin* 0)

(defstruct view window x y width height)

(defmethod lem-if:invoke ((implementation capi-impl) function)
  (with-error-handler ()
    (setf *lem-panel* (make-instance 'lem-panel))
    (capi:display
     (make-instance 'capi:interface
                    :auto-menus nil
                    :best-width 800
                    :best-height 600
                    :layout *lem-panel*))
    (setf *lem-process*
          (funcall function
                   (lambda ())
                   (lambda (report)
                     (declare (ignore report))
                     (capi:quit-interface *lem-panel*))))))

(defmethod lem-if:display-background-mode ((implementation capi-impl))
  (with-error-handler ()
    :light))

(defmethod lem-if:update-foreground ((implementation capi-impl) color-name)
  (change-foreground (lem-panel-editor-pane *lem-panel*) color-name))

(defmethod lem-if:update-background ((implementation capi-impl) color-name)
  (change-background (lem-panel-editor-pane *lem-panel*) color-name))

(defmethod lem-if:display-width ((implementation capi-impl))
  (with-error-handler ()
    (editor-pane-width (lem-panel-editor-pane *lem-panel*))))

(defmethod lem-if:display-height ((implementation capi-impl))
  (with-error-handler ()
    (editor-pane-height (lem-panel-editor-pane *lem-panel*))))

(defmethod lem-if:make-view ((implementation capi-impl) window x y width height use-modeline)
  (with-error-handler ()
    (make-view :window window :x x :y y :width width :height height)))

(defmethod lem-if:delete-view ((implementation capi-impl) view)
  (with-error-handler ()
    (values)))

(defmethod lem-if:clear ((implementation capi-impl) view)
  (with-error-handler ()
    (draw-rectangle (lem-panel-editor-pane *lem-panel*)
                     (view-x view)
                     (view-y view)
                     (1- (view-width view))
                     (view-height view))))

(defmethod lem-if:set-view-size ((implementation capi-impl) view width height)
  (with-error-handler ()
    (setf (view-width view) width)
    (setf (view-height view) height)))

(defmethod lem-if:set-view-pos ((implementation capi-impl) view x y)
  (with-error-handler ()
    (setf (view-x view) x)
    (setf (view-y view) y)))

(defmethod lem-if:print ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (draw-text (lem-panel-editor-pane *lem-panel*)
               string
               (+ (view-x view) x)
               (+ (view-y view) y)
               (lem:ensure-attribute attribute nil))))

(defmethod lem-if:print-modeline ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (draw-text (lem-panel-editor-pane *lem-panel*)
               string
               (+ (view-x view) x)
               (+ (view-y view) (view-height view) y)
               (lem:ensure-attribute attribute nil))))

(defmethod lem-if:clear-eol ((implementation capi-impl) view x y)
  (with-error-handler ()
    (draw-rectangle (lem-panel-editor-pane *lem-panel*)
                    (+ (view-x view) x)
                    (+ (view-y view) y)
                    (- (view-width view) x)
                    1)))

(defmethod lem-if:clear-eob ((implementation capi-impl) view x y)
  (with-error-handler ()
    (when (plusp x)
      (lem-if:clear-eol implementation view x y)
      (incf y))
    (draw-rectangle (lem-panel-editor-pane *lem-panel*)
                     (view-x view)
                     (+ (view-y view) y)
                     (view-width view)
                     (- (view-height view) y))))

(defmethod lem-if:redraw-view-after ((implementation capi-impl) view focus-window-p)
  (with-error-handler ()
    (when (and (not (lem:floating-window-p (view-window view)))
               (< 0 (view-x view)))
      (draw-rectangle (lem-panel-editor-pane *lem-panel*)
                       (view-x view)
                       (view-y view)
                       0.1
                       (1+ (view-height view))
                       :black))))

(defmethod lem-if:update-display ((implementation capi-impl))
  (update-display (lem-panel-editor-pane *lem-panel*))
  (update-tab-layout *lem-panel*))

;(defmethod lem-if:scroll ((implementation capi-impl) view n)
;  )

(pushnew :lem-capi *features*)
