(in-package :lem-lispworks)

(defclass capi-impl (lem:implementation)
  ()
  (:default-initargs
   :support-floating-window nil
   :native-scroll-support nil
   :redraw-after-modifying-floating-window nil))

(setf lem:*implementation* (make-instance 'capi-impl))

(defvar *lem-pane*)
(defvar *editor-thread*)

(defmethod lem-if:invoke ((implementation capi-impl) function)
  (with-error-handler ()
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
                   (lambda ())
                   (lambda (report)
                     (when report
                       (with-open-file (out "~/ERROR"
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                         (write-string report out)))
                     (capi:quit-interface *lem-pane*))))))

(defmethod lem-if:set-first-view ((implementation capi-impl) view)
  (with-error-handler ()
    (set-first-window *lem-pane* view)))

(defmethod lem-if:display-background-mode ((implementation capi-impl))
  ;; TODO
  :light)

(defmethod lem-if:update-foreground ((implementation capi-impl) color-name)
  ;; TODO
  )

(defmethod lem-if:update-background ((implementation capi-impl) color-name)
  ;; TODO
  )

(defmethod lem-if:display-width ((implementation capi-impl))
  (with-error-handler ()
    (lem-pane-width *lem-pane*)))

(defmethod lem-if:display-height ((implementation capi-impl))
  (with-error-handler ()
    (lem-pane-height *lem-pane*)))

(defmethod lem-if:make-view ((implementation capi-impl) window x y width height use-modeline)
  (with-error-handler ()
    (if (lem:minibuffer-window-p window)
        (lem-pane-minibuffer *lem-pane*)
        (make-instance 'window-pane
                       :window window
                       :visible-width `(:character ,width)
                       :visible-height `(:character ,height)))))

(defmethod lem-if:delete-view ((implementation capi-impl) view)
  (with-error-handler ()
    (delete-window *lem-pane* view)))

(defmethod lem-if:clear ((implementation capi-impl) view)
  (with-error-handler ()
    (clear view)))

(defmethod lem-if:set-view-size ((implementation capi-impl) view width height)
  (setf (lem-pane-modified-p *lem-pane*) t))

(defmethod lem-if:set-view-pos ((implementation capi-impl) view x y)
  (setf (lem-pane-modified-p *lem-pane*) t))

(defmethod lem-if:print ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (draw-string view string x y (lem:ensure-attribute attribute nil))))

(defmethod lem-if:print-modeline ((implementation capi-impl) view x y string attribute)
  (with-error-handler ()
    (draw-string-in-modeline view string x y (lem:ensure-attribute attribute nil))))

(defmethod lem-if:clear-eol ((implementation capi-impl) view x y)
  (with-error-handler ()
    (clear-eol view x y)))

(defmethod lem-if:clear-eob ((implementation capi-impl) view x y)
  (with-error-handler ()
    (clear-eob view x y)))

;(defmethod lem-if:redraw-window ((implementation capi-impl) window force)
;  )

(defmethod lem-if:redraw-view-after ((implementation capi-impl) view focus-window-p)
  )

(defmethod lem-if:update-display ((implementation capi-impl))
  (with-error-handler ()
    (with-apply-in-pane-process-wait-single (*lem-pane*)
      (when (lem-pane-modified-p *lem-pane*)
        (setf (lem-pane-modified-p *lem-pane*) nil)
        (update-window-ratios *lem-pane*))
      (map-window-panes *lem-pane* #'update-window))))

;(defmethod lem-if:scroll ((implementation capi-impl) view n)
;  )

(defmethod lem-if:split-window-horizontally ((implementation capi-impl) view new-view)
  (with-error-handler ()
    (setf (lem-pane-modified-p *lem-pane*) t)
    (split-horizontally *lem-pane* view new-view)))

(defmethod lem-if:split-window-vertically ((implementation capi-impl) view new-view)
  (with-error-handler ()
    (setf (lem-pane-modified-p *lem-pane*) t)
    (split-vertically *lem-pane* view new-view)))
