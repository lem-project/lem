(in-package :lem-lispworks)

(defvar *non-focus-interface*)
(defvar *menu-items*)

(defmethod lem-if:display-popup-menu ((implementation capi-impl) items
                                      &key action-callback print-function focus-attribute non-focus-attribute)
  (declare (ignore focus-attribute non-focus-attribute))
  (setf *menu-items* items)
  (let ((window-pane (lem:window-view (lem:current-window))))
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (setf *non-focus-interface*
            (capi:prompt-with-list-non-focus
             items
             :owner window-pane
             :x (* (lem:point-column (lem:current-point)) char-width)
             :y (* (1+ (lem:window-cursor-y (lem:current-window))) char-height)
             :print-function (or print-function #'princ-to-string)
             :action-callback (lambda (item interface)
                                (declare (ignore interface))
                                (mp:process-interrupt *editor-thread* action-callback item))
             :list-updater (lambda () *menu-items*))))))

(defmethod lem-if:popup-menu-update ((implementation capi-impl) items)
  (setf *menu-items* items)
  (capi:non-focus-update *non-focus-interface*))

(defmethod lem-if:popup-menu-quit ((implementation capi-impl))
  (capi:non-focus-terminate *non-focus-interface*))

(defmethod lem-if:popup-menu-down ((implementation capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :down)))

(defmethod lem-if:popup-menu-up ((implementation capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :up)))

(defmethod lem-if:popup-menu-first ((implementation capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :prior)))

(defmethod lem-if:popup-menu-last ((implementation capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :next)))

(defmethod lem-if:popup-menu-select ((implementation capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec #\return)))

(defmethod lem-if:display-popup-message ((implementation capi-impl) text timeout)
  (let ((window-pane (lem:window-view (lem:current-window))))
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (capi:display-non-focus-message text
                                      :timeout timeout
                                      :owner window-pane
                                      :x (* (lem:point-column (lem:current-point)) char-width)
                                      :y (* (lem:window-cursor-y (lem-base:current-point)) char-height)))))
