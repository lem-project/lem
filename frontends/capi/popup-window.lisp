(in-package :lem-capi)

(defvar *non-focus-interface* nil)
(defvar *menu-items* nil)

(defmethod lem-if:display-popup-menu ((implementation capi-impl) items
                                      &key action-callback print-spec focus-attribute non-focus-attribute)
  (declare (ignore focus-attribute non-focus-attribute))
  (setf *menu-items* items)
  (let ((window-pane (lem:window-view (lem:current-window))))
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (setf *non-focus-interface*
            (capi:prompt-with-list-non-focus
             items
             :owner window-pane
             :x (round (* (lem:point-column (lem:current-point)) char-width))
             :y (round (* (1+ (lem:window-cursor-y (lem:current-window))) char-height))
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
  (clear-popup-message)
  (let ((window-pane (lem:window-view (lem:current-window))))
    (multiple-value-bind (char-width char-height)
        (window-pane-char-size window-pane)
      (multiple-value-bind (buffer width height)
          (lem.popup-window::make-popup-buffer text)
        (let ((lw-buffer (editor:make-buffer "temp" :temporary t)))
          (editor:insert-string (editor:buffer-point lw-buffer)
                                (lem:points-to-string (lem:buffer-start-point buffer)
                                                      (lem:buffer-end-point buffer)))
          (editor:buffer-start (editor:buffer-point lw-buffer))
          (with-apply-in-pane-process-wait-single (*lem-panel*)
            (setf *non-focus-interface*
                  (capi::display-non-focus-editor-pane
                   :x (round (* (lem:point-column (lem:current-point)) char-width))
                   :y (round (* (1+ (lem:window-cursor-y (lem:current-window))) char-height))
                   :owner window-pane
                   :width (* width char-width)
                   :height (* height char-height)
                   :echo-area-p nil
                   :buffer-name lw-buffer
                   :editor-font (window-pane-normal-font
                                 (first (all-window-panes (lem-panel-window-panel *lem-panel*)))))))
          (when timeout
            (check-type timeout (integer 0 *))
            (lem:start-timer (* timeout 1000) nil 'clear-popup-message))
          *non-focus-interface*)))))

(defmethod lem-if:delete-popup-message ((implementation capi-impl) popup-message)
  (capi:non-focus-terminate popup-message))

(defun clear-popup-message ()
  (when *non-focus-interface*
    (capi:non-focus-terminate *non-focus-interface*)))
