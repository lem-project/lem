(in-package :lem-lispworks)

(defclass lem-panel (capi:simple-layout)
  ((tab-layout
    :initarg :tab-layout
    :reader lem-panel-tab-layout)
   (window-panel
    :initarg :window-panel
    :reader lem-panel-window-panel)))

(defmethod initialize-instance :around ((lem-panel lem-panel) &rest initargs)
  (let* ((window-panel (make-instance 'window-panel))
         (tab-layout (make-instance 'capi:tab-layout
                                    :description (list window-panel)
                                    :items (or (lem:buffer-list) (list nil))
                                    :visible-child-function nil
                                    :print-function (lambda (x) (if (lem:bufferp x) (lem:buffer-name x) ""))
                                    :callback-type :data
                                    :selection-callback (lambda (buffer)
                                                          (lem:send-event (lambda ()
                                                                            (lem:switch-to-buffer buffer nil)
                                                                            (lem:redraw-display))))))
         (layout (make-instance 'capi:row-layout :description (list tab-layout))))
    (apply #'call-next-method lem-panel
           :description (list layout)
           :tab-layout tab-layout
           :window-panel window-panel
           initargs)))

(defun update-tab-layout (lem-panel)
  (labels ((modified-buffer-list-p ()
             (block outer
               (or (/= (length (capi:collection-items (lem-panel-tab-layout lem-panel)))
                       (length (lem:buffer-list)))
                   (map nil
                        (lambda (x y)
                          (unless (equal x y)
                            (return-from outer t)))
                        (capi:collection-items (lem-panel-tab-layout lem-panel))
                        (lem:buffer-list)))))
           (modified-current-buffer-p ()
             (/= (capi:choice-selection (lem-panel-tab-layout lem-panel))
                 (position (lem:current-buffer) (lem:buffer-list)))))
    (unless (lem:minibuffer-window-active-p)
      (when (modified-buffer-list-p)
        (setf (capi:collection-items (lem-panel-tab-layout lem-panel))
              (lem:buffer-list)))
      (when (modified-current-buffer-p)
        (setf (capi:choice-selection (lem-panel-tab-layout lem-panel))
              (position (lem:current-buffer) (lem:buffer-list)))))))
