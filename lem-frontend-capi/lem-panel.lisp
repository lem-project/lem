(in-package :lem-capi)

(defclass lem-panel (capi:simple-layout)
  ((tab-layout
    :initarg :tab-layout
    :reader lem-panel-tab-layout)
   (editor-pane
    :initarg :editor-pane
    :reader lem-panel-editor-pane)))

(defmethod initialize-instance ((lem-panel lem-panel) &rest initargs)
  (let* ((editor-pane (make-instance 'editor-pane))
         (layout (make-instance 'capi:tab-layout
                                :description (list editor-pane)
                                :items (or (lem:buffer-list) (list nil))
                                :visible-child-function nil
                                :print-function (lambda (x) (if (lem:bufferp x) (lem:buffer-name x) ""))
                                :callback-type :data
                                :selection-callback (lambda (buffer)
                                                      (lem:send-event (lambda ()
                                                                        (lem:switch-to-buffer buffer nil)
                                                                        (lem:redraw-display)))))))

    (apply #'call-next-method lem-panel
           :description (list layout)
           :editor-pane editor-pane
           :tab-layout layout
           initargs)))

(defun update-tab-layout (lem-panel)
  (labels ((modified-buffer-list-p ()
             (block outer
               (map nil
                    (lambda (x y)
                      (unless (equal x y)
                        (return-from outer t)))
                    (capi:collection-items (lem-panel-tab-layout lem-panel))
                    (lem:buffer-list))))
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
