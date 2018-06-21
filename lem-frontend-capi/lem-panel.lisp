(in-package :lem-capi)

(defclass lem-panel (capi:simple-layout)
  ((enable-directory-view-p
    :initarg :enable-directory-view-p
    :accessor lem-panel-enable-directory-view-p)
   (tab-layout
    :initarg :tab-layout
    :reader lem-panel-tab-layout)
   (window-panel
    :initarg :window-panel
    :reader lem-panel-window-panel)
   (directory-view
    :initarg :directory-view
    :reader lem-panel-directory-view)
   (layout
    :initarg :layout
    :reader lem-panel-layout)))

(defmethod initialize-instance :around ((lem-panel lem-panel) &rest initargs)
  (let* ((window-panel
           (make-instance 'window-panel))
         (directory-view
           (make-instance 'directory-view
                          :callback (lambda (pathname)
                                      (when (uiop:file-pathname-p pathname)
                                        (lem:send-event (lambda ()
                                                          (lem:find-file pathname)
                                                          (lem:redraw-display)))))
                          :visible-max-width 200))
         (tab-layout
           (make-instance 'capi:tab-layout
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
           :enable-directory-view-p nil
           :directory-view directory-view
           :layout layout
           initargs)))

(defun enable-directory-view (lem-panel)
  (with-apply-in-pane-process-wait-single (lem-panel)
    (setf (lem-panel-enable-directory-view-p lem-panel) t)
    (setf (capi:layout-description (lem-panel-layout lem-panel))
          (list (lem-panel-directory-view lem-panel)
                (lem-panel-tab-layout lem-panel)))))

(defun disable-directory-view (lem-panel)
  (with-apply-in-pane-process-wait-single (lem-panel)
    (setf (lem-panel-enable-directory-view-p lem-panel) nil)
    (setf (capi:layout-description (lem-panel-layout lem-panel))
          (list (lem-panel-tab-layout lem-panel)))))

(defun toggle-directory-view (lem-panel)
  (if (lem-panel-enable-directory-view-p lem-panel)
      (disable-directory-view lem-panel)
      (enable-directory-view lem-panel)))

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
