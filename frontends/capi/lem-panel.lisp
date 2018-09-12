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
                          :items (list (list "Main" window-panel))
                          :print-function #'first
                          :visible-child-function #'second
                          :callback-type :data
                          :selection-callback (lambda (item)
                                                (when-let (callback (third item))
                                                  (funcall callback)))))
         (layout (make-instance 'capi:row-layout :description (list tab-layout))))
    (apply #'call-next-method lem-panel
           :description (list layout)
           :tab-layout tab-layout
           :window-panel window-panel
           :enable-directory-view-p nil
           :directory-view directory-view
           :layout layout
           initargs)))

(defun find-tab-layout (lem-panel name &optional (function #'find))
  (funcall function name
           (capi:collection-items (lem-panel-tab-layout lem-panel))
           :key #'first :test #'equal))

(defun position-tab-layout (lem-panel name)
  (find-tab-layout lem-panel name #'position))

(defun add-tab-layout (lem-panel name layout &optional selection-callback)
  (let ((item (list name layout selection-callback)))
    (if-let (pos (position-tab-layout lem-panel name))
      (setf (elt (capi:collection-items (lem-panel-tab-layout lem-panel)) pos) item)
      (setf (capi:collection-items (lem-panel-tab-layout lem-panel))
            (concatenate 'list
                         (capi:collection-items (lem-panel-tab-layout lem-panel))
                         (list item))))))

(defun change-to-tab (lem-panel name)
  (with-apply-in-pane-process-wait-single (lem-panel)
    (when-let (pos (position-tab-layout lem-panel name))
      (setf (capi:choice-selection (lem-panel-tab-layout lem-panel)) pos)
      (capi:set-pane-focus (second (elt (capi:collection-items
                                         (lem-panel-tab-layout lem-panel))
                                        pos))))))

(defun current-tab-is-main (lem-panel)
  (zerop (capi:choice-selection (lem-panel-tab-layout lem-panel))))

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
