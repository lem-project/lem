(in-package :lem-lispworks)

(defclass window-panel (capi:column-layout)
  ((initialized
    :initform nil
    :accessor window-panel-initialized)
   (minibuffer
    :accessor window-panel-minibuffer)
   (modified-p
    :initform nil
    :accessor window-panel-modified-p)
   (resizing
    :initform nil
    :accessor window-panel-resizing)))

(defmethod initialize-instance :after ((window-panel window-panel) &rest initargs)
  (declare (ignore initargs))
  (let ((minibuffer-pane (make-instance 'window-pane
                                        :visible-max-height '(:character 1)
                                        :window-panel window-panel)))
    (setf (window-panel-minibuffer window-panel) minibuffer-pane)
    (setf (capi:layout-description window-panel)
          (list (make-instance 'capi:column-layout
                               :description
                               (list nil
                                     minibuffer-pane))))))

(defun set-first-window (window-panel window-pane)
  (with-apply-in-pane-process-wait-single (window-panel)
    (setf (window-panel-initialized window-panel) t)
    (setf (capi:layout-description (first (capi:layout-description window-panel)))
          (list window-pane (window-panel-minibuffer window-panel)))))

(defun map-window-panes (window-panel function)
  (with-apply-in-pane-process-wait-single (window-panel)
    (capi:map-pane-descendant-children
     window-panel
     (lambda (pane)
       (when (typep pane 'window-pane)
         (funcall function pane))))))

(defun all-window-panes (window-panel)
  (let ((window-panes '()))
    (map-window-panes window-panel
                      (lambda (window-pane)
                        (push window-pane window-panes)))
    window-panes))

(defun window-panel-width (window-panel)
  (with-apply-in-pane-process-wait-single (window-panel)
    (let ((window-pane (window-panel-minibuffer window-panel)))
      (round (capi:simple-pane-visible-width window-panel)
             (window-pane-char-width window-pane)))))

(defun window-panel-height (window-panel)
  (with-apply-in-pane-process-wait-single (window-panel)
    (let ((window-pane (window-panel-minibuffer window-panel)))
      (round (capi:simple-pane-visible-height window-panel)
             (window-pane-char-height window-pane)))))

(defun split-window (window-panel current-window-pane new-window-pane layout-class-name)
  (with-apply-in-pane-process-wait-single (window-panel)
    (block outer
      (let ((*window-is-modifying-p* t))
        (capi:map-pane-descendant-children
         window-panel
         (lambda (pane)
           (when (or (typep pane 'capi:column-layout)
                     (typep pane 'capi:row-layout))
             (when-let (pos (position current-window-pane (capi:layout-description pane)))
               (setf (capi:layout-description pane)
                     (nconc (subseq (capi:layout-description pane) 0 pos)
                            (list (make-instance layout-class-name
                                                 :description (list current-window-pane
                                                                    :divider
                                                                    new-window-pane)))
                            (subseq (capi:layout-description pane) (1+ pos))))
               (return-from outer)))))))))

(defun split-horizontally (window-panel current-window-pane new-window-pane)
  (split-window window-panel current-window-pane new-window-pane 'capi:row-layout))

(defun split-vertically (window-panel current-window-pane new-window-pane)
  (split-window window-panel current-window-pane new-window-pane 'capi:column-layout))

(defun window-panel-delete-window (window-panel window-pane)
  (labels ((f (pane)
             (cond ((typep pane 'capi:layout)
                    (let ((pos (position window-pane (capi:layout-description pane))))
                      (setf (capi:layout-ratios pane) nil)
                      (setf (capi:layout-description pane)
                            (if pos
                                (nconc (delete :divider (subseq (capi:layout-description pane) 0 pos)
                                               :count 1 :from-end t)
                                       (delete :divider (subseq (capi:layout-description pane) (1+ pos))
                                               :count 1))
                                (delete nil
                                        (map 'list #'f (capi:layout-description pane)))))
                      (cond ((null (capi:layout-description pane))
                             nil)
                            ((null (rest (capi:layout-description pane)))
                             (first (capi:layout-description pane)))
                            (t
                             pane))))
                   (t
                    pane))))
    (with-apply-in-pane-process-wait-single (window-panel)
      (let ((*window-is-modifying-p* t))
        (f window-panel)))))

(defun update-window-ratios (window-panel)
  (labels ((sum (list)
             (loop :for n :in list
                   :sum (or n 0) :into sum
                   :finally (return (if (zerop sum) nil sum))))
           (f (pane)
             (cond ((typep pane 'capi:row-layout)
                    (let* ((width-height-list (mapcar #'f (capi:layout-description pane)))
                           (ratios (mapcar #'first width-height-list)))
                      (setf (capi:layout-ratios pane) ratios)
                      (list (sum ratios)
                            (second (first width-height-list)))))
                   ((typep pane 'capi:column-layout)
                    (let* ((width-height-list (mapcar #'f (capi:layout-description pane)))
                           (ratios (mapcar #'second width-height-list)))
                      (setf (capi:layout-ratios pane) ratios)
                      (list (first (first width-height-list))
                            (sum ratios))))
                   ((typep pane 'window-pane)
                    (let ((window (if (eq pane (window-panel-minibuffer window-panel))
                                      (lem::minibuffer-window)
                                      (window-pane-window pane))))
                      (list (lem:window-width window)
                            (lem:window-height window))))
                   (t
                    (list nil nil)))))
    (with-apply-in-pane-process-wait-single (window-panel)
      (let ((*window-is-modifying-p* t))
        (f window-panel)))))

(defun update-window-size (window-panel)
  (labels ((f (pane x y)
             (cond ((typep pane 'capi:row-layout)
                    (let ((w 0)
                          (h nil))
                      (dolist (child (capi:layout-description pane))
                        (unless (eq child :divider)
                          (multiple-value-bind (child-x child-y child-w child-h) (f child x y)
                            (declare (ignore child-x child-y))
                            (incf x child-w)
                            (incf w child-w)
                            (setf h child-h))))
                      (values x y w h)))
                   ((typep pane 'capi:column-layout)
                    (let ((w nil)
                          (h 0))
                      (dolist (child (capi:layout-description pane))
                        (unless (eq child :divider)
                          (multiple-value-bind (child-x child-y child-w child-h) (f child x y)
                            (declare (ignore child-x child-y))
                            (incf y child-h)
                            (incf h child-h)
                            (setf w child-w))))
                      (values x y w h)))
                   ((typep pane 'window-pane)
                    (if (eq pane (window-panel-minibuffer window-panel))
                        (multiple-value-bind (w h)
                            (window-pane-size pane)
                          (values x y w h))
                        (let ((window (window-pane-window pane)))
                          (multiple-value-bind (w h)
                              (window-pane-size pane)
                            (lem::window-set-pos window x y)
                            (lem::window-set-size window w h)
                            (values x y w h))))))))
    (with-apply-in-pane-process-wait-single (window-panel)
      (f window-panel 0 0)
      (lem:send-event :resize)
      (setf (window-panel-resizing window-panel) nil))))

(defun window-panel-resize-callback (window-panel)
  (cond (*window-is-modifying-p*)
        (t
         (unless (window-panel-resizing window-panel)
           (setf (window-panel-resizing window-panel) t)
           (mp:schedule-timer-relative (mp:make-timer #'update-window-size window-panel) 0.1)))))
