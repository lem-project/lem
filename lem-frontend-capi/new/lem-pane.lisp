(in-package :lem-lispworks)

(defclass lem-pane (capi:column-layout)
  ((initialized
    :initform nil
    :accessor lem-pane-initialized)
   (minibuffer
    :accessor lem-pane-minibuffer)
   (modified-p
    :initform nil
    :accessor lem-pane-modified-p)
   (resizing
    :initform nil
    :accessor lem-pane-resizing)))

(defmethod initialize-instance :after ((lem-pane lem-pane) &rest initargs)
  (declare (ignore initargs))
  (let ((minibuffer-pane (make-instance 'window-pane
                                        :visible-max-height '(:character 1)
                                        :lem-pane lem-pane)))
    (setf (lem-pane-minibuffer lem-pane) minibuffer-pane)
    (setf (capi:layout-description lem-pane)
          (list (make-instance 'capi:column-layout
                               :description
                               (list nil
                                     minibuffer-pane))))))

(defun set-first-window (lem-pane window-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (setf (lem-pane-initialized lem-pane) t)
    (setf (capi:layout-description (first (capi:layout-description lem-pane)))
          (list window-pane (lem-pane-minibuffer lem-pane)))))

(defun map-window-panes (lem-pane function)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (capi:map-pane-descendant-children
     lem-pane
     (lambda (pane)
       (when (typep pane 'window-pane)
         (funcall function pane))))))

(defun all-window-panes (lem-pane)
  (let ((window-panes '()))
    (map-window-panes lem-pane
                      (lambda (window-pane)
                        (push window-pane window-panes)))
    window-panes))

(defun lem-pane-width (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (let ((window-pane (lem-pane-minibuffer lem-pane)))
      (round (capi:simple-pane-visible-width lem-pane)
             (window-pane-char-width window-pane)))))

(defun lem-pane-height (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (let ((window-pane (lem-pane-minibuffer lem-pane)))
      (round (capi:simple-pane-visible-height lem-pane)
             (window-pane-char-height window-pane)))))

(defun split-window (lem-pane current-window-pane new-window-pane layout-class-name)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (block outer
      (let ((*window-is-modifying-p* t))
        (capi:map-pane-descendant-children
         lem-pane
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

(defun split-horizontally (lem-pane current-window-pane new-window-pane)
  (split-window lem-pane current-window-pane new-window-pane 'capi:row-layout))

(defun split-vertically (lem-pane current-window-pane new-window-pane)
  (split-window lem-pane current-window-pane new-window-pane 'capi:column-layout))

(defun delete-window-from-lem-pane (lem-pane window-pane)
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
    (with-apply-in-pane-process-wait-single (lem-pane)
      (let ((*window-is-modifying-p* t))
        (f lem-pane)))))

(defun update-window-ratios (lem-pane)
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
                    (let ((window (if (eq pane (lem-pane-minibuffer lem-pane))
                                      (lem::minibuffer-window)
                                      (window-pane-window pane))))
                      (list (lem:window-width window)
                            (lem:window-height window))))
                   (t
                    (list nil nil)))))
    (with-apply-in-pane-process-wait-single (lem-pane)
      (let ((*window-is-modifying-p* t))
        (f lem-pane)))))

(defun update-window-size (lem-pane)
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
                    (if (eq pane (lem-pane-minibuffer lem-pane))
                        (multiple-value-bind (w h)
                            (window-pane-size pane)
                          (values x y w h))
                        (let ((window (window-pane-window pane)))
                          (multiple-value-bind (w h)
                              (window-pane-size pane)
                            (lem::window-set-pos window x y)
                            (lem::window-set-size window w h)
                            (values x y w h))))))))
    (with-apply-in-pane-process-wait-single (lem-pane)
      (f lem-pane 0 0)
      (lem:send-event :resize)
      (setf (lem-pane-resizing lem-pane) nil))))

(defun lem-pane-resize-callback (lem-pane)
  (cond (*window-is-modifying-p*)
        (t
         (unless (lem-pane-resizing lem-pane)
           (setf (lem-pane-resizing lem-pane) t)
           (mp:schedule-timer-relative (mp:make-timer #'update-window-size lem-pane) 0.1)))))
