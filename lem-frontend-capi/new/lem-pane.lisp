(in-package :lem-lispworks)

(defclass lem-pane (capi:column-layout)
  ((minibuffer
    :accessor lem-pane-minibuffer)
   (modified-p
    :initform nil
    :accessor lem-pane-modified-p)))

(defmethod initialize-instance :after ((lem-pane lem-pane) &rest initargs)
  (declare (ignore initargs))
  (let ((minibuffer-pane (make-instance 'window-pane
                                        :visible-max-height '(:character 1))))
    (setf (lem-pane-minibuffer lem-pane) minibuffer-pane)
    (setf (capi:layout-description lem-pane)
          (list (make-instance 'capi:column-layout
                               :description
                               (list nil
                                     minibuffer-pane))))))

(defmacro with-apply-in-pane-process-wait-single ((lem-pane) &body body)
  `(capi:apply-in-pane-process-wait-single ,lem-pane nil (lambda () ,@body)))

(defun set-first-window (lem-pane window-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (setf (capi:layout-description (first (capi:layout-description lem-pane)))
          (list window-pane (lem-pane-minibuffer lem-pane)))))

(defun all-window-panes (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (let ((window-panes '()))
      (capi:map-pane-descendant-children
       lem-pane
       (lambda (pane)
         (when (typep pane 'window-pane)
           (push pane window-panes))))
      window-panes)))

(defun lem-pane-width (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (let ((window-pane (lem-pane-minibuffer lem-pane)))
      (floor (capi:simple-pane-visible-width lem-pane)
             (window-pane-char-width window-pane)))))

(defun lem-pane-height (lem-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (let ((window-pane (lem-pane-minibuffer lem-pane)))
      (floor (capi:simple-pane-visible-height lem-pane)
             (window-pane-char-height window-pane)))))

(defun split-window (lem-pane current-window-pane new-window-pane layout-class-name)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (block outer
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
             (return-from outer))))))))

(defun split-horizontally (lem-pane current-window-pane new-window-pane)
  (split-window lem-pane current-window-pane new-window-pane 'capi:row-layout))

(defun split-vertically (lem-pane current-window-pane new-window-pane)
  (split-window lem-pane current-window-pane new-window-pane 'capi:column-layout))

(defun delete-window-1 (pane window-pane)
  (cond ((typep pane 'capi:layout)
         (let ((pos (position window-pane (capi:layout-description pane))))
           (setf (capi:layout-ratios pane) nil)
           (setf (capi:layout-description pane)
                 (if pos
                     (nconc (delete :divider (subseq (capi:layout-description pane) 0 pos) :count 1 :from-end t)
                            (delete :divider (subseq (capi:layout-description pane) (1+ pos)) :count 1))
                     (delete nil
                             (map 'list
                                  (lambda (c) (delete-window-1 c window-pane))
                                  (capi:layout-description pane)))))
           (cond ((null (capi:layout-description pane))
                  nil)
                 ((null (rest (capi:layout-description pane)))
                  (first (capi:layout-description pane)))
                 (t
                  pane))))
        (t
         pane)))

(defun delete-window (lem-pane window-pane)
  (with-apply-in-pane-process-wait-single (lem-pane)
    (delete-window-1 lem-pane window-pane)))

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
      (f lem-pane))))

(defun test ()
  (setq x (capi:contain (make-instance 'lem-pane)))
  (set-first-window x (make-instance 'window-pane))
  (split-horizontally x (first (all-window-panes x)) (make-instance 'window-pane))
  (split-vertically x (first (all-window-panes x)) (make-instance 'window-pane))
  (split-horizontally x (second (all-window-panes x)) (make-instance 'window-pane))
  (split-vertically x (fourth (all-window-panes x)) (make-instance 'window-pane)))

#|
 (setq x (capi:contain (make-instance 'lem-pane)))
 (set-first-window x (make-instance 'window-pane))
 (split-horizontally x (first (all-window-panes x)) (make-instance 'window-pane))
 (split-vertically x (first (all-window-panes x)) (make-instance 'window-pane))
 (split-horizontally x (second (all-window-panes x)) (make-instance 'window-pane))
 (split-vertically x (fourth (all-window-panes x)) (make-instance 'window-pane))
 (delete-window x (first (all-window-panes x)))
|#
