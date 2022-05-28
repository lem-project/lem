(in-package :lem)

(defun window-tree ()
  (frame-window-tree (current-frame)))

(defun (setf window-tree) (new-window-tree)
  (let ((frame (current-frame)))
    (setf (frame-window-tree frame) new-window-tree)))

(defstruct (window-node (:constructor %make-window-node))
  split-type
  car
  cdr)

(defun make-window-node (split-type car cdr)
  (assert (member split-type '(:hsplit :vsplit)))
  (%make-window-node :split-type split-type
                     :car car
                     :cdr cdr))

(defun window-tree-leaf-p (window)
  (windowp window))

(defun window-tree-map (tree fn)
  (labels ((f (tree)
             (cond ((window-tree-leaf-p tree)
                    (funcall fn tree))
                   (t
                    (f (window-node-car tree))
                    (f (window-node-cdr tree))))))
    (f tree)
    nil))

(defun window-tree-flatten (tree)
  (let ((windows))
    (window-tree-map tree
                     (lambda (win)
                       (push win windows)))
    (nreverse windows)))

(defun window-tree-find (tree window)
  (window-tree-map tree
                   (lambda (win)
                     (when (eq win window)
                       (return-from window-tree-find win)))))

(defun window-tree-find-if (tree test)
  (window-tree-map tree
                   (lambda (win)
                     (when (funcall test win)
                       (return-from window-tree-find-if win)))))

(defun window-tree-parent (tree node)
  (cond ((window-tree-leaf-p tree) nil)
        ((eq node (window-node-car tree))
         (values tree
                 (lambda ()
                   (window-node-car tree))
                 (lambda (new-value)
                   (setf (window-node-car tree) new-value))
                 (lambda ()
                   (window-node-cdr tree))
                 (lambda (new-value)
                   (setf (window-node-cdr tree) new-value))))
        ((eq node (window-node-cdr tree))
         (values tree
                 (lambda ()
                   (window-node-cdr tree))
                 (lambda (new-value)
                   (setf (window-node-cdr tree) new-value))
                 (lambda ()
                   (window-node-car tree))
                 (lambda (new-value)
                   (setf (window-node-car tree) new-value))))
        (t
         (multiple-value-bind (parent
                               getter
                               setter
                               another-getter
                               another-setter)
             (window-tree-parent (window-node-car tree) node)
           (if parent
               (values parent getter setter another-getter another-setter)
               (window-tree-parent (window-node-cdr tree) node))))))
