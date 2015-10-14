;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(define-class window () *current-window*
  win
  nlines
  ncols
  y
  x
  buffer
  disp-lines
  vtop-linum
  cur-linum
  cur-col
  max-col
  wrap-ylist
  redraw-flag
  delete-hook)

(defun window-p (x)
  (typep x window))

(defun make-window (buffer nlines ncols y x)
  (let ((window
         (make-instance 'window
                        :win (cl-charms/low-level:newwin nlines ncols y x)
                        :nlines nlines
                        :ncols ncols
                        :y y
                        :x x
                        :buffer buffer
                        :disp-lines (make-array (1- nlines) :initial-element nil)
                        :vtop-linum 1
                        :cur-linum 1
                        :cur-col 0
                        :max-col 0
                        :wrap-ylist nil)))
    (cl-charms/low-level:keypad (window-win window) 1)
    window))

(defstruct (window-node (:constructor make-window-node-internal))
  split-type
  car
  cdr)

(defun make-window-node (split-type car cdr)
  (assert (member split-type '(:hsplit :vsplit)))
  (make-window-node-internal :split-type split-type
                             :car car
                             :cdr cdr))

(defun window-tree-leaf-p (window)
  (typep window 'window))

(defun window-tree-map (fn tree)
  (labels ((f (tree)
              (cond ((window-tree-leaf-p tree)
                     (funcall fn tree))
                    (t
                     (f (window-node-car tree))
                     (f (window-node-cdr tree))))))
    (f tree)
    nil))

(defmacro do-window-tree ((var tree) &body body)
  `(window-tree-map #'(lambda (,var) ,@body) ,tree))

(defun window-tree-flatten (tree)
  (let ((windows))
    (window-tree-map #'(lambda (win)
                         (push win windows))
                     tree)
    (nreverse windows)))

(defun window-tree-find (tree window)
  (window-tree-map #'(lambda (win)
                       (when (eq win window)
                         (return-from window-tree-find win)))
                   tree))

(defun window-tree-parent (tree node)
  (cond ((window-tree-leaf-p tree) nil)
        ((eq node (window-node-car tree))
         (values tree
                 #'(lambda ()
                     (window-node-car tree))
                 #'(lambda (new-value)
                     (setf (window-node-car tree) new-value))
                 #'(lambda ()
                     (window-node-cdr tree))
                 #'(lambda (new-value)
                     (setf (window-node-cdr tree) new-value))))
        ((eq node (window-node-cdr tree))
         (values tree
                 #'(lambda ()
                     (window-node-cdr tree))
                 #'(lambda (new-value)
                     (setf (window-node-cdr tree) new-value))
                 #'(lambda ()
                     (window-node-car tree))
                 #'(lambda (new-value)
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
