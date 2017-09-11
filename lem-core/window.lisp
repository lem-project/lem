(in-package :lem)

(export '(truncate-lines
          *window-sufficient-width*
          *scroll-recenter-p*
          *window-scroll-functions*
          *window-size-change-functions*
          *window-show-buffer-functions*
          find-window
          window-view-point
          windowp
          window-id
          window-x
          window-y
          window-width
          window-height
          window-buffer
          window-parameter
          window-redraw
          current-window
          window-list
          one-window-p
          deleted-window-p
          window-recenter
          window-scroll
          window-see
          split-window-vertically
          split-window-horizontally
          split-window-sensibly
          get-next-window
          delete-window
          get-buffer-windows
          other-buffer
          switch-to-buffer
          pop-to-buffer
          floating-windows
          balloon-message
          redraw-display))

(define-editor-variable truncate-lines t)

(defvar *window-sufficient-width* 150)
(defvar *scroll-recenter-p* t)
(defvar *window-scroll-functions* '())
(defvar *window-size-change-functions* '())
(defvar *window-show-buffer-functions* '())

(defvar *floating-windows* '())

(defvar *window-tree*)

(defvar *current-window*)

(defvar *window-id-counter* 0)

(defclass window ()
  ((id
    :initform (mod (incf *window-id-counter*) most-positive-fixnum)
    :reader window-id
    :type fixnum)
   (x
    :initarg :x
    :accessor window-%x
    :type fixnum)
   (y
    :initarg :y
    :accessor window-%y
    :type fixnum)
   (width
    :initarg :width
    :accessor window-%width
    :type fixnum)
   (height
    :initarg :height
    :accessor window-%height
    :type fixnum)
   (buffer
    :initarg :buffer
    :accessor window-%buffer
    :type buffer)
   (screen
    :reader window-screen)
   (view-point
    :reader window-view-point
    :writer set-window-view-point
    :type point)
   (point
    :accessor %window-point
    :type point)
   (delete-hook
    :initform nil
    :reader window-delete-hook
    :writer set-window-delete-hook
    :type (or null function))
   (use-modeline-p
    :initarg :use-modeline-p
    :initform nil
    :reader window-use-modeline-p
    :type boolean)
   (parameters
    :initform nil
    :accessor window-parameters)))

(defmethod initialize-instance :after ((window window) &rest initargs)
  (declare (ignore initargs))
  (with-slots (screen view-point point) window
    (setf screen (make-screen (window-x window)
                              (window-y window)
                              (window-width window)
                              (window-height window)
                              (window-use-modeline-p window)))
    (setf view-point (copy-point (buffer-point (window-buffer window)) :right-inserting))
    (setf point (copy-point (buffer-start-point (window-buffer window)) :right-inserting))))

(defun make-window (buffer x y width height use-modeline-p)
  (make-instance 'window
                 :buffer buffer
                 :x x
                 :y y
                 :width width
                 :height height
                 :use-modeline-p use-modeline-p))

(defgeneric %delete-window (window))
(defgeneric window-redraw (window force)
  (:method (window force)
    (redraw-display-window window force)))

(defun windowp (x)
  (typep x 'window))

(defun window-x (&optional (window (current-window)))
  (window-%x window))

(defun window-y (&optional (window (current-window)))
  (window-%y window))

(defun window-width (&optional (window (current-window)))
  (window-%width window))

(defun window-height (&optional (window (current-window)))
  (window-%height window))

(defun window-buffer (&optional (window (current-window)))
  (window-%buffer window))

(defun find-window (id)
  (window-tree-find-if (window-tree)
                       (lambda (window)
                         (= id (window-id window)))))

(defun set-window-buffer (window buffer)
  (screen-modify (window-screen window))
  (setf (window-%buffer window) buffer))

(defun window-buffer-point (window)
  (buffer-point (window-buffer window)))

(defun window-point (window)
  (if (eq window (current-window))
      (window-buffer-point window)
      (%window-point window)))

(defun window-parameter (window parameter)
  (getf (window-parameters window) parameter))

(defun (setf window-parameter) (value window parameter)
  (setf (getf (window-parameters window) parameter) value))

(defun current-window ()
  *current-window*)

(defun (setf current-window) (new-window)
  (check-type new-window window)
  (when (boundp '*current-window*)
    (let ((old-window (current-window)))
      (move-point (%window-point old-window)
                  (window-buffer-point old-window))))
  (let ((buffer (window-buffer new-window)))
    (setf (current-buffer) buffer)
    (move-point (buffer-point buffer)
                (%window-point new-window)))
  (setf *current-window* new-window))

(defun window-tree ()
  *window-tree*)

(defun (setf window-tree) (new-window-tree)
  (setf *window-tree* new-window-tree))

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

(defun window-list ()
  (window-tree-flatten
   (window-tree)))

(defun one-window-p ()
  (window-tree-leaf-p (window-tree)))

(defun deleted-window-p (window)
  (cond ((minibuffer-window-p window)
         nil)
        ((window-tree-find (window-tree) window)
         nil)
        (t t)))

(defun %free-window (window)
  (delete-point (window-view-point window))
  (delete-point (%window-point window))
  (screen-delete (window-screen window)))

(defun window-topleft-y () 0)
(defun window-topleft-x () 0)
(defun window-max-width () (- (display-width) (window-topleft-x)))
(defun window-max-height () (- (display-height) (minibuffer-window-height) (window-topleft-y)))

(defun window-init ()
  (setf (current-window)
        (make-window (current-buffer)
                     (window-topleft-x)
                     (window-topleft-y)
                     (window-max-width)
                     (window-max-height)
                     t))
  (setf (window-tree) (current-window)))

(defun window-recenter (window)
  (line-start
   (move-point (window-view-point window)
               (window-buffer-point window)))
  (window-scroll window (- (floor (window-%height window) 2))))

(defun map-wrapping-line (window string fn)
  (let ((lem-base::*tab-size* (variable-value 'tab-width :default (window-buffer window))))
    (loop :with start := 0
          :and width := (1- (window-width window))
          :for i := (wide-index string width :start start)
          :while i
          :do
             (funcall fn i)
             (setq start i))))

(defun %scroll-down-if-wrapping (window)
  (when (variable-value 'truncate-lines :default (window-buffer window))
    (let ((view-charpos (point-charpos (window-view-point window))))
      (line-start (window-view-point window))
      (map-wrapping-line window
                         (line-string (window-view-point window))
                         (lambda (c)
                           (when (< view-charpos c)
                             (line-offset (window-view-point window) 0 c)
                             (return-from %scroll-down-if-wrapping t)))))
    nil))

(defun window-scroll-down (window)
  (unless (%scroll-down-if-wrapping window)
    (line-offset (window-view-point window) 1)))

(defun %scroll-up-if-wrapping (window)
  (when (and (variable-value 'truncate-lines :default (window-buffer window))
             (not (first-line-p (window-view-point window))))
    (let ((charpos-list))
      (map-wrapping-line window
                         (line-string (if (start-line-p (window-view-point window))
                                          (line-offset (copy-point (window-view-point window)
                                                                   :temporary)
                                                       -1)
                                          (window-view-point window)))
                         (lambda (c)
                           (push c charpos-list)))
      (cond ((and charpos-list (start-line-p (window-view-point window)))
             (line-offset (window-view-point window) -1)
             (line-end (window-view-point window))))
      (dolist (c charpos-list)
        (when (< c (point-charpos (window-view-point window)))
          (line-offset (window-view-point window) 0 c)
          (return-from %scroll-up-if-wrapping t)))
      (line-start (window-view-point window))
      (not (null charpos-list)))))

(defun window-scroll-up (window)
  (unless (%scroll-up-if-wrapping window)
    (line-offset (window-view-point window) -1)))

(defun window-scroll (window n)
  (screen-modify (window-screen window))
  (dotimes (_ (abs n))
    (if (plusp n)
        (window-scroll-down window)
        (window-scroll-up window)))
  (run-hooks *window-scroll-functions* window))

(defun window-wrapping-offset (window)
  (unless (variable-value 'truncate-lines :default (window-buffer window))
    (return-from window-wrapping-offset 0))
  (let ((offset 0))
    (labels ((inc (arg)
               (declare (ignore arg))
               (incf offset)))
      (map-region (window-view-point window)
                  (window-buffer-point window)
                  (lambda (string lastp)
                    (declare (ignore lastp))
                    (map-wrapping-line window
                                       string
                                       #'inc)))
      offset)))

(defun window-cursor-y-not-wrapping (window)
  (count-lines (window-buffer-point window)
               (window-view-point window)))

(defun window-cursor-y (window)
  (+ (window-cursor-y-not-wrapping window)
     (window-wrapping-offset window)))

(defun window-offset-view (window)
  (cond ((and (point< (window-buffer-point window)
                      (window-view-point window))
              (not (same-line-p (window-buffer-point window)
                                (window-view-point window))))
         (- (count-lines (window-buffer-point window)
                         (window-view-point window))))
        ((and (same-line-p (window-buffer-point window)
                           (window-view-point window))
              (not (start-line-p (window-view-point window))))
         -1)
        ((let ((n (- (window-cursor-y window)
                     (- (window-%height window)
                        (if (window-use-modeline-p window) 2 1)))))
           (when (< 0 n) n)))
        (t
         0)))

(defun window-see (window &optional (recenter *scroll-recenter-p*))
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
          (window-recenter window)
          (window-scroll window offset)))))

(defun split-window-after (current-window new-window split-type)
  (window-set-size current-window
                   (window-%width current-window)
                   (window-%height current-window))
  (move-point (window-view-point new-window)
              (window-view-point current-window))
  (move-point (%window-point new-window)
              (window-buffer-point current-window))
  (move-point (window-buffer-point new-window)
              (window-buffer-point current-window))
  (window-see new-window)
  (multiple-value-bind (node getter setter)
      (window-tree-parent (window-tree) current-window)
    (if (null node)
        (setf (window-tree)
              (make-window-node split-type
                                current-window
                                new-window))
        (funcall setter
                 (make-window-node split-type
                                   (funcall getter)
                                   new-window))))
  t)

(defun split-window-vertically (window &optional height)
  (unless (minibuffer-window-p window)
    (let* ((use-modeline-p t)
           (min (+ 1 (if use-modeline-p 1 0)))
           (max (- (window-height window) min)))
      (cond ((null height)
             (setf height (floor (window-height window) 2)))
            ((> min height)
             (setf height min))
            ((< max height)
             (setf height max))))
    (let ((new-window
            (make-window (window-buffer window)
                         (window-x window)
                         (+ (window-y window) height)
                         (window-width window)
                         (- (window-height window) height)
                         t)))
      (setf (window-%height window) height)
      (split-window-after window new-window :vsplit))))

(defun split-window-horizontally (window &optional width)
  (unless (minibuffer-window-p window)
    (let* ((fringe-size 0)
           (min (+ 2 fringe-size))
           (max (- (window-width window) min)))
      (cond ((null width)
             (setf width (floor (window-width window) 2)))
            ((> min width)
             (setf width min))
            ((< max width)
             (setf width max))))
    (let ((new-window
            (make-window (window-buffer window)
                         (+ 1 (window-x window) width)
                         (window-y window)
                         (- (window-width window) width 1)
                         (window-height window)
                         t)))
      (setf (window-%width window) width)
      (split-window-after window new-window :hsplit))))

(defun split-window-sensibly (window)
  (if (< *window-sufficient-width* (window-%width window))
      (split-window-horizontally window)
      (split-window-vertically window)))

(defun get-next-window (window &optional (window-list (window-list)))
  (let ((result (member window window-list)))
    (if (cdr result)
        (cadr result)
        (car window-list))))

(defun window-set-pos (window x y)
  (screen-set-pos (window-screen window) x y)
  (setf (window-%x window) x)
  (setf (window-%y window) y))

(defun window-set-size (window width height)
  (setf (window-%width window) width)
  (setf (window-%height window) height)
  (screen-set-size (window-screen window)
                   width
                   height))

(defun window-move (window dx dy)
  (window-set-pos window
                  (+ (window-%x window) dx)
                  (+ (window-%y window) dy)))

(defun window-resize (window dw dh)
  (window-set-size window
                   (+ (window-%width window) dw)
                   (+ (window-%height window) dh))
  (run-hooks *window-size-change-functions* window))

(defun adjust-size-windows-after-delete-window (deleted-window
                                                window-tree
                                                horizontal-p)
  (let ((window-list (window-tree-flatten window-tree)))
    (if horizontal-p
        (cond ((< (window-%x deleted-window)
                  (window-%x (car window-list)))
               (dolist (win (min-if #'window-%x window-list))
                 (window-set-pos win
                                 (window-%x deleted-window)
                                 (window-%y win))
                 (window-set-size win
                                  (+ (window-%width deleted-window)
                                     1
                                     (window-%width win))
                                  (window-%height win))))
              (t
               (dolist (win (max-if #'window-%x window-list))
                 (window-set-size win
                                  (+ (window-%width deleted-window)
                                     1
                                     (window-%width win))
                                  (window-%height win)))))
        (cond ((< (window-%y deleted-window)
                  (window-%y (car window-list)))
               (dolist (win (min-if #'window-%y window-list))
                 (window-set-pos win
                                 (window-%x win)
                                 (window-%y deleted-window))
                 (window-set-size win
                                  (window-%width win)
                                  (+ (window-%height deleted-window)
                                     (window-%height win)))))
              (t
               (dolist (win (max-if #'window-%y window-list))
                 (window-set-size win
                                  (window-%width win)
                                  (+ (window-%height deleted-window)
                                     (window-%height win)))))))))

(defmethod %delete-window ((window window))
  (when (or (one-window-p)
            (minibuffer-window-p window))
    (editor-error "Can not delete this window"))
  (when (eq (current-window) window)
    (setf (current-window)
          (get-next-window (current-window))))
  (multiple-value-bind (node getter setter another-getter another-setter)
      (window-tree-parent (window-tree) window)
    (declare (ignore getter setter another-setter))
    (adjust-size-windows-after-delete-window
     window
     (funcall another-getter)
     (eq (window-node-split-type node) :hsplit))
    (multiple-value-bind (node2 getter2 setter2)
        (window-tree-parent (window-tree) node)
      (declare (ignore getter2))
      (if (null node2)
          (setf (window-tree) (funcall another-getter))
          (funcall setter2 (funcall another-getter))))))

(defun delete-window (window)
  (%delete-window window)
  (when (window-delete-hook window)
    (funcall (window-delete-hook window)))
  (%free-window window)
  t)

(defun collect-left-windows (window-list)
  (min-if #'window-%x window-list))

(defun collect-right-windows (window-list)
  (max-if (lambda (window)
            (+ (window-%x window)
               (window-%width window)))
          window-list))

(defun collect-top-windows (window-list)
  (min-if #'window-%y window-list))

(defun collect-bottom-windows (window-list)
  (max-if (lambda (window)
            (+ (window-%y window)
               (window-%height window)))
          window-list))

(defun %shrink-windows (window-list
                        collect-windows-fn
                        check-fn
                        diff-height
                        diff-width
                        shift-height
                        shift-width)
  (let ((shrink-window-list
          (funcall collect-windows-fn window-list)))
    (dolist (window shrink-window-list)
      (when (not (funcall check-fn window))
        (return-from %shrink-windows nil)))
    (cond ((/= 0 diff-width)
           (dolist (window shrink-window-list)
             (window-resize window (- diff-width) 0)))
          ((/= 0 diff-height)
           (dolist (window shrink-window-list)
             (window-resize window 0 (- diff-height)))))
    (cond ((/= 0 shift-width)
           (dolist (window shrink-window-list)
             (window-move window shift-width 0)))
          ((/= 0 shift-height)
           (dolist (window shrink-window-list)
             (window-move window 0 shift-height)))))
  t)

(defun shrink-top-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-top-windows
                   (lambda (window)
                     (< 2 (window-%height window)))
                   n 0 n 0))

(defun shrink-bottom-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-bottom-windows
                   (lambda (window)
                     (< 2 (window-%height window)))
                   n 0 0 0))

(defun shrink-left-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-left-windows
                   (lambda (window)
                     (< 2 (window-%width window)))
                   0 n 0 n))

(defun shrink-right-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-right-windows
                   (lambda (window)
                     (< 2 (window-%width window)))
                   0 n 0 0))

(defun %grow-windows (window-list
                      collect-windows-fn
                      diff-height
                      diff-width
                      shift-height
                      shift-width)
  (dolist (window (funcall collect-windows-fn window-list))
    (cond ((/= 0 shift-width)
           (window-move window shift-width 0))
          ((/= 0 shift-height)
           (window-move window 0 shift-height)))
    (cond ((/= 0 diff-width)
           (window-resize window diff-width 0))
          ((/= 0 diff-height)
           (window-resize window 0 diff-height))))
  t)

(defun grow-top-windows (window-list n)
  (%grow-windows window-list
                 #'collect-top-windows
                 n 0 (- n) 0))

(defun grow-bottom-windows (window-list n)
  (%grow-windows window-list
                 #'collect-bottom-windows
                 n 0 0 0))

(defun grow-left-windows (window-list n)
  (%grow-windows window-list
                 #'collect-left-windows
                 0 n 0 (- n)))

(defun grow-right-windows (window-list n)
  (%grow-windows window-list
                 #'collect-right-windows
                 0 n 0 0))

(defun grow-window-internal (grow-window-list shrink-window-list n)
  (if (< (window-%y (car grow-window-list))
         (window-%y (car shrink-window-list)))
      (and (shrink-top-windows shrink-window-list n)
           (grow-bottom-windows grow-window-list n))
      (and (shrink-bottom-windows shrink-window-list n)
           (grow-top-windows grow-window-list n))))

(defun grow-window-horizontally-internal
    (grow-window-list shrink-window-list n)
  (if (< (window-%x (car grow-window-list))
         (window-%x (car shrink-window-list)))
      (and (shrink-left-windows shrink-window-list n)
           (grow-right-windows grow-window-list n))
      (and (shrink-right-windows shrink-window-list n)
           (grow-left-windows grow-window-list n))))

(defun resize-window-recursive (node n apply-fn split-type)
  (multiple-value-bind (parent-node
                        getter
                        setter
                        another-getter
                        another-setter)
      (window-tree-parent (window-tree) node)
    (declare (ignore setter another-setter))
    (cond ((null parent-node) nil)
          ((eq split-type (window-node-split-type parent-node))
           (funcall apply-fn
                    (window-tree-flatten (funcall getter))
                    (window-tree-flatten (funcall another-getter))
                    n))
          (t
           (resize-window-recursive parent-node n apply-fn split-type)))))

(defun adjust-windows (frame-x frame-y frame-width frame-height)
  (let ((window-list (window-list)))
    (let ((old-frame-x (window-x (first (collect-left-windows window-list))))
          (old-frame-y (window-y (first (collect-top-windows window-list)))))
      (dolist (window window-list)
        (window-set-pos window
                        (+ (window-x window) (- frame-x old-frame-x))
                        (+ (window-y window) (- frame-y old-frame-y))))
      (let ((delete-windows '()))
        (dolist (window window-list)
          (cond ((<= frame-height (+ 2 (window-y window)))
                 (push window delete-windows))
                ((<= frame-width (+ 1 (window-x window)))
                 (push window delete-windows))))
        (dolist (window delete-windows)
          (when (one-window-p) (return))
          (delete-window window)))))
  (let ((window-list (window-list)))
    (dolist (window (collect-right-windows window-list))
      (window-resize window
                     (- frame-width
                        (+ (window-x window) (window-width window)))
                     0))
    (dolist (window (collect-bottom-windows window-list))
      (window-resize window
                     0
                     (- frame-height
                        (+ (window-y window) (window-height window)))))))

(defun get-buffer-windows (buffer)
  (loop :for window :in (window-list)
        :when (eq buffer (window-buffer window))
        :collect window))

(defun other-buffer ()
  (let ((buffer-list (buffer-list)))
    (dolist (win (window-list))
      (setq buffer-list
            (remove (window-buffer win)
                    buffer-list)))
    (if (null buffer-list)
        (car (buffer-list))
        (car buffer-list))))

(defun run-show-buffer-hooks (window)
  (when (window-parameter window 'change-buffer)
    (setf (window-parameter window 'change-buffer) nil)
    (run-hooks *window-show-buffer-functions* window)))

(defun switch-to-buffer (buffer &optional (update-prev-buffer-p t) (move-prev-point t))
  (check-type buffer buffer)
  (check-switch-minibuffer-window)
  (without-interrupts
    (unless (eq (current-buffer) buffer)
      (when update-prev-buffer-p
        (setf (window-parameter (current-window) 'split-p) nil)
        (let ((old-buffer (current-buffer)))
          (update-prev-buffer old-buffer)
          (%buffer-clear-keep-binfo old-buffer)
          (setf (%buffer-keep-binfo old-buffer)
                (list (copy-point (window-view-point (current-window)) :right-inserting)
                      (copy-point (window-buffer-point (current-window)) :right-inserting)))))
      (set-window-buffer (current-window) buffer)
      (setf (current-buffer) buffer)
      (delete-point (%window-point (current-window)))
      (delete-point (window-view-point (current-window)))
      (cond ((and (%buffer-keep-binfo buffer) move-prev-point)
             (destructuring-bind (view-point cursor-point)
                 (%buffer-keep-binfo buffer)
               (set-window-view-point (copy-point view-point) (current-window))
               (setf (%window-point (current-window)) (copy-point cursor-point))
               (move-point (buffer-point (current-buffer)) cursor-point)))
            (t
             (setf (%window-point (current-window))
                   (copy-point (buffer-start-point buffer) :right-inserting))
             (set-window-view-point (copy-point (buffer-start-point buffer) :right-inserting)
                                    (current-window)))))
    (setf (window-parameter (current-window) 'change-buffer) t))
  buffer)

(defun pop-to-buffer (buffer &optional force-split-p)
  (if (eq buffer (current-buffer))
      (return-from pop-to-buffer (values (current-window) nil))
      (let ((parent-window (current-window))
            (split-p))
        (when (or (one-window-p) force-split-p)
          (setf split-p t)
          (split-window-sensibly (if (minibuffer-window-active-p)
                                     (minibuffer-calls-window)
                                     (current-window))))
        (with-current-window
            (or (window-tree-find-if (window-tree)
                                     (lambda (window)
                                       (eq buffer (window-buffer window))))
                (get-next-window (if (minibuffer-window-active-p)
                                     (minibuffer-calls-window)
                                     (current-window))))
          (switch-to-buffer buffer)
          (setf (window-parameter (current-window) 'split-p) split-p)
          (setf (window-parameter (current-window) 'parent-window) parent-window)
          (values (current-window) split-p)))))

(defclass floating-window (window) ())

(defun make-floating-window (buffer x y width height use-modeline-p)
  (let ((window (make-instance 'floating-window
                               :buffer buffer
                               :x x
                               :y y
                               :width width
                               :height height
                               :use-modeline-p use-modeline-p)))
    (push window *floating-windows*)
    window))

(defmethod %delete-window ((window floating-window))
  (setf *floating-windows*
        (delete window *floating-windows*)))

(defun balloon (orig-window buffer width height)
  (let* ((y (+ (window-y orig-window)
               (window-cursor-y orig-window)
               1))
         (x (+ (window-x orig-window)
               (let ((x (point-column (window-buffer-point orig-window))))
                 (when (<= (window-width orig-window) x)
                   (let ((mod (mod x (window-width orig-window)))
                         (floor (floor x (window-width orig-window))))
                     (setf x (+ mod floor))
                     (incf y floor)))
                 x))))
    (cond
      ((<= (display-height)
           (+ y (min height
                     (floor (display-height) 3))))
       (cond ((>= 0 (- y height))
              (setf y 1)
              (setf height (min height (- (display-height) 1))))
             (t
              (decf y (+ height 1)))))
      ((<= (display-height) (+ y height))
       (setf height (- (display-height) y))))
    (when (<= (display-width) (+ x width))
      (when (< (display-width) width)
        (setf width (display-width)))
      (setf x (- (display-width) width)))
    (make-floating-window buffer x y width height nil)))

(defvar *balloon-message-window* nil)

(defun balloon-message (text)
  (clear-balloon-message)
  (let ((buffer (make-buffer "*balloon*" :temporary t :enable-undo-p nil)))
    (erase-buffer buffer)
    (let ((p (buffer-point buffer))
          (max-column 0))
      (insert-string p text)
      (buffer-start p)
      (loop :for column := (point-column (line-end p))
            :do (setf max-column (max max-column column))
            :while (line-offset p 1))
      (buffer-start p)
      (with-point ((s p))
        (loop :do
                 (move-to-column p max-column t)
                 (put-text-property (line-start (move-point s p))
                                    (line-end p)
                                    :attribute 'balloon-attribute)
              :while (line-offset p 1)))
      (let ((window
              (balloon (current-window)
                       buffer
                       (+ 1 max-column)
                       (buffer-nlines buffer))))
        (buffer-start (window-view-point window))
        (window-see window)
        (setf *balloon-message-window* window)))))

(defun clear-balloon-message ()
  (when *balloon-message-window*
    (delete-window *balloon-message-window*)
    (setf *balloon-message-window* nil)
    (redraw-display t)))

(defun redraw-display (&optional force)
  (without-interrupts
    (dolist (window (window-list))
      (unless (eq window (current-window))
        (window-redraw window force)))
    (cond ((minibuffer-window-active-p)
           (window-redraw (current-minibuffer-window) force))
          (t
           (window-redraw (current-minibuffer-window) force)
           (window-redraw (current-window) force)))
    (dolist (window *floating-windows*)
      (window-redraw window t))
    (update-display)))

(defun change-display-size-hook (redraw-p)
  (adjust-windows (window-topleft-x)
                  (window-topleft-y)
                  (+ (window-max-width) (window-topleft-x))
                  (+ (window-max-height) (window-topleft-y)))
  (minibuf-update-size)
  (when redraw-p (redraw-display)))
