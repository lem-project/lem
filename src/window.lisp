(in-package :lem)

(export '(*window-sufficient-width*
          *scroll-recenter-p*
          *window-scroll-functions*
          *window-size-change-functions*
          window-list
          window
          window-screen
          window-use-modeline-p
          window-height
          window-width
          window-y
          window-x
          window-buffer
          window-delete-hook
          current-window
          one-window-p
          deleted-window-p
          window-see
          split-window-vertically
          split-window-horizontally
          split-window-sensibly
          get-next-window
          delete-window
          switch-to-buffer
          pop-to-buffer))

(defvar *window-sufficient-width* 150)
(defvar *scroll-recenter-p* t)
(defvar *window-scroll-functions* nil)
(defvar *window-size-change-functions* nil)
(defvar *window-show-buffer-functions* nil)

(defvar *modified-window-tree-p* nil)

(defvar *window-tree*)

(defvar *current-window*)

(defclass window ()
  ((x
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
   (%buffer
    :initarg :%buffer
    :accessor window-%buffer
    :type buffer)
   (screen
    :initarg :screen
    :reader window-screen)
   (view-marker
    :initarg :view-marker
    :reader window-view-marker
    :writer set-window-view-marker
    :type marker)
   (point-marker
    :initarg :point-marker
    :reader %window-point-marker
    :type marker)
   (delete-hook
    :initform nil
    :reader window-delete-hook
    :writer set-window-delete-hook
    :type (or null function))
   (use-modeline-p
    :initarg :use-modeline-p
    :reader window-use-modeline-p
    :type boolean)
   (parameters
    :initform nil
    :accessor window-parameters)))

(defun window-p (x)
  (typep x 'window))

(defun make-window (buffer x y width height use-modeline-p)
  (setf *modified-window-tree-p* t)
  (make-instance 'window
                 :x x
                 :y y
                 :width width
                 :height height
                 :%buffer buffer
                 :screen (make-screen x y width height use-modeline-p)
                 :view-marker (copy-marker (buffer-point-marker buffer) :right-inserting)
                 :use-modeline-p use-modeline-p
                 :point-marker (copy-marker (buffers-start buffer) :right-inserting)))

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

(defun (setf window-buffer) (buffer &optional (window (current-window)))
  (screen-modify (window-screen window))
  (setf (window-%buffer window) buffer))

(defun window-point-marker (window)
  (if (eq window (current-window))
      (buffer-point-marker (window-buffer window))
      (%window-point-marker window)))

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
      (move-point (%window-point-marker old-window)
                  (buffer-point-marker (window-buffer old-window)))))
  (let ((buffer (window-buffer new-window)))
    (setf (current-buffer) buffer)
    (move-point (buffer-point-marker buffer)
                (%window-point-marker new-window)))
  (setf *current-window* new-window))

(defun window-tree ()
  *window-tree*)

(defun (setf window-tree) (new-window-tree)
  (setf *modified-window-tree-p* t)
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
  (window-p window))

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
                     #'(lambda (win)
                         (push win windows)))
    (nreverse windows)))

(defun window-tree-find (tree window)
  (window-tree-map tree
                   #'(lambda (win)
                       (when (eq win window)
                         (return-from window-tree-find win)))))

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
  (delete-marker (window-view-marker window))
  (delete-marker (%window-point-marker window))
  (screen-delete (window-screen window)))

(defun dump-window-tree (window-tree current-window)
  (labels ((f (window-tree)
              (if (window-tree-leaf-p window-tree)
                  (list
                   :window
                   (eq current-window window-tree)
                   (buffer-name (window-buffer window-tree))
                   (window-%x window-tree)
                   (window-%y window-tree)
                   (window-%width window-tree)
                   (window-%height window-tree)
                   (window-view-marker window-tree)
                   (%window-point-marker window-tree)
                   (window-delete-hook window-tree)
                   (window-parameters window-tree)
                   (window-use-modeline-p window-tree))
                  (list
                   (window-node-split-type window-tree)
                   (f (window-node-car window-tree))
                   (f (window-node-cdr window-tree))))))
    (f window-tree)))

(defun load-window-tree (dumped-tree)
  (dolist (window (window-list))
    (%free-window window))
  (let ((current-window nil))
    (labels ((f (dumped-tree)
                (if (eq :window (car dumped-tree))
                    (destructuring-bind (current-window-p
                                         buffer-name
                                         x
                                         y
                                         width
                                         height
                                         view-marker
                                         point-marker
                                         delete-hook
                                         parameters
                                         use-modeline-p)
                        (cdr dumped-tree)
                      (let ((window (make-window (get-buffer-create buffer-name)
                                                 x y width height use-modeline-p)))
                        (move-point (window-view-marker window) view-marker)
                        (move-point (%window-point-marker window) point-marker)
                        (set-window-delete-hook delete-hook window)
                        (setf (window-parameters window) parameters)
                        (when current-window-p
                          (setf current-window window))
                        window))
                    (destructuring-bind (split-type car-window cdr-window)
                        dumped-tree
                      (make-window-node split-type
                                        (f car-window)
                                        (f cdr-window))))))
      (setf (window-tree) (f dumped-tree))
      (setf (current-window)
            (or current-window
                (car (window-list)))))))

(defun call-with-save-windows (current-window function)
  (let ((dumped-tree (dump-window-tree (window-tree) current-window))
        (*modified-window-tree-p* nil))
    (unwind-protect (funcall function)
      (when *modified-window-tree-p*
        (load-window-tree dumped-tree)))))

(defun window-max-width () (display-width))
(defun window-max-height () (- (display-height) (minibuffer-window-height)))

(defun window-init ()
  (setf (current-window)
        (make-window (get-buffer-create "*tmp*")
                     0
                     0
                     (window-max-width)
                     (window-max-height)
                     t))
  (setf (window-tree) (current-window)))

(defun window-recenter (window)
  (move-point (window-view-marker window)
              (line-start (copy-marker (window-point-marker window) :temporary)))
  (window-scroll window (- (floor (window-%height window) 2))))

(defun map-wrapping-line (string winwidth fn)
  (loop :with start := 0
        :for i := (wide-index string (1- winwidth) :start start)
        :while i :do
        (funcall fn i)
        (setq start i)))

(defun %scroll-down-if-wrapping (window)
  (when (buffer-truncate-lines (window-buffer window))
    (let ((view-charpos (marker-charpos (window-view-marker window))))
      (line-start (window-view-marker window))
      (map-wrapping-line (line-string-at (window-view-marker window))
                         (window-%width window)
                         (lambda (c)
                           (when (< view-charpos c)
                             (line-offset (window-view-marker window) 0 c)
                             (return-from %scroll-down-if-wrapping t)))))
    nil))

(defun window-scroll-down (window)
  (unless (%scroll-down-if-wrapping window)
    (line-offset (window-view-marker window) 1)))

(defun %scroll-up-if-wrapping (window)
  (when (and (buffer-truncate-lines (window-buffer window))
             (not (first-line-p (window-view-marker window))))
    (let ((charpos-list))
      (map-wrapping-line (line-string-at (if (start-line-p (window-view-marker window))
                                             (line-offset (copy-marker (window-view-marker window)
                                                                       :temporary)
                                                          -1)
                                             (window-view-marker window)))
                         (window-%width window)
                         (lambda (c)
                           (push c charpos-list)))
      (cond ((and charpos-list (start-line-p (window-view-marker window)))
             (line-offset (window-view-marker window) -1)
             (line-end (window-view-marker window))))
      (dolist (c charpos-list)
        (when (< c (marker-charpos (window-view-marker window)))
          (line-offset (window-view-marker window) 0 c)
          (return-from %scroll-up-if-wrapping t)))
      (line-start (window-view-marker window))
      (not (null charpos-list)))))

(defun window-scroll-up (window)
  (unless (%scroll-up-if-wrapping window)
    (line-offset (window-view-marker window) -1)))

(defun window-scroll (window n)
  (screen-modify (window-screen window))
  (dotimes (_ (abs n))
    (if (plusp n)
        (window-scroll-down window)
        (window-scroll-up window)))
  (dolist (fun *window-scroll-functions*)
    (funcall fun window)))

(defun window-wrapping-offset (window)
  (unless (buffer-truncate-lines (window-buffer window))
    (return-from window-wrapping-offset 0))
  (let ((offset 0))
    (labels ((inc (arg)
               (declare (ignore arg))
               (incf offset)))
      (map-region (window-view-marker window)
                  (window-point-marker window)
                  (lambda (string lastp)
                    (declare (ignore lastp))
                    (map-wrapping-line string
                                       (window-%width window)
                                       #'inc)))
      offset)))

(defun window-cursor-y-not-wrapping (window)
  (1- (count-lines (window-point-marker window)
                   (window-view-marker window))))

(defun window-cursor-y (window)
  (+ (window-cursor-y-not-wrapping window)
     (window-wrapping-offset window)))

(defun window-offset-view (window)
  (cond ((and (marker< (window-point-marker window)
                       (window-view-marker window))
              (not (same-line-p (window-point-marker window)
                                (window-view-marker window))))
         (1- (count-lines (window-point-marker window)
                          (window-view-marker window))))
        ((and (same-line-p (window-point-marker window)
                           (window-view-marker window))
              (not (start-line-p (window-view-marker window))))
         -1)
        ((let ((n (- (window-cursor-y window)
                     (- (window-%height window) 2))))
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
  (move-point (window-view-marker new-window)
              (window-view-marker current-window))
  (move-point (window-point-marker new-window)
              (window-point-marker current-window))
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

(defun split-window-vertically (window)
  (unless (minibuffer-window-p window)
    (multiple-value-bind (winheight rem)
        (floor (window-%height window) 2)
      (let ((newwin (make-window (window-buffer window)
                                 (window-%x window)
                                 (+ (window-%y window) winheight rem)
                                 (window-%width window)
                                 winheight
                                 t)))
        (decf (window-%height window) winheight)
        (split-window-after window newwin :vsplit)))))

(defun split-window-horizontally (window)
  (unless (minibuffer-window-p window)
    (multiple-value-bind (winwidth rem)
        (floor (window-%width window) 2)
      (let ((newwin (make-window (window-buffer window)
                                 (+ (window-%x window)
                                    winwidth
                                    rem)
                                 (window-%y window)
                                 winwidth
                                 (window-%height window)
                                 t)))
        (decf (window-%width window) (1+ winwidth))
        (split-window-after window newwin :hsplit)))))

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
  (dolist (fun *window-size-change-functions*)
    (funcall fun window)))

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

(defun delete-window (window)
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
          (funcall setter2 (funcall another-getter)))))
  (when (window-delete-hook window)
    (funcall (window-delete-hook window)))
  (%free-window window)
  t)

(defun collect-left-windows (window-list)
  (min-if #'window-%x window-list))

(defun collect-right-windows (window-list)
  (max-if #'(lambda (window)
              (+ (window-%x window)
                 (window-%width window)))
          window-list))

(defun collect-top-windows (window-list)
  (min-if #'window-%y window-list))

(defun collect-bottom-windows (window-list)
  (max-if #'(lambda (window)
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
                   #'(lambda (window)
                       (< 2 (window-%height window)))
                   n 0 n 0))

(defun shrink-bottom-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-bottom-windows
                   #'(lambda (window)
                       (< 2 (window-%height window)))
                   n 0 0 0))

(defun shrink-left-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-left-windows
                   #'(lambda (window)
                       (< 2 (window-%width window)))
                   0 n 0 n))

(defun shrink-right-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-right-windows
                   #'(lambda (window)
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

(defun get-buffer-windows (buffer)
  (loop :for window :in (window-list)
    :when (eq buffer (window-buffer window))
    :collect window))

(defun window-prompt-display (window)
  (when (window-parameter window 'change-buffer)
    (setf (window-parameter window 'change-buffer) nil)
    (dolist (fun *window-show-buffer-functions*)
      (funcall fun window))))

(defun switch-to-buffer (buffer &optional (update-prev-buffer-p t))
  (check-type buffer buffer)
  (check-switch-minibuffer-window)
  (unless (eq (current-buffer) buffer)
    (when update-prev-buffer-p
      (setf (window-parameter (current-window) :split-p) nil)
      (let ((old-buffer (current-buffer)))
        (update-prev-buffer old-buffer)
        (buffer-clear-keep-binfo old-buffer)
        (setf (buffer-keep-binfo old-buffer)
              (list (copy-marker (window-view-marker (current-window)))
                    (copy-marker (buffer-point-marker (window-buffer (current-window))))))))
    (setf (window-buffer (current-window)) buffer)
    (setf (current-buffer) buffer)
    (marker-change-buffer (%window-point-marker (current-window)) buffer)
    (marker-change-buffer (window-view-marker (current-window)) buffer)
    (cond ((buffer-keep-binfo buffer)
           (destructuring-bind (view-point cursor-point)
               (buffer-keep-binfo buffer)
             (move-point (window-view-marker (current-window)) view-point)
             (move-point (%window-point-marker (current-window)) cursor-point)
             (move-point (window-point-marker (current-window)) cursor-point)))
          (t
           (move-point (window-view-marker (current-window)) (buffers-start buffer)))))
  (setf (window-parameter (current-window) 'change-buffer) t)
  buffer)

(defun pop-to-buffer (buffer)
  (if (eq buffer (current-buffer))
      (values (current-window) nil)
      (let ((split-p))
        (when (one-window-p)
          (setq split-p t)
          (split-window-sensibly (if (minibuffer-window-active-p)
                                     (minibuffer-calls-window)
                                     (current-window))))
        (with-current-window (or (window-tree-find (window-tree)
                                                   (lambda (window)
                                                     (eq buffer (window-buffer window))))
                                 (get-next-window (if (minibuffer-window-active-p)
                                                      (minibuffer-calls-window)
                                                      (current-window))))
          (switch-to-buffer buffer)
          (values (current-window) split-p)))))
