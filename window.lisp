;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*modeline-default-format*
          *window-sufficient-width*
          modeline-read-only-p
          modeline-modified-p
          modeline-name
          modeline-major-mode
          modeline-minor-modes
          modeline-linum
          modeline-column
          window-list
          window
          window-point
          window-height
          window-width
          window-y
          window-x
          window-buffer
          window-vtop-linum
          window-current-linum
          window-current-charpos
          window-delete-hook
          current-window
          one-window-p
          deleted-window-p
          recenter
          redraw-display
          split-window-vertically
          split-window-horizontally
          split-window
          get-next-window
          other-window
          delete-other-windows
          delete-current-window
          delete-window
          pop-to-buffer
          display-buffer
          quit-window
          grow-window
          shrink-window
          grow-window-horizontally
          shrink-window-horizontally
          scroll-down
          scroll-up))

(defvar *redraw-flags* '(:one-line :unnecessary :all))

(defvar *window-sufficient-width* 150)

(defvar *modeline-default-format*
  (list 'modeline-read-only-p
        'modeline-modified-p
        " "
        *program-name*
        ": "
        'modeline-name
        " ("
        'modeline-major-mode
        'modeline-minor-modes
        ") "
        "("
        'modeline-linum
        ", "
        'modeline-column
        ")"))

(defvar *modified-window-tree-p* nil)

(defvar *window-tree*)

(defvar *current-window*)

(defstruct (window (:constructor %make-window))
  x
  y
  width
  height
  buffer
  %screen
  vtop-linum
  vtop-charpos
  point-marker
  redraw-flag
  delete-hook
  parameters)

(defun make-window (buffer x y width height)
  (let* ((window
          (%make-window
           :x x
           :y y
           :width width
           :height height
           :buffer buffer
           :%screen (make-screen x y width height)
           :vtop-linum 1
           :vtop-charpos 0)))
    (setf (window-point-marker window)
          (make-marker buffer (make-point 1 0)))
    window))

(defun window-screen (window)
  (screen-%scrwin (window-%screen window)))

(defun (setf window-screen) (new-win window)
  (setf (screen-%scrwin (window-%screen window)) new-win))

(defun window-point (&optional (window (current-window)))
  (marker-point (window-point-marker window)))

(defun window-current-charpos (&optional (window (current-window)))
  (marker-charpos (window-point-marker window)))

(defun (setf window-current-charpos) (new-pos &optional (window (current-window)))
  (setf (marker-charpos (window-point-marker window)) new-pos))

(defun window-current-linum (&optional (window (current-window)))
  (marker-linum (window-point-marker window)))

(defun (setf window-current-linum) (new-linum &optional (window (current-window)))
  (setf (marker-linum (window-point-marker window)) new-linum))

(defun window-parameter (window parameter)
  (getf (window-parameters window) parameter))

(defun (setf window-parameter) (value window parameter)
  (setf (getf (window-parameters window) parameter) value))

(defun current-window ()
  *current-window*)

(defun (setf current-window) (new-window)
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

(defun dump-window-tree (window-tree current-window)
  (labels ((f (window-tree)
              (if (window-tree-leaf-p window-tree)
                  (list
                   :window
                   (eq current-window window-tree)
                   (buffer-name (window-buffer window-tree))
                   (window-x window-tree)
                   (window-y window-tree)
                   (window-width window-tree)
                   (window-height window-tree)
                   (window-vtop-linum window-tree)
                   (window-vtop-charpos window-tree)
                   (marker-point (window-point-marker window-tree))
                   (window-delete-hook window-tree)
                   (window-parameters window-tree))
                  (list
                   (window-node-split-type window-tree)
                   (f (window-node-car window-tree))
                   (f (window-node-cdr window-tree))))))
    (f window-tree)))

(defun load-window-tree (dumped-tree)
  (dolist (window (window-list))
    (screen-delete (window-%screen window)))
  (let ((current-window nil))
    (labels ((f (dumped-tree)
                (if (eq :window (car dumped-tree))
                    (destructuring-bind (current-window-p
                                         buffer-name
                                         x
                                         y
                                         width
                                         height
                                         vtop-linum
                                         vtop-charpos
                                         point
                                         delete-hook
                                         parameters)
                        (cdr dumped-tree)
                      (let ((window (make-window (get-buffer-create buffer-name)
                                                 x y width height)))
                        (setf (window-vtop-charpos window) vtop-charpos)
                        (setf (window-vtop-linum window) vtop-linum)
                        (setf (window-delete-hook window) delete-hook)
                        (setf (window-parameters window) parameters)
                        (point-set point window)
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

(defun window-init ()
  (setf (current-window)
        (make-window (get-buffer-create "*tmp*")
                     0
                     0
                     charms/ll:*cols*
                     (- charms/ll:*lines* 1)))
  (setf (window-tree) (current-window)))

(define-key *global-keymap* (kbd "C-l") 'recenter)
(define-command recenter () ()
  (dolist (window (window-list))
    (screen-clear (window-%screen window)))
  (window-recenter (current-window))
  (syntax-scan-window (current-window))
  (redraw-display)
  t)

(defun window-recenter (window)
  (setf (window-vtop-charpos window) 0)
  (setf (window-vtop-linum window)
        (window-current-linum window))
  (window-scroll window (- (floor (window-height window) 2))))

(defun window-posline (window)
  (cond
   ((<= (buffer-nlines (window-buffer window))
        (window-height window))
    "All")
   ((= 1 (window-vtop-linum window))
    "Top")
   ((<= (buffer-nlines (window-buffer window))
        (+ (window-vtop-linum window) (window-height window)))
    "Bot")
   (t
    (format nil "~2d%"
            (floor
             (* 100
                (float (/ (window-vtop-linum window)
                          (buffer-nlines (window-buffer window))))))))))

(defun modeline-read-only-p (window)
  (if (buffer-read-only-p (window-buffer window)) "%" "-"))
(defun modeline-modified-p (window)
  (if (buffer-modified-p (window-buffer window)) "*" "-"))
(defun modeline-name (window)
  (buffer-name (window-buffer window)))
(defun modeline-major-mode (window)
  (string-downcase (buffer-major-mode (window-buffer window))))
(defun modeline-minor-modes (window)
  (let ((modes (buffer-minor-modes (window-buffer window))))
    (if modes
        (format nil " ~(~{~a~^ ~}~)" modes)
        "")))
(defun modeline-linum (window)
  (window-current-linum window))
(defun modeline-column (window)
  (str-width (buffer-line-string (window-buffer window)
                                 (window-current-linum window))
             0
             (window-current-charpos window)))

(defun modeline-string (window)
  (let* ((line-pos (window-posline window))
         (winwidth (window-width window))
         (str (with-output-to-string (out)
                (dolist (x
                         (get-bvar :modeline-format
                                   :buffer (window-buffer window)
                                   :default *modeline-default-format*))
                  (if (or (symbolp x) (functionp x))
                      (princ (funcall x window) out)
                      (princ x out))))))
    (let ((n (- winwidth 7 (length str))))
      (if (minusp n)
          (format nil "~a ~a --" str line-pos)
          (format nil "~a~v,,,va ~a --"
                  str
                  n
                  (if (eq window (current-window)) #\- #\space)
                  #\space
                  line-pos)))))

(defun map-wrapping-line (string winwidth fn)
  (loop :with start := 0
    :for i := (wide-index string (1- winwidth) :start start)
    :while i :do
    (funcall fn i)
    (setq start i)))

(defun %scroll-down-if-wrapping (window)
  (when (buffer-truncate-lines (window-buffer window))
    (let ((vtop-charpos (window-vtop-charpos window)))
      (setf (window-vtop-charpos window) 0)
      (map-wrapping-line (buffer-line-string (window-buffer window)
                                             (window-vtop-linum window))
                         (window-width window)
                         #'(lambda (c)
                             (when (< vtop-charpos c)
                               (setf (window-vtop-charpos window) c)
                               (return-from %scroll-down-if-wrapping t)))))
    nil))

(defun window-scroll-down (window)
  (unless (%scroll-down-if-wrapping window)
    (incf (window-vtop-linum window))))

(defun %scroll-up-if-wrapping (window)
  (when (and (buffer-truncate-lines (window-buffer window))
             (< 1 (window-vtop-linum window)))
    (let ((charpos-list))
      (map-wrapping-line (buffer-line-string
                          (window-buffer window)
                          (- (window-vtop-linum window)
                             (if (= 0 (window-vtop-charpos window)) 1 0)))
                         (window-width window)
                         #'(lambda (c)
                             (push c charpos-list)))
      (cond ((and charpos-list (= 0 (window-vtop-charpos window)))
             (decf (window-vtop-linum window))
             (setf (window-vtop-charpos window)
                   most-positive-fixnum)))
      (dolist (c charpos-list)
        (when (< c (window-vtop-charpos window))
          (setf (window-vtop-charpos window) c)
          (return-from %scroll-up-if-wrapping t)))
      (setf (window-vtop-charpos window) 0)
      (not (null charpos-list)))))

(defun window-scroll-up (window)
  (unless (%scroll-up-if-wrapping window)
    (decf (window-vtop-linum window))))

(defun window-scroll (window n)
  (dotimes (_ (abs n))
    (if (plusp n)
        (window-scroll-down window)
        (window-scroll-up window)))
  (when (< (window-vtop-linum window) 1)
    (setf (window-vtop-linum window) 1))
  (when (< (buffer-nlines (window-buffer window))
           (window-vtop-linum window))
    (setf (window-vtop-linum window)
          (buffer-nlines (window-buffer window)))))

(defun window-wrapping-offset (window)
  (unless (buffer-truncate-lines (window-buffer window))
    (return-from window-wrapping-offset 0))
  (let ((offset 0))
    (labels ((inc (arg)
                  (declare (ignore arg))
                  (incf offset))
             (f (string eof-p linum)
                (declare (ignore eof-p))
                (if (= linum (window-current-linum window))
                    (map-wrapping-line (subseq string
                                               0
                                               (min (length string)
                                                    (1+ (window-current-charpos window))))
                                       (window-width window)
                                       #'inc)
                    (map-wrapping-line (if (= (window-vtop-linum window) linum)
                                           (subseq string (window-vtop-charpos window))
                                           string)
                                       (window-width window)
                                       #'inc))))
      (map-buffer-lines #'f
                        (window-buffer window)
                        (window-vtop-linum window)
                        (window-current-linum window))
      offset)))

(defun window-cursor-y-not-wrapping (window)
  (- (window-current-linum window)
     (window-vtop-linum window)))

(defun window-cursor-y (window)
  (+ (window-cursor-y-not-wrapping window)
     (window-wrapping-offset window)))

(defun window-offset-view (window)
  (cond ((< (window-current-linum window)
            (window-vtop-linum window))
         (- (window-current-linum window)
            (window-vtop-linum window)))
        ((and (= (window-current-linum window)
                 (window-vtop-linum window))
              (< 0 (window-vtop-charpos window)))
         -1)
        ((let ((n (- (window-cursor-y window)
                     (- (window-height window) 2))))
           (when (< 0 n) n)))
        (t
         0)))

(defun window-see (window &optional (recenter *scroll-recenter-p*))
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
          (window-recenter window)
          (window-scroll window offset)))))

(defun split-window-after (new-window split-type)
  (let ((current-window (current-window)))
    (window-set-size current-window
                     (window-height (current-window))
                     (window-width (current-window)))
    (setf (window-vtop-linum new-window)
          (window-vtop-linum current-window))
    (setf (window-current-linum new-window)
          (window-current-linum current-window))
    (setf (window-current-charpos new-window)
          (window-current-charpos current-window))
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
    t))

(defun split-window-vertically (window)
  (unless (minibuffer-window-p window)
    (multiple-value-bind (winheight rem)
        (floor (window-height window) 2)
      (let ((newwin (make-window (window-buffer window)
                                 (window-x window)
                                 (+ (window-y window) winheight rem)
                                 (window-width window)
                                 winheight)))
        (decf (window-height window) winheight)
        (split-window-after newwin :vsplit)))))

(define-key *global-keymap* (kbd "C-x 2") 'split-active-window-vertically)
(define-command split-active-window-vertically () ()
  (split-window-vertically (current-window)))

(defun split-window-horizontally (window)
  (unless (minibuffer-window-active-p)
    (multiple-value-bind (winwidth rem)
        (floor (window-width window) 2)
      (let ((newwin (make-window (window-buffer window)
                                 (+ (window-x window)
                                    winwidth
                                    rem
                                    1)
                                 (window-y window)
                                 (1- winwidth)
                                 (window-height window))))
        (decf (window-width window) winwidth)
        (split-window-after newwin :hsplit)))))

(define-key *global-keymap* (kbd "C-x 3") 'split-active-window-horizontally)
(define-command split-active-window-horizontally () ()
  (split-window-horizontally (current-window)))

(defun split-window-sensibly (window)
  (if (< *window-sufficient-width* (window-width window))
      (split-window-horizontally window)
      (split-window-vertically window)))

(defun get-next-window (window &optional (window-list (window-list)))
  (let ((result (member window window-list)))
    (if (cdr result)
        (cadr result)
        (car window-list))))

(define-key *global-keymap* (kbd "C-x o") 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (let ((window-list
          (append (mklist (active-minibuffer-window))
                  (window-list))))
    (when (minusp n)
      (setf n (- (length window-list) (abs n))))
    (dotimes (_ n t)
      (setf (current-window)
            (get-next-window (current-window)
                             window-list)))))

(defun window-set-pos (window x y)
  (screen-set-pos (window-%screen window) x y)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-set-size (window winheight winwidth)
  (setf (window-height window) winheight)
  (setf (window-width window) winwidth)
  (screen-set-size (window-%screen window)
                   winwidth
                   winheight))

(defun window-move (window dy dx)
  (window-set-pos window
                  (+ (window-x window) dx)
                  (+ (window-y window) dy)))

(defun window-resize (window dl dc)
  (window-set-size window
                   (+ (window-height window) dl)
                   (+ (window-width window) dc)))

(define-key *global-keymap* (kbd "C-x 1") 'delete-other-windows)
(define-command delete-other-windows () ()
  (unless (minibuffer-window-active-p)
    (dolist (win (window-list))
      (unless (eq win (current-window))
        (delete-window win)))
    (window-set-pos (current-window) 0 0)
    (window-set-size (current-window)
                     (1- charms/ll:*lines*)
                     charms/ll:*cols*)
    t))

(defun adjust-size-windows-after-delete-window (deleted-window
                                                window-tree
                                                horizontal-p)
  (let ((window-list (window-tree-flatten window-tree)))
    (if horizontal-p
        (cond ((< (window-x deleted-window)
                  (window-x (car window-list)))
               (dolist (win (min-if #'window-x window-list))
                 (window-set-pos win
                                 (window-x deleted-window)
                                 (window-y win))
                 (window-set-size win
                                  (window-height win)
                                  (+ (window-width deleted-window)
                                     1
                                     (window-width win)))))
              (t
               (dolist (win (max-if #'window-x window-list))
                 (window-set-size win
                                  (window-height win)
                                  (+ (window-width deleted-window)
                                     1
                                     (window-width win))))))
        (cond ((< (window-y deleted-window)
                  (window-y (car window-list)))
               (dolist (win (min-if #'window-y window-list))
                 (window-set-pos win
                                 (window-x win)
                                 (window-y deleted-window))
                 (window-set-size win
                                  (+ (window-height deleted-window)
                                     (window-height win))
                                  (window-width win))))
              (t
               (dolist (win (max-if #'window-y window-list))
                 (window-set-size win
                                  (+ (window-height deleted-window)
                                     (window-height win))
                                  (window-width win))))))))

(defun delete-window (window)
  (when (or (one-window-p)
            (minibuffer-window-p window))
    (editor-error "Can not delete this window"))
  (when (eq (current-window) window)
    (other-window))
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
  (screen-delete (window-%screen window))
  t)

(define-key *global-keymap* (kbd "C-x 0") 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window (current-window)))

(defun pop-to-buffer (buffer)
  (check-switch-minibuffer-window)
  (if (eq buffer (current-buffer))
      (values (current-window) nil)
      (let ((split-p))
        (when (one-window-p)
          (setq split-p t)
          (split-window-sensibly (current-window)))
        (with-current-window (or (window-tree-find
                                  (window-tree)
                                  #'(lambda (window)
                                      (eq buffer (window-buffer window))))
                                 (get-next-window (current-window)))
          (set-buffer buffer)
          (values (current-window) split-p)))))

(defun display-buffer (buffer)
  (multiple-value-bind (window split-p)
      (pop-to-buffer buffer)
    (setf (window-parameter window :split-p) split-p)
    window))

(define-command quit-window (&optional window kill-buffer-p) ("P")
  (unless (window-p window)
    (setq window (current-window)))
  (cond
   ((window-parameter window :split-p)
    (when kill-buffer-p
      (kill-buffer (window-buffer window)))
    (delete-window window))
   (t
    (if kill-buffer-p
        (kill-buffer (window-buffer window))
        (set-buffer (bury-buffer (window-buffer window))
                    nil)))))

(defun collect-left-windows (window-list)
  (min-if #'window-x window-list))

(defun collect-right-windows (window-list)
  (max-if #'(lambda (window)
              (+ (window-x window)
                 (window-width window)))
          window-list))

(defun collect-top-windows (window-list)
  (min-if #'window-y window-list))

(defun collect-bottom-windows (window-list)
  (max-if #'(lambda (window)
              (+ (window-y window)
                 (window-height window)))
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
             (window-resize window 0 (- diff-width))))
          ((/= 0 diff-height)
           (dolist (window shrink-window-list)
             (window-resize window (- diff-height) 0))))
    (cond ((/= 0 shift-width)
           (dolist (window shrink-window-list)
             (window-move window 0 shift-width)))
          ((/= 0 shift-height)
           (dolist (window shrink-window-list)
             (window-move window shift-height 0)))))
  t)

(defun shrink-top-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-top-windows
                   #'(lambda (window)
                       (< 2 (window-height window)))
                   n 0 n 0))

(defun shrink-bottom-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-bottom-windows
                   #'(lambda (window)
                       (< 2 (window-height window)))
                   n 0 0 0))

(defun shrink-left-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-left-windows
                   #'(lambda (window)
                       (< 2 (window-width window)))
                   0 n 0 n))

(defun shrink-right-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-right-windows
                   #'(lambda (window)
                       (< 2 (window-width window)))
                   0 n 0 0))

(defun %grow-windows (window-list
                      collect-windows-fn
                      diff-height
                      diff-width
                      shift-height
                      shift-width)
  (dolist (window (funcall collect-windows-fn window-list))
    (cond ((/= 0 shift-width)
           (window-move window 0 shift-width))
          ((/= 0 shift-height)
           (window-move window shift-height 0)))
    (cond ((/= 0 diff-width)
           (window-resize window 0 diff-width))
          ((/= 0 diff-height)
           (window-resize window diff-height 0))))
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
  (if (< (window-y (car grow-window-list))
         (window-y (car shrink-window-list)))
      (and (shrink-top-windows shrink-window-list n)
           (grow-bottom-windows grow-window-list n))
      (and (shrink-bottom-windows shrink-window-list n)
           (grow-top-windows grow-window-list n))))

(defun grow-window-horizontally-internal
    (grow-window-list shrink-window-list n)
  (if (< (window-x (car grow-window-list))
         (window-x (car shrink-window-list)))
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

(define-key *global-keymap* (kbd "C-x ^") 'grow-window)
(define-command grow-window (n) ("p")
  (when (< n 0)
    (return-from grow-window (shrink-window (- n))))
  (when (one-window-p)
    (minibuf-print "Only one window")
    (return-from grow-window nil))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal x y n))
                           :vsplit))

(define-key *global-keymap* (kbd "C-x C-z") 'shrink-window)
(define-command shrink-window (n) ("p")
  (when (< n 0)
    (return-from shrink-window (grow-window (- n))))
  (when (one-window-p)
    (minibuf-print "Only one window")
    (return-from shrink-window nil))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-internal y x n))
                           :vsplit))

(define-key *global-keymap* (kbd "C-x }") 'grow-window-horizontally)
(define-command grow-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from grow-window-horizontally (shrink-window-horizontally (- n))))
  (when (one-window-p)
    (minibuf-print "Only one window")
    (return-from grow-window-horizontally nil))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal x y n))
                           :hsplit))

(define-key *global-keymap* (kbd "C-x {") 'shrink-window-horizontally)
(define-command shrink-window-horizontally (n) ("p")
  (when (< n 0)
    (return-from shrink-window-horizontally (grow-window-horizontally (- n))))
  (when (one-window-p)
    (minibuf-print "Only one window")
    (return-from shrink-window-horizontally nil))
  (resize-window-recursive (current-window) n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal y x n))
                           :hsplit))

(define-key *global-keymap* (kbd "C-down") 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
      (scroll-up (- n))
      (dotimes (_ n t)
        (when (= (window-cursor-y (current-window)) 0)
          (unless (forward-line n)
            (return nil)))
        (let ((prev-linum (window-vtop-linum (current-window)))
              (prev-charpos (window-vtop-charpos (current-window))))
          (window-scroll (current-window) 1)
          (when (and (= prev-linum (window-vtop-linum (current-window)))
                     (= prev-charpos (window-vtop-charpos (current-window))))
            (return nil))))))

(define-key *global-keymap* (kbd "C-up") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
      (scroll-down (- n))
      (dotimes (_ n t)
        (when (and (= (window-cursor-y (current-window))
                      (- (window-height (current-window)) 2))
                   (/= 1 (window-vtop-linum (current-window))))
          (unless (forward-line (- n))
            (return nil)))
        (let ((prev-linum (window-vtop-linum (current-window)))
              (prev-charpos (window-vtop-charpos (current-window))))
          (window-scroll (current-window) -1)
          (when (and (= prev-linum (window-vtop-linum (current-window)))
                     (= prev-charpos (window-vtop-charpos (current-window))))
            (return nil))))))
