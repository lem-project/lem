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
          window-nlines
          window-ncols
          window-y
          window-x
          window-buffer
          window-vtop-linum
          window-cur-linum
          window-cur-col
          window-delete-hook
          one-window-p
          select-window
          selected-window
          deleted-window-p
          recenter
          redraw-screen
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

(defvar *current-cols*)
(defvar *current-lines*)

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

(defvar *window-tree*)

(define-class window () *current-window*
  win
  nlines
  ncols
  y
  x
  buffer
  disp-lines
  vtop-linum
  vtop-column
  point-marker
  wrap-ylist
  redraw-flag
  delete-hook
  parameters)

(defun window-p (x)
  (typep x 'window))

(defun make-window (buffer nlines ncols y x)
  (let ((window
         (make-instance 'window
                        :win (charms/ll:newwin nlines ncols y x)
                        :nlines nlines
                        :ncols ncols
                        :y y
                        :x x
                        :buffer buffer
                        :disp-lines (make-array (1- nlines) :initial-element nil)
                        :vtop-linum 1
                        :vtop-column 0
                        :wrap-ylist nil)))
    (charms/ll:keypad (window-win window) 1)
    (setf (window-point-marker window)
          (make-marker (make-point 1 0) buffer))
    window))

(defun window-cur-col (&optional (window *current-window*))
  (marker-column (window-point-marker window)))

(defun (setf window-cur-col) (new-col &optional (window *current-window*))
  (setf (marker-column (window-point-marker window)) new-col))

(defun window-cur-linum (&optional (window *current-window*))
  (marker-linum (window-point-marker window)))

(defun (setf window-cur-linum) (new-linum &optional (window *current-window*))
  (setf (marker-linum (window-point-marker window)) new-linum))

(defun window-parameter (window parameter)
  (getf (window-parameters window) parameter))

(defun (setf window-parameter) (value window parameter)
  (setf (getf (window-parameters window) parameter) value))

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

(defmacro do-window-tree ((var tree) &body body)
  `(window-tree-map ,tree #'(lambda (,var) ,@body)))

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
   *window-tree*))

(defun one-window-p ()
  (window-tree-leaf-p *window-tree*))

(defun select-window (window)
  (setq *current-window* window))

(defun selected-window ()
  *current-window*)

(defun deleted-window-p (window)
  (not (window-tree-find *window-tree* window)))

(defun window-init ()
  (setq *current-cols* charms/ll:*cols*)
  (setq *current-lines* charms/ll:*lines*)
  (setq *current-window*
        (make-window (get-buffer-create "*tmp*")
                     (- charms/ll:*lines* 1)
                     charms/ll:*cols*
                     0
                     0))
  (setq *window-tree* *current-window*)
  (set-attr :modeline (get-attr :highlight))
  (set-attr :modeline-inactive (get-attr :highlight)))

(defun resize-screen ()
  (destructuring-bind (height width)
      (lem-winsize:win-size (xterm-fd))
    (charms/ll:resizeterm height width)))

(define-key *global-keymap* (kbd "C-l") 'recenter)
(define-command recenter () ()
  (when (run-xterm-p)
    (resize-screen))
  (adjust-screen-size)
  (do-window-tree (window *window-tree*)
    (charms/ll:clearok (window-win window) 1))
  (window-recenter *current-window*)
  (syntax-scan-window *current-window*)
  (window-update-all)
  t)

(defun window-recenter (window)
  (setf (window-vtop-column window) 0)
  (setf (window-vtop-linum window)
        (window-cur-linum window))
  (window-scroll window (- (floor (window-nlines window) 2))))

(defun %scroll-down-if-wrapping (window)
  (when (buffer-truncate-lines (window-buffer window))
    (let ((vtop-column (window-vtop-column window)))
      (setf (window-vtop-column window) 0)
      (map-wrapping-line (buffer-line-string (window-buffer window)
                                             (window-vtop-linum window))
                         (window-ncols window)
                         #'(lambda (c)
                             (when (< vtop-column c)
                               (setf (window-vtop-column window) c)
                               (return-from %scroll-down-if-wrapping t)))))
    nil))

(defun window-scroll-down (window)
  (unless (%scroll-down-if-wrapping window)
    (incf (window-vtop-linum window))))

(defun %scroll-up-if-wrapping (window)
  (when (and (buffer-truncate-lines (window-buffer window))
             (< 1 (window-vtop-linum window)))
    (let ((columns))
      (map-wrapping-line (buffer-line-string
                          (window-buffer window)
                          (- (window-vtop-linum window)
                             (if (= 0 (window-vtop-column window)) 1 0)))
                         (window-ncols window)
                         #'(lambda (c)
                             (push c columns)))
      (cond ((and columns (= 0 (window-vtop-column window)))
             (decf (window-vtop-linum window))
             (setf (window-vtop-column window)
                   most-positive-fixnum)))
      (dolist (c columns)
        (when (< c (window-vtop-column window))
          (setf (window-vtop-column window) c)
          (return-from %scroll-up-if-wrapping t)))
      (setf (window-vtop-column window) 0)
      (not (null columns)))))

(defun window-scroll-up (window)
  (unless (%scroll-up-if-wrapping window)
    (decf (window-vtop-linum window))))

(defun window-scroll (window n)
  (dotimes (_ (abs n))
    (if (plusp n)
        (window-scroll-down window)
        (window-scroll-up window)))
  (multiple-value-bind (outp offset)
      (head-line-p window (1+ (window-vtop-linum window)))
    (when outp
      (incf (window-vtop-linum window) offset)))
  (multiple-value-bind (outp offset)
      (tail-line-p window (window-vtop-linum window))
    (when outp
      (incf (window-vtop-linum window) offset))))

(defun window-posline (window)
  (cond
   ((<= (buffer-nlines (window-buffer window))
        (window-nlines window))
    "All")
   ((= 1 (window-vtop-linum window))
    "Top")
   ((<= (buffer-nlines (window-buffer window))
        (+ (window-vtop-linum window) (window-nlines window)))
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
  (window-cur-linum window))
(defun modeline-column (window)
  (str-width (buffer-line-string (window-buffer window)
                                 (window-cur-linum window))
             0
             (window-cur-col window)))

(defun modeline-string (window)
  (let* ((line-pos (window-posline window))
         (ncols (window-ncols window))
         (str (with-output-to-string (out)
                (dolist (x
                         (buffer-get (window-buffer window)
                                     :modeline-format
                                     *modeline-default-format*))
                  (if (or (symbolp x) (functionp x))
                      (princ (funcall x window) out)
                      (princ x out))))))
    (let ((n (- ncols 7 (length str))))
      (if (minusp n)
          (format nil "~a ~a --" str line-pos)
          (format nil "~a~v,,,va ~a --"
                  str
                  n
                  (if (eq window *current-window*) #\- #\space)
                  #\space
                  line-pos)))))

(defun window-refresh-modeline (window)
  (let ((attr
         (if (eq window *current-window*)
             (get-attr :modeline)
             (get-attr :modeline-inactive))))
    (charms/ll:wattron (window-win window) attr)
    (let ((modeline-str (modeline-string window)))
      (charms/ll:mvwaddstr (window-win window)
                           (1- (window-nlines window))
                           0
                           modeline-str))
    (charms/ll:wattroff (window-win window)
                        attr)))

(defun window-cursor-y (window)
  (- (window-cur-linum window)
     (window-vtop-linum window)))

(defun window-cursor-y-if-wrapping (window)
  (if (buffer-truncate-lines (window-buffer window))
      (+ (- (window-cur-linum window)
            (window-vtop-linum window))
         (window-wrapping-offset window))
      (window-cursor-y window)))

(defun window-print-line (window y str &key (start-x 0) (string-start 0) string-end)
  (check-type str fatstring)
  (loop
    :with x := start-x :and win := (window-win window) :and ctrl-attr := (get-attr :red)
    :for i :from string-start :below (or string-end (fat-length str))
    :do (multiple-value-bind (char attr)
            (fat-char str i)
          (when (ctrl-p char)
            (setq attr ctrl-attr))
          (charms/ll:wattron win attr)
          (charms/ll:mvwaddstr win y x (string char))
          (charms/ll:wattroff win attr)
          (setq x (char-width char x)))))

(defun window-refresh-line (window curx cury y str)
  (check-type str fatstring)
  (when (= cury y)
    (setq curx (str-width (fat-string str) 0 (window-cur-col window))))
  (let ((width (str-width (fat-string str)))
        (cols (window-ncols window)))
    (cond
     ((< width (window-ncols window))
      nil)
     ((or (/= cury y)
          (< curx (1- cols)))
      (let ((i (wide-index (fat-string str) cols)))
        (setq str
              (if (<= cols (str-width (fat-string str) 0 i))
                  (fat-concat (fat-substring str 0 (1- i)) " $")
                  (fat-concat (fat-substring str 0 i) "$")))))
     ((< (window-cur-col window) (fat-length str))
      (let* ((start (wide-index (fat-string str) (- curx cols -4)))
             (end (window-cur-col window))
             (substr (fat-substring str start end)))
        (setq curx (- cols 2))
        (if (wide-char-p (fat-char substr (- (fat-length substr) 1)))
            (progn
              (setq str
                    (fat-concat "$"
                                (fat-substring
                                 substr
                                 0 (1- (fat-length substr)))
                                " $"))
              (decf curx))
            (setq str (fat-concat "$" substr "$")))))
     (t
      (let ((start (- curx cols -3)))
        (setq str
              (fat-concat "$"
                          (fat-substring str
                                         (wide-index (fat-string str) start)))))
      (setq curx (- cols 1))))
    (window-print-line window y str))
  (values curx cury y))

(defun divide-line-width (str ncols)
  (check-type str fatstring)
  (labels ((f (str acc)
              (if (< (str-width (fat-string str)) ncols)
                  (nreverse (cons str acc))
                  (let ((i (wide-index (fat-string str) ncols)))
                    (f (fat-substring str i)
                       (cons (fat-substring str 0 i) acc))))))
    (f str nil)))

(defun map-wrapping-line (string ncols fn)
  (loop :with start := 0
    :for i := (wide-index string ncols :start start)
    :while i :do
    (funcall fn i)
    (setq start i)))

(defun window-wrapping-offset (window)
  (unless (buffer-truncate-lines (window-buffer window))
    (return-from window-wrapping-offset 0))
  (let ((offset 0))
    (labels ((f (string eof-p linum)
                (declare (ignore eof-p))
                (map-wrapping-line (if (= (window-vtop-linum window) linum)
                                       (subseq string (window-vtop-column window))
                                       string)
                                   (window-ncols window)
                                   #'(lambda (arg)
                                       (declare (ignore arg))
                                       (incf offset)))))
      (map-buffer-lines #'f
                        (window-buffer window)
                        (window-vtop-linum window)
                        (1- (window-cur-linum window)))
      offset)))

(defvar *wrapping-fatstring* (make-fatstring "!" 0))

(defun window-refresh-line-wrapping (window curx cury y str)
  (check-type str fatstring)
  (when (and (< 0 (window-vtop-column window)) (= y 0))
    (setq str (fat-substring str (window-vtop-column window))))
  (when (= y cury)
    (setq curx (str-width (fat-string str) 0 (window-cur-col window))))
  (loop :with start := 0 :and ncols := (window-ncols window)
    :for i := (wide-index (fat-string str) ncols :start start)
    :while (< y (1- (window-nlines window)))
    :do (cond ((null i)
               (window-print-line window y str :string-start start)
               (return))
              (t
               (cond ((< y cury)
                      (incf cury))
                     ((= y cury)
                      (let ((len (str-width (fat-string str) start i)))
                        (when (< len curx)
                          (decf curx len)
                          (incf cury)))))
               (window-print-line window y str :string-start start :string-end i)
               (window-print-line window y *wrapping-fatstring* :start-x (1- ncols))
               (incf y)
               (push y (window-wrap-ylist window))
               (setq start i))))
  (values curx cury y))

(defun get-window-refresh-line-function (window)
  (if (buffer-truncate-lines (window-buffer window))
      #'window-refresh-line-wrapping
      #'window-refresh-line))

(defun window-refresh-lines (window)
  (let ((curx 0)
        (cury (window-cursor-y window))
        (refresh-line
         (get-window-refresh-line-function window)))
    (setf (window-wrap-ylist window) nil)
    (loop
      :with y := 0
      :for str :across (buffer-display-lines
                        (window-buffer window)
                        (window-disp-lines window)
                        (window-vtop-linum window)
                        (1- (window-nlines window)))
      :while (< y (1- (window-nlines window))) :do
      (cond (str
             (check-type str fatstring)
             (multiple-value-setq (curx cury y)
                                  (funcall refresh-line
                                           window curx cury y str)))
            (t
             (return)))
      (incf y))
    (setf (window-wrap-ylist window)
          (nreverse (window-wrap-ylist window)))
    (charms/ll:wmove (window-win window)
                     cury
                     curx)))

(defun window-refresh-separator (window)
  (charms/ll:attron charms/ll:a_reverse)
  (when (< 0 (window-x window))
    (loop :with x := (- (window-x window) 1)
      :for y :from (window-y window) :repeat (window-nlines window) :do
      (charms/ll:mvwaddch charms/ll:*stdscr*
                          y x #.(char-code #\|))))
  (charms/ll:attroff charms/ll:a_reverse)
  (charms/ll:wnoutrefresh charms/ll:*stdscr*))

(defun window-refresh (window)
  (window-refresh-modeline window)
  (window-refresh-lines window)
  (window-refresh-separator window)
  (charms/ll:wnoutrefresh (window-win window)))

(defun window-offset-view (window)
  (cond ((< (window-cur-linum window)
            (window-vtop-linum window))
         (- (window-cur-linum window)
            (window-vtop-linum window)))
        ((and (= (window-cur-linum window)
                 (window-vtop-linum window))
              (< 0 (window-vtop-column window)))
         -1)
        ((let ((n (- (window-cursor-y-if-wrapping window)
                     (- (window-nlines window) 2))))
           (when (< 0 n) n)))
        (t
         0)))

(defun window-adjust-view (window &optional (recenter *scroll-recenter-p*))
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
          (window-recenter window)
          (window-scroll window offset)))))

(defun window-update (window &optional update-display-p)
  (cond
   ((eq *current-window* *minibuf-window*)
    (minibuf-window-update))
   (t
    (charms/ll:werase (window-win window))
    (window-adjust-view window)
    (window-refresh window)
    (when update-display-p
      (charms/ll:doupdate)))))

(defun window-update-all ()
  (bt:with-lock-held (*editor-lock*)
    (do-window-tree (win *window-tree*)
      (unless (eq win *current-window*)
        (window-update win)))
    (window-update *current-window*)
    (charms/ll:doupdate)))

(defun redraw-screen ()
  (window-update-all))

(defun %window-current-line (window)
  (let* ((str (buffer-line-fatstring
               (window-buffer window)
               (window-cur-linum window)))
         (curx (str-width (fat-string str) 0 (window-cur-col)))
         (cury (window-cursor-y window)))
    (dolist (y (window-wrap-ylist window))
      (when (<= y cury)
        (incf cury)))
    (values str curx cury)))

(defun window-maybe-update-one-line ()
  (let* ((window *current-window*))
    (multiple-value-bind (str curx cury)
        (%window-current-line window)
      (declare (ignore curx))
      (multiple-value-bind (curx2 cury2)
          (funcall (get-window-refresh-line-function window)
                   window 0 cury cury str)
        (if (= cury cury2)
            (charms/ll:wmove (window-win window) cury2 curx2)
            (window-update-all))))))

(defun window-maybe-update-cursor ()
  (let* ((window *current-window*)
         (ncols (window-ncols window)))
    (multiple-value-bind (str curx cury)
        (%window-current-line window)
      (when (<= (1- ncols) curx)
        (let ((strings (divide-line-width str ncols)))
          (loop :for str :in strings
            :while (<= (1- ncols) curx) :do
            (incf cury)
            (decf curx (str-width (fat-string str))))))
      (charms/ll:wmove (window-win window) cury curx))))

(defvar *brackets-overlays* nil)

(defun window-brackets-highlight ()
  (mapc #'delete-overlay *brackets-overlays*)
  (setq *brackets-overlays* nil)
  (let ((highlight-points))
    (when (eql #\( (following-char))
      (save-excursion
       (when (forward-sexp 1 t)
         (push (progn (prev-char 1) (point))
               highlight-points))))
    (when (eql #\) (preceding-char))
      (save-excursion
       (when (backward-sexp 1 t)
         (push (point) highlight-points))))
    (let ((attr (make-attr :color :cyan :reverse-p t)))
      (dolist (point highlight-points)
        (push (make-overlay point
                            (make-point (point-linum point)
                                        (1+ (point-column point)))
                            :attr attr)
              *brackets-overlays*))
      (if highlight-points t nil))))

(defun window-maybe-update ()
  (window-brackets-highlight)
  (window-update-all)
  (setf (window-redraw-flag) nil))

(defun split-window-after (new-window split-type)
  (window-set-size *current-window*
                   (window-nlines)
                   (window-ncols))
  (setf (window-vtop-linum new-window)
        (window-vtop-linum))
  (setf (window-cur-linum new-window)
        (window-cur-linum))
  (setf (window-cur-col new-window)
        (window-cur-col))
  (multiple-value-bind (node getter setter)
      (window-tree-parent *window-tree* *current-window*)
    (if (null node)
        (setq *window-tree*
              (make-window-node split-type
                                *current-window*
                                new-window))
        (funcall setter
                 (make-window-node split-type
                                   (funcall getter)
                                   new-window))))
  t)

(define-key *global-keymap* (kbd "C-x 2") 'split-window-vertically)
(define-command split-window-vertically () ()
  (when (eq *current-window* *minibuf-window*)
    (return-from split-window-vertically nil))
  (multiple-value-bind (nlines rem)
      (floor (window-nlines) 2)
    (let ((newwin (make-window
                   (window-buffer)
                   nlines
                   (window-ncols)
                   (+ (window-y)
                      nlines
                      rem)
                   (window-x))))
      (decf (window-nlines) nlines)
      (split-window-after newwin :vsplit))))

(define-key *global-keymap* (kbd "C-x 3") 'split-window-horizontally)
(define-command split-window-horizontally () ()
  (when (eq *current-window* *minibuf-window*)
    (return-from split-window-horizontally nil))
  (multiple-value-bind (ncols rem)
      (floor (window-ncols) 2)
    (let ((newwin (make-window
                   (window-buffer)
                   (window-nlines)
                   (1- ncols)
                   (window-y)
                   (+ (window-x)
                      ncols
                      rem
                      1))))
      (decf (window-ncols) ncols)
      (split-window-after newwin :hsplit))))

(defun split-window ()
  (if (< *window-sufficient-width* (window-ncols *current-window*))
      (split-window-horizontally)
      (split-window-vertically)))

(defun get-next-window (window)
  (let* ((window-list
          (if *minibuf-read-line-busy-p*
              (cons *minibuf-window* (window-list))
              (window-list)))
         (result (member window window-list)))
    (if (cdr result)
        (cadr result)
        (car window-list))))

(define-key *global-keymap* (kbd "C-x o") 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (dotimes (_ n)
    (setq *current-window*
          (get-next-window *current-window*)))
  (adjust-point)
  t)

(defun window-set-pos (window y x)
  (charms/ll:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-set-size (window nlines ncols)
  (charms/ll:wresize (window-win window) nlines ncols)
  (setf (window-nlines window) nlines)
  (setf (window-ncols window) ncols)
  (setf (window-disp-lines window)
        (make-array (1- nlines)
                    :initial-element nil)))

(defun window-move (window dy dx)
  (window-set-pos window
                  (+ (window-y window) dy)
                  (+ (window-x window) dx)))

(defun window-resize (window dl dc)
  (window-set-size window
                   (+ (window-nlines window) dl)
                   (+ (window-ncols window) dc)))

(define-key *global-keymap* (kbd "C-x 1") 'delete-other-windows)
(define-command delete-other-windows () ()
  (do-window-tree (win *window-tree*)
    (unless (eq win *current-window*)
      (charms/ll:delwin (window-win win))))
  (setq *window-tree* *current-window*)
  (window-set-pos *current-window* 0 0)
  (window-set-size *current-window*
                   (1- charms/ll:*lines*)
                   charms/ll:*cols*)
  t)

(defun adjust-size-windows-after-delete-window (deleted-window
                                                window-tree
                                                horizontal-p)
  (let ((window-list (window-tree-flatten window-tree)))
    (if horizontal-p
        (cond ((< (window-x deleted-window)
                  (window-x (car window-list)))
               (dolist (win (min-if #'window-x window-list))
                 (window-set-pos win
                                 (window-y win)
                                 (window-x deleted-window))
                 (window-set-size win
                                  (window-nlines win)
                                  (+ (window-ncols deleted-window)
                                     1
                                     (window-ncols win)))))
              (t
               (dolist (win (max-if #'window-x window-list))
                 (window-set-size win
                                  (window-nlines win)
                                  (+ (window-ncols deleted-window)
                                     1
                                     (window-ncols win))))))
        (cond ((< (window-y deleted-window)
                  (window-y (car window-list)))
               (dolist (win (min-if #'window-y window-list))
                 (window-set-pos win
                                 (window-y deleted-window)
                                 (window-x win))
                 (window-set-size win
                                  (+ (window-nlines deleted-window)
                                     (window-nlines win))
                                  (window-ncols win))))
              (t
               (dolist (win (max-if #'window-y window-list))
                 (window-set-size win
                                  (+ (window-nlines deleted-window)
                                     (window-nlines win))
                                  (window-ncols win))))))))

(defun delete-window (window)
  (when (one-window-p)
    (minibuf-print "Can not delete this window")
    (return-from delete-window nil))
  (when (eq *current-window* window)
    (other-window))
  (multiple-value-bind (node getter setter another-getter another-setter)
      (window-tree-parent *window-tree* window)
    (declare (ignore getter setter another-setter))
    (adjust-size-windows-after-delete-window
     window
     (funcall another-getter)
     (eq (window-node-split-type node) :hsplit))
    (multiple-value-bind (node2 getter2 setter2)
        (window-tree-parent *window-tree* node)
      (declare (ignore getter2))
      (if (null node2)
          (setq *window-tree* (funcall another-getter))
          (funcall setter2 (funcall another-getter)))))
  (when (window-delete-hook window)
    (funcall (window-delete-hook window)))
  (charms/ll:delwin (window-win window))
  t)

(define-key *global-keymap* (kbd "C-x 0") 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window *current-window*))

(defun pop-to-buffer (buffer)
  (if (eq buffer (window-buffer))
      (values *current-window* nil)
      (let ((split-p))
        (when (one-window-p)
          (setq split-p t)
          (split-window))
        (let ((*current-window*
               (or (window-tree-find
                    *window-tree*
                    #'(lambda (window)
                        (eq buffer (window-buffer window))))
                   (get-next-window *current-window*))))
          (set-buffer buffer)
          (values *current-window* split-p)))))

(defun display-buffer (buffer)
  (multiple-value-bind (window split-p)
      (pop-to-buffer buffer)
    (setf (window-parameter window :split-p) split-p)
    window))

(define-command quit-window (&optional window kill-buffer-p) ("P")
  (unless window
    (setq window *current-window*))
  (cond
   ((window-parameter window :split-p)
    (when kill-buffer-p
      (kill-buffer (window-buffer window)))
    (delete-window window))
   (t
    (if kill-buffer-p
        (kill-buffer (window-buffer window))
        (bury-buffer (window-buffer window))))))

(defun adjust-screen-size ()
  (let ((delete-windows))
    (do-window-tree (window *window-tree*)
      (when (<= charms/ll:*lines*
                (+ (window-y window) 2))
        (push window delete-windows))
      (when (<= charms/ll:*cols*
                (+ (window-x window) 1))
        (push window delete-windows)))
    (mapc #'delete-window delete-windows))
  (let ((window-list (window-tree-flatten *window-tree*)))
    (dolist (window (collect-right-windows window-list))
      (window-resize window
                     0
                     (- charms/ll:*cols*
                        *current-cols*)))
    (dolist (window (collect-bottom-windows window-list))
      (window-resize window
                     (- charms/ll:*lines*
                        *current-lines*)
                     0))
    (setq *current-cols* charms/ll:*cols*)
    (setq *current-lines* charms/ll:*lines*)
    (window-update-all)))

(defun collect-left-windows (window-list)
  (min-if #'window-x window-list))

(defun collect-right-windows (window-list)
  (max-if #'(lambda (window)
              (+ (window-x window)
                 (window-ncols window)))
          window-list))

(defun collect-top-windows (window-list)
  (min-if #'window-y window-list))

(defun collect-bottom-windows (window-list)
  (max-if #'(lambda (window)
              (+ (window-y window)
                 (window-nlines window)))
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
                       (< 2 (window-nlines window)))
                   n 0 n 0))

(defun shrink-bottom-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-bottom-windows
                   #'(lambda (window)
                       (< 2 (window-nlines window)))
                   n 0 0 0))

(defun shrink-left-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-left-windows
                   #'(lambda (window)
                       (< 2 (window-ncols window)))
                   0 n 0 n))

(defun shrink-right-windows (window-list n)
  (%shrink-windows window-list
                   #'collect-right-windows
                   #'(lambda (window)
                       (< 2 (window-ncols window)))
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
      (window-tree-parent *window-tree* node)
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
  (resize-window-recursive *current-window* n
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
  (resize-window-recursive *current-window* n
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
  (resize-window-recursive *current-window* n
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
  (resize-window-recursive *current-window* n
                           #'(lambda (x y n)
                               (grow-window-horizontally-internal y x n))
                           :hsplit))

(define-key *global-keymap* (kbd "C-down") 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
      (scroll-up (- n))
      (dotimes (_ n t)
        (when (= (window-cursor-y-if-wrapping *current-window*) 0)
          (unless (next-line n)
            (return nil)))
        (let ((prev-linum (window-vtop-linum))
              (prev-column (window-vtop-column)))
          (window-scroll *current-window* 1)
          (when (and (= prev-linum (window-vtop-linum))
                     (= prev-column (window-vtop-column)))
            (return nil))))))

(define-key *global-keymap* (kbd "C-up") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
      (scroll-down (- n))
      (dotimes (_ n t)
        (when (and (= (window-cursor-y-if-wrapping *current-window*)
                      (- (window-nlines) 2))
                   (/= 1 (window-vtop-linum)))
          (unless (prev-line n)
            (return nil)))
        (let ((prev-linum (window-vtop-linum))
              (prev-column (window-vtop-column)))
          (window-scroll *current-window* -1)
          (when (and (= prev-linum (window-vtop-linum))
                     (= prev-column (window-vtop-column)))
            (return nil))))))
