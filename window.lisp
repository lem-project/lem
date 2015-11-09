;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(*modeline-default-format*
          *window-sufficient-width*
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
          one-window-p
          select-window
          selected-window
          deleted-window-p
          set-window-delete-hook
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

(defun window-list ()
  (window-tree-flatten
   *window-tree*))

(defun one-window-p ()
  (window-tree-leaf-p *window-tree*))

(defun select-window (window)
  (setq *current-window* window))

(defun selected-window (window)
  *current-window*)

(defun deleted-window-p (window)
  (not (window-tree-find *window-tree* window)))

(defun set-window-delete-hook (window fn)
  (setf (window-delete-hook window) fn))

(defun window-init ()
  (setq *current-cols* cl-charms/low-level:*cols*)
  (setq *current-lines* cl-charms/low-level:*lines*)
  (setq *current-window*
        (make-window (get-buffer-create "*tmp*")
                     (- cl-charms/low-level:*lines* 1)
                     cl-charms/low-level:*cols*
                     0
                     0))
  (setq *window-tree* *current-window*))

(define-key *global-keymap* (kbd "C-l") 'recenter)
(define-command recenter () ()
  (syntax-scan-window *current-window*)
  (do-window-tree (window *window-tree*)
    (cl-charms/low-level:clearok (window-win window) 1))
  (window-recenter *current-window*)
  (window-update-all)
  t)

(defun window-recenter (window)
  (setf (window-vtop-linum window)
        (window-cur-linum window))
  (window-scroll window (- (floor (window-nlines window) 2))))

(defun window-scroll (window n)
  (incf (window-vtop-linum window) n)
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
  (window-cur-col window))

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
          (format nil "~a~v,,,va ~a --" str n #\- #\space line-pos)))))

(defun window-refresh-modeline (window)
  (cl-charms/low-level:wattron (window-win window)
                               cl-charms/low-level:a_reverse)
  (let ((modeline-str (modeline-string window)))
    (cl-charms/low-level:mvwaddstr (window-win window)
                                   (1- (window-nlines window))
                                   0
                                   modeline-str))
  (cl-charms/low-level:wattroff (window-win window)
                                cl-charms/low-level:a_reverse))

(defun window-cursor-y (window)
  (- (window-cur-linum window)
     (window-vtop-linum window)))

(defun window-print-char (win y x str attr)
  (cl-charms/low-level:wattron win attr)
  (cl-charms/low-level:mvwaddstr win y (str-width str x) (string (schar str x)))
  (cl-charms/low-level:wattroff win attr))

(defun window-print-line (window y str)
  (check-type str fatstring)
  (loop
    :with x := 0 :and win := (window-win window)
    :for i :from 0 :below (fat-length str)
    :do (multiple-value-bind (char attr)
            (fat-char str i)
          (cl-charms/low-level:wattron win attr)
          (cl-charms/low-level:mvwaddstr win y x (string char))
          (cl-charms/low-level:wattroff win attr)
          (setq x (char-width char x)))))

(defun window-refresh-line (window curx cury y str)
  (check-type str fatstring)
  (when (= cury y)
    (setq curx (str-width (fat-string str) (window-cur-col window))))
  (let ((width (str-width (fat-string str)))
        (cols (window-ncols window)))
    (cond
     ((< width (window-ncols window))
      nil)
     ((or (/= cury y)
          (< curx (1- cols)))
      (let ((i (wide-index (fat-string str) cols)))
        (setq str
              (if (<= cols (str-width (fat-string str) i))
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

(defun window-refresh-line-wrapping (window curx cury y str)
  (check-type str fatstring)
  (let ((ncols (window-ncols window)))
    (when (= y cury)
      (setq curx (str-width (fat-string str) (window-cur-col window))))
    (let ((strings (divide-line-width str ncols)))
      (if (null (cdr strings))
          (window-print-line window y str)
          (do ((rest-strings strings (cdr rest-strings)))
              ((or (null rest-strings)
                   (>= y (1- (window-nlines window)))))
            (let ((str (car rest-strings))
                  (wrapping-flag (cdr rest-strings)))
              (when wrapping-flag
                (if (< y cury)
                    (incf cury)
                    (when (= y cury)
                      (let ((len (str-width (fat-string str))))
                        (when (< len curx)
                          (decf curx len)
                          (incf cury))))))
              (window-print-line window
                                 y
                                 (if wrapping-flag
				     (fat-concat str "\\")
                                     str))
              (when wrapping-flag
                (incf y)
                (push y (window-wrap-ylist window))))))))
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
    (cl-charms/low-level:wmove (window-win window)
                               cury
                               curx)))

(defun window-refresh-separator (window)
  (cl-charms/low-level:attron cl-charms/low-level:a_reverse)
  (when (< 0 (window-x window))
    (loop :with x := (- (window-x window) 1)
      :for y :from (window-y window) :repeat (window-nlines window) :do
      (cl-charms/low-level:mvwaddch cl-charms/low-level:*stdscr*
                                    y x #.(char-code #\|))))
  (cl-charms/low-level:attroff cl-charms/low-level:a_reverse)
  (cl-charms/low-level:wnoutrefresh cl-charms/low-level:*stdscr*))

(defun window-refresh (window)
  (window-refresh-modeline window)
  (window-refresh-lines window)
  (window-refresh-separator window)
  (cl-charms/low-level:wnoutrefresh (window-win window)))

(defun window-offset-view (window)
  (let ((vtop-linum (window-vtop-linum window))
        (nlines (- (window-nlines window)
                   (length (window-wrap-ylist window))))
        (linum (window-cur-linum window)))
    (cond
     ((< #1=(+ vtop-linum nlines -2) linum)
      (- linum #1#))
     ((> vtop-linum linum)
      (- linum vtop-linum))
     (t
      0))))

(defun window-adjust-view (window &optional (recenter *scroll-recenter-p*))
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
          (window-recenter window)
          (window-scroll window offset)))))

(defun window-update (window)
  (cl-charms/low-level:werase (window-win window))
  (window-adjust-view window)
  (window-refresh window))

(defun window-update-all ()
  (when *allow-interrupt-p*
    (sleep 0.0001))
  (let ((allow-interrupt-p *allow-interrupt-p*))
    (bt:with-lock-held (*editor-lock*)
      (setq *allow-interrupt-p* nil)
      (do-window-tree (win *window-tree*)
        (unless (eq win *current-window*)
          (window-update win)))
      (window-update *current-window*)
      (cl-charms/low-level:doupdate)
      )
    (setq *allow-interrupt-p* allow-interrupt-p)))

(defun redraw-screen ()
  (window-update-all))

(defun %window-current-line (window)
  (let* ((str (buffer-line-fatstring
               (window-buffer window)
               (window-cur-linum window)))
         (curx (str-width (fat-string str)
                          (window-cur-col)))
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
            (cl-charms/low-level:wmove (window-win window) cury2 curx2)
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
      (cl-charms/low-level:wmove (window-win window) cury curx))))

(defvar *brackets-overlays* nil)

(defun window-brackets-highlight ()
  (mapc #'delete-overlay *brackets-overlays*)
  (setq *brackets-overlays* nil)
  (let ((highlight-points))
    (when (eql #\( (following-char))
      (save-excursion
       (let ((orig-point (point)))
         (when (forward-sexp 1 t)
           (push (progn (prev-char 1) (point))
                 highlight-points)))))
    (when (eql #\) (preceding-char))
      (save-excursion
       (let ((orig-point (save-excursion
                          (prev-char 1)
                          (point))))
         (when (backward-sexp 1 t)
           (push (point) highlight-points)))))
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
  (setf (window-max-col new-window)
        (window-max-col))
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
  (let* ((window-list (window-list))
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
  (cl-charms/low-level:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-set-size (window nlines ncols)
  (cl-charms/low-level:wresize (window-win window) nlines ncols)
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
      (cl-charms/low-level:delwin (window-win win))))
  (setq *window-tree* *current-window*)
  (window-set-pos *current-window* 0 0)
  (window-set-size *current-window*
                   (1- cl-charms/low-level:*lines*)
                   cl-charms/low-level:*cols*)
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
  (cl-charms/low-level:delwin (window-win window))
  t)

(define-key *global-keymap* (kbd "C-x 0") 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window *current-window*))

(defun pop-to-buffer (buffer)
  (if (eq buffer (window-buffer))
      (values *current-window*
              (one-window-p))
      (let ((one-p (one-window-p)))
        (when one-p
          (split-window))
        (let ((*current-window*
               (or (window-tree-find
                    *window-tree*
                    #'(lambda (window)
                        (eq buffer (window-buffer window))))
                   (get-next-window *current-window*))))
          (set-buffer buffer)
          (values *current-window* one-p)))))

(defun popup (buffer output-function &key (goto-bob-p t) (erase-p t))
  (multiple-value-bind (*current-window* newwin-p)
      (pop-to-buffer buffer)
    (when erase-p
      (erase-buffer))
    (when output-function
      (with-open-stream (out (make-buffer-output-stream buffer))
        (funcall output-function out)))
    (when goto-bob-p
      (beginning-of-buffer))
    (values *current-window* newwin-p)))

(defun adjust-screen-size ()
  (let ((delete-windows))
    (do-window-tree (window *window-tree*)
      (when (<= cl-charms/low-level:*lines*
                (+ (window-y window) 2))
        (push window delete-windows))
      (when (<= cl-charms/low-level:*cols*
                (+ (window-x window) 1))
        (push window delete-windows)))
    (mapc #'delete-window delete-windows))
  (let ((window-list (window-tree-flatten *window-tree*)))
    (dolist (window (collect-right-windows window-list))
      (window-resize window
                     0
                     (- cl-charms/low-level:*cols*
                        *current-cols*)))
    (dolist (window (collect-bottom-windows window-list))
      (window-resize window
                     (- cl-charms/low-level:*lines*
                        *current-lines*)
                     0))
    (setq *current-cols* cl-charms/low-level:*cols*)
    (setq *current-lines* cl-charms/low-level:*lines*)
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

(define-key *global-keymap* (kbd "C-x C-n") 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
      (scroll-up (- n))
      (dotimes (_ n t)
        (when (= (window-cursor-y *current-window*) 0)
          (unless (next-line n)
            (return nil)))
        (let ((prev (window-vtop-linum)))
          (window-scroll *current-window* 1)
          (when (= prev (window-vtop-linum))
            (return nil))))))

(define-key *global-keymap* (kbd "C-x C-p") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
      (scroll-down (- n))
      (dotimes (_ n t)
        (when (and (= (window-cursor-y *current-window*)
                      (- (window-nlines) 2))
                   (/= 1 (window-vtop-linum)))
          (unless (prev-line n)
            (return nil)))
        (let ((prev (window-vtop-linum)))
          (window-scroll *current-window* (- 1))
          (when (= prev (window-vtop-linum))
            (return nil))))))
