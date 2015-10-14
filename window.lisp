;; -*- Mode: Lisp; Package: Lem -*-

(in-package :lem)

(export '(*modeline-default-format*
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
          deleted-window-p
          set-window-delete-hook
          recenter
          redraw-screen
          split-window
          get-next-window
          other-window
          delete-other-windows
          delete-current-window
          delete-window
          pop-to-buffer
          grow-window
          shrink-window
          scroll-down
          scroll-up))

(defvar *redraw-flags* '(:one-line :unnecessary :all))

(defvar *current-cols*)
(defvar *current-lines*)

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

(defun deleted-window-p (window)
  (not (window-tree-find *window-tree* window)))

(defun set-window-delete-hook (window fn)
  (setf (window-delete-hook window) fn))

(defun window-init ()
  (setq *current-cols* cl-charms/low-level:*cols*)
  (setq *current-lines* cl-charms/low-level:*lines*)
  (setq *current-window*
        (make-window (get-buffer-create "*scratch*")
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

(defun window-refresh-lines (window)
  (let ((curx 0)
        (cury (window-cursor-y window))
        (refresh-line
         (if (buffer-truncate-lines (window-buffer window))
             #'window-refresh-line-wrapping
             #'window-refresh-line)))
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
  (do-window-tree (win *window-tree*)
    (unless (eq win *current-window*)
      (window-update win)))
  (window-update *current-window*)
  (cl-charms/low-level:doupdate))

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

(defun window-require-update-one-line ()
  (let* ((window *current-window*))
    (multiple-value-bind (str curx cury)
        (%window-current-line window)
      (declare (ignore curx))
      (multiple-value-bind (curx2 cury2)
          (window-refresh-line-wrapping window 0 cury cury str)
        (if (= cury cury2)
            (cl-charms/low-level:wmove (window-win window) cury2 curx2)
            (window-update-all))))))

(defun window-require-update-cursor ()
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

(defun window-require-update ()
  (case (window-redraw-flag *current-window*)
    ((:one-line)
     (window-refresh-modeline *current-window*)
     (window-require-update-one-line))
    ((:unnecessary)
     (window-refresh-modeline *current-window*)
     (window-require-update-cursor))
    ((:all)
     (window-update-all))
    (otherwise
     (window-update-all)))
  (setf (window-redraw-flag *current-window*) nil))

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

(define-key *global-keymap* (kbd "C-x 2") 'split-window)
(define-command split-window () ()
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
    (adjust-size-windows-after-delete-window
     window
     (funcall another-getter)
     (eq (window-node-split-type node) :hsplit))
    (multiple-value-bind (node2 getter2 setter2)
        (window-tree-parent *window-tree* node)
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
               (or (find-if #'(lambda (window)
                                (eq buffer (window-buffer window)))
                            *window-list*)
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

(defun adjust-screen-size () ;***
  (dolist (win *window-list*)
    (window-set-size win
                     (window-nlines win)
                     cl-charms/low-level:*cols*))
  (dolist (win *window-list*)
    (when (<= cl-charms/low-level:*lines* (+ 2 (window-y win)))
      (delete-window win)))
  (let ((win (car (last *window-list*))))
    (window-set-size win
                     (+ (window-nlines win)
                        (- cl-charms/low-level:*lines* *current-lines*))
                     (window-ncols win)))
  (setq *current-cols* cl-charms/low-level:*cols*)
  (setq *current-lines* cl-charms/low-level:*lines*)
  (window-update-all))

(define-key *global-keymap* (kbd "C-x ^") 'grow-window)
(define-command grow-window (n) ("p") ;***
  (if (one-window-p)
      (progn
        (minibuf-print "Only one window")
        nil)
      (let* ((lowerwin (lower-window *current-window*))
             (upperwin (if lowerwin nil (upper-window *current-window*))))
        (if lowerwin
            (cond
             ((>= 1 (- (window-nlines lowerwin) n))
              (minibuf-print "Impossible change")
              nil)
             (t
              (window-resize *current-window* n 0)
              (window-resize lowerwin (- n) 0)
              (window-move lowerwin n 0)
              t))
            (cond
             ((>= 1 (- (window-nlines upperwin) n))
              (minibuf-print "Impossible change")
              nil)
             (t
              (window-resize *current-window* n 0)
              (window-move *current-window* (- n) 0)
              (window-resize upperwin (- n) 0)))))))

(define-key *global-keymap* (kbd "C-x C-z") 'shrink-window)
(define-command shrink-window (n) ("p") ;***
  (cond
   ((one-window-p)
    (minibuf-print "Only one window")
    nil)
   ((>= 1 (- (window-nlines *current-window*) n))
    (minibuf-print "Impossible change")
    nil)
   (t
    (let* ((lowerwin (lower-window *current-window*))
           (upperwin (if lowerwin nil (upper-window *current-window*))))
      (cond
       (lowerwin
        (window-resize *current-window* (- n) 0)
        (window-resize lowerwin (+ n) 0)
        (window-move lowerwin (- n) 0))
       (t
        (window-resize *current-window* (- n) 0)
        (window-move *current-window* n 0)
        (window-resize upperwin n 0)))))))

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
