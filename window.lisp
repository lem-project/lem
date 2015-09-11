(in-package :lem)

(export '(window
          one-window-p
          recenter
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
  wrap-count)

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
                        :wrap-count 0)))
    (cl-charms/low-level:keypad (window-win window) 1)
    window))

(defvar *current-cols*)
(defvar *current-lines*)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-charms/low-level:*cols*)
  (setq *current-lines* cl-charms/low-level:*lines*)
  (setq *current-window*
        (make-window (get-buffer-create "*scratch*")
                     (- cl-charms/low-level:*lines* 1)
                     cl-charms/low-level:*cols*
                     0
                     0))
  (setq *window-list* (list *current-window*)))

(define-key *global-keymap* (kbd "C-l") 'recenter)
(define-command recenter () ()
  (syntax-scan-window *current-window*)
  (dolist (window *window-list*)
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

(defun modeline-string (bg-char
                        ncols
                        ronly-p
                        modif-p
                        program-name
                        buffer-name
                        modes
                        linum
                        column
                        line-pos)
  (let ((str (format nil "~C~C ~A: ~A ~A (~D, ~D)"
                     (if ronly-p #\% #\-)
                     (if modif-p #\* #\-)
                     program-name
                     buffer-name
                     modes
                     linum
                     column)))
    (let ((n (- ncols 7 (length str))))
      (if (minusp n)
          (format nil "~A ~A ~C~C" str line-pos bg-char bg-char)
          (format nil "~A~V,,,VA ~A ~C~C" str n bg-char #\space
                  line-pos bg-char bg-char)))))

(defun window-refresh-modeline (window)
  (cl-charms/low-level:wattron (window-win window)
                               cl-charms/low-level:a_reverse)
  (let ((modeline-str
         (modeline-string (if (eq window *current-window*) #\- #\space)
                          (window-ncols window)
                          (buffer-read-only-p (window-buffer window))
                          (buffer-modified-p (window-buffer window))
                          *program-name*
                          (buffer-name (window-buffer window))
                          (let ((*current-window* window))
                            (mapcar 'mode-name
                                    (cons (major-mode)
                                          (buffer-minor-modes
                                           (window-buffer)))))
                          (window-cur-linum window)
                          (str-width (buffer-line-string
                                      (window-buffer window)
                                      (window-cur-linum window))
                                     (window-cur-col window))
                          (window-posline window))))
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
                (incf (window-wrap-count window))))))))
  (values curx cury y))

(defun window-refresh-lines (window)
  (let ((curx 0)
        (cury (window-cursor-y window))
        (refresh-line
         (if (buffer-truncate-lines (window-buffer window))
             #'window-refresh-line-wrapping
             #'window-refresh-line)))
    (setf (window-wrap-count window) 0)
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
    (cl-charms/low-level:wmove (window-win window)
                               cury
                               curx)))

(defun window-refresh (window)
  (window-refresh-modeline window)
  (window-refresh-lines window)
  (cl-charms/low-level:wnoutrefresh (window-win window)))

(defun window-offset-view (window)
  (let ((vtop-linum (window-vtop-linum window))
        (nlines (- (window-nlines window)
                   (window-wrap-count window)))
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
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (window-update win)))
  (window-update *current-window*)
  (cl-charms/low-level:doupdate))

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
      (window-set-size *current-window*
                       (window-nlines)
                       (window-ncols))
      (setf (window-vtop-linum newwin)
            (window-vtop-linum))
      (setf (window-cur-linum newwin)
            (window-cur-linum))
      (setf (window-cur-col newwin)
            (window-cur-col))
      (setf (window-max-col newwin)
            (window-max-col))
      (setq *window-list*
            (sort (copy-list (append *window-list* (list newwin)))
                  #'(lambda (b1 b2)
                      (< (window-y b1) (window-y b2)))))))
  t)

(defun get-next-window (window)
  (let ((result (member window *window-list*)))
    (if (cdr result)
        (cadr result)
        (car *window-list*))))

(defun upper-window (window)
  (unless (one-window-p)
    (do ((prev *window-list* (cdr prev)))
        ((null (cdr prev)))
      (when (eq (cadr prev) window)
        (return (car prev))))))

(defun lower-window (window)
  (cadr (member window *window-list*)))

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
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (cl-charms/low-level:delwin (window-win win))))
  (setq *window-list* (list *current-window*))
  (window-set-pos *current-window* 0 0)
  (window-set-size *current-window*
                   (1- cl-charms/low-level:*lines*)
                   cl-charms/low-level:*cols*)
  t)

(define-key *global-keymap* (kbd "C-x 0") 'delete-current-window)
(define-command delete-current-window () ()
  (delete-window *current-window*))

(defun delete-window (window)
  (cond
   ((one-window-p)
    (minibuf-print "Can not delete this window")
    nil)
   (t
    (when (eq *current-window* window)
      (other-window))
    (cl-charms/low-level:delwin (window-win window))
    (let ((wlist (reverse *window-list*)))
      (let ((upwin (cadr (member window wlist))))
        (when (null upwin)
          (setq upwin (cadr *window-list*))
          (window-set-pos upwin 0 (window-x upwin)))
        (window-set-size upwin
                         (+ (window-nlines upwin)
                            (window-nlines window))
                         (window-ncols upwin))))
    (setq *window-list* (delete window *window-list*))
    t)))

(defun adjust-screen-size ()
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

(defun pop-to-buffer (buffer)
  (let ((one-p (one-window-p)))
    (when one-p
      (split-window))
    (let ((*current-window*
           (or (find-if #'(lambda (window)
                            (eq buffer (window-buffer window)))
                        *window-list*)
               (get-next-window *current-window*))))
      (set-buffer buffer)
      (values *current-window* one-p))))

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

(define-key *global-keymap* (kbd "C-x ^") 'grow-window)
(define-command grow-window (n) ("p")
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
(define-command shrink-window (n) ("p")
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
          (next-line n))
        (window-scroll *current-window* 1))))

(define-key *global-keymap* (kbd "C-x C-p") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
      (scroll-down (- n))
      (dotimes (_ n t)
        (when (and (= (window-cursor-y *current-window*)
                      (- (window-nlines) 2))
                   (/= 1 (window-vtop-linum)))
          (prev-line n))
        (window-scroll *current-window* (- 1)))))
