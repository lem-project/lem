(in-package :lem)

(define-class window () *current-window*
  win
  nlines
  ncols
  y
  x
  buffer
  display-lines
  vtop-linum
  cur-linum
  cur-col
  max-col)

(defun make-window (buffer nlines ncols y x)
  (let ((window
         (make-instance 'window
                        :win (cl-ncurses:newwin nlines ncols y x)
                        :nlines nlines
                        :ncols ncols
                        :y y
                        :x x
                        :buffer buffer
                        :display-lines (make-array nlines :initial-element nil)
                        :vtop-linum 1
                        :cur-linum 1
                        :cur-col 0
                        :max-col 0)))
    window))

(defvar *current-cols*)
(defvar *current-lines*)
(defvar *window-force-update* nil)

(defun one-window-p ()
  (null (cdr *window-list*)))

(defun window-init ()
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (setq *current-window*
        (make-window (get-buffer-create "*tmp*")
                     (- cl-ncurses:*lines* 1)
                     cl-ncurses:*cols*
                     0
                     0))
  (setq *window-list* (list *current-window*)))

(define-key *global-keymap* "C-l" 'recenter)
(define-command recenter () ()
  (window-recenter *current-window*)
  (window-update-all t)
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

(defun window-refresh-modeline-1 (window bg-char)
  (let ((str
         (format nil
                 (format nil
                         "~~~d,,,'~ca ~a ~a"
                         (- (window-ncols window) 7)
                         bg-char
                         (window-posline window)
                         (make-string 2 :initial-element bg-char))
                 (format nil "~c~c ~a: ~a (~a) (~d, ~d)"
                         (if (buffer-read-only-p (window-buffer window)) #\% bg-char)
                         (if (buffer-modified-p (window-buffer window)) #\* bg-char)
                         *program-name*
                         (buffer-name (window-buffer window))
                         (let ((*current-window* window))
                           (mode-name))
                         (window-cur-linum window)
                         (window-cur-col window)))))
    (window-update-line
     window
     (1- (window-nlines window))
     str
     str)))

(defun window-refresh-modeline (window)
  (cl-ncurses:wattron (window-win window) cl-ncurses:a_reverse)
  (window-refresh-modeline-1 window #\-)
  (cl-ncurses:wattroff (window-win window) cl-ncurses:a_reverse))

(defun window-cursor-y (window)
  (- (window-cur-linum window)
     (window-vtop-linum window)))

(defun window-update-line-p (window y str)
  (or *window-force-update*
      (null (aref (window-display-lines window) y))
      (string/= str (aref (window-display-lines window) y))))

(defun display-detab (str)
  (loop with i = 0 and chars = ()
        for c across str do
        (cond ((char= c #\tab)
               (let ((j (char-width c i)))
                 (dotimes (_ (- j i))
                   (push #\space chars))
                 (setq i j)))
              (t
               (incf i)
               (push c chars)))
        finally (return
                  (coerce (nreverse chars)
                          'string))))

(defun window-update-line (window y cache-str display-str)
  (when (window-update-line-p window y cache-str)
    (setf (aref (window-display-lines window) y) cache-str)
    (setq display-str (display-detab display-str))
    (cl-ncurses:mvwaddstr
     (window-win window) y 0
     (concatenate 'string
                  display-str
                  (make-string (- (window-ncols window) (length display-str))
                               :initial-element #\space)))
    (cl-ncurses:touchline (window-win) y 1)))

(defun window-refresh-line (window str y)
  (let ((cury (window-cursor-y window))
        (curx)
        (oldstr str))
    (when (= cury y)
      (setq curx (str-width str (window-cur-col window))))
    (when (window-update-line-p window y oldstr)
      (let ((width (str-width str))
            (cols (window-ncols window)))
        (cond
         ((< width (window-ncols window))
          nil)
         ((or (/= cury y)
              (< curx (1- cols)))
          (let ((i (wide-index str cols)))
            (setq str
                  (if (<= cols (str-width str i))
                    (format nil "~a $" (subseq str 0 (1- i)))
                    (format nil "~a$" (subseq str 0 i))))))
         ((< (window-cur-col window) (length str))
          (let* ((begin (wide-index str (- curx cols -4)))
                 (end (1+ (window-cur-col window)))
                 (substr (subseq str begin end)))
            (setq curx (- cols 2))
            (if (wide-char-p (aref substr (- (length substr) 1)))
              (progn
                (setq str
                      (format nil "$~a $"
                              (subseq substr 0 (1- (length substr)))))
                (decf curx))
              (setq str (format nil "$~a$" substr)))))
         (t
          (setq str
                (format nil
                        "$~a"
                        (substring-width str (- curx cols -3))))
          (setq curx (- cols 1))))
        (window-update-line window y oldstr str)))
    curx))

(defun window-refresh-lines (window)
  (let (x)
    (do ((lines
           (buffer-take-lines (window-buffer window)
                               (window-vtop-linum window)
                               (1- (window-nlines window)))
           (cdr lines))
         (y 0 (1+ y)))
        ((<= (1- (window-nlines window)) y))
      (if (null lines)
        (window-update-line window y "" "")
        (let ((curx (window-refresh-line window (car lines) y)))
          (when curx
            (setq x curx)))))
    (cl-ncurses:wmove (window-win window)
                      (- (window-cur-linum window) (window-vtop-linum window))
                      x)))

(defun window-refresh (window)
  (window-refresh-modeline window)
  (window-refresh-lines window)
  (cl-ncurses:wnoutrefresh (window-win window)))

(defun window-offset-view (window)
  (let ((vtop-linum (window-vtop-linum window))
	(nlines (window-nlines window))
	(linum (window-cur-linum window)))
    (cond
      ((< #1=(+ vtop-linum nlines -2) linum)
	(- linum #1#))
      ((> vtop-linum linum)
	(- linum vtop-linum))
      (t
	0))))

(defun window-adjust-view (window recenter)
  (let ((offset (window-offset-view window)))
    (unless (zerop offset)
      (if recenter
        (window-recenter window)
	(window-scroll window offset)))))

(defun window-update-1 (window)
  (window-adjust-view window t)
  (window-refresh window))

(defun window-update (window &optional force)
  (let ((*window-force-update* force))
    (window-update-1 window)
    (when force
      (cl-ncurses:wrefresh (window-win window)))))

(defun window-update-all (&optional force)
  (let ((*window-force-update* force))
    (dolist (win *window-list*)
      (unless (eq win *current-window*)
        (window-update win)))
    (window-update-1 *current-window*)
    (cl-ncurses:doupdate)))

(define-key *global-keymap* "C-x2" 'split-window)
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
      (cl-ncurses:wresize
       (window-win)
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
          (lambda (b1 b2)
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

(define-key *global-keymap* "C-xo" 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (setq *current-window*
      (get-next-window *current-window*))))

(defun window-set-pos (window y x)
  (cl-ncurses:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-set-size (window nlines ncols)
  (cl-ncurses:wresize (window-win window) nlines ncols)
  (setf (window-nlines window) nlines)
  (setf (window-ncols window) ncols)
  (setf (window-display-lines window)
        (make-array nlines :initial-element nil)))

(defun window-move (window dy dx)
  (window-set-pos window
    (+ (window-y window) dy)
    (+ (window-x window) dx)))

(defun window-resize (window dl dc)
  (window-set-size window
    (+ (window-nlines window) dl)
    (+ (window-ncols window) dc)))

(define-key *global-keymap* "C-x1" 'delete-other-windows)
(define-command delete-other-windows () ()
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (cl-ncurses:delwin (window-win win))))
  (setq *window-list* (list *current-window*))
  (window-set-pos *current-window* 0 0)
  (window-set-size *current-window*
    (1- cl-ncurses:*lines*)
    cl-ncurses:*cols*)
  t)

(define-key *global-keymap* "C-x0" 'delete-window)
(define-command delete-window () ()
  (delete-window-1 *current-window*))

(defun delete-window-1 (window)
  (cond
   ((one-window-p)
    (write-message "Can not delete this window")
    nil)
   (t
    (when (eq *current-window* window)
      (other-window))
    (cl-ncurses:delwin (window-win window))
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
      cl-ncurses:*cols*))
  (dolist (win *window-list*)
    (when (<= cl-ncurses:*lines* (+ 2 (window-y win)))
      (delete-window-1 win)))
  (let ((win (car (last *window-list*))))
    (window-set-size win
      (+ (window-nlines win)
        (- cl-ncurses:*lines* *current-lines*))
      (window-ncols win)))
  (setq *current-cols* cl-ncurses:*cols*)
  (setq *current-lines* cl-ncurses:*lines*)
  (window-update-all))

(defun pop-to-buffer (buffer)
  (when (one-window-p)
    (values (split-window) t))
  (let ((*current-window*
         (or (find-if (lambda (window)
                        (eq buffer (window-buffer window)))
                      *window-list*)
             (get-next-window *current-window*))))
    (set-buffer buffer)
    (values *current-window* nil)))

(defun popup (buffer fn &optional (goto-bob-p t) (erase-p t))
  (let ((*current-window* (pop-to-buffer buffer)))
    (when erase-p
      (erase-buffer))
    (funcall fn)
    (when goto-bob-p
      (beginning-of-buffer))))

(defun popup-string (buffer string)
  (popup buffer
         (lambda ()
           (insert-string string))
         t
         t))

(define-key *global-keymap* "C-x^" 'grow-window)
(define-command grow-window (n) ("p")
  (if (one-window-p)
    (progn
     (write-message "Only one window")
     nil)
    (let* ((lowerwin (lower-window *current-window*))
           (upperwin (if lowerwin nil (upper-window *current-window*))))
      (if lowerwin
        (cond
         ((>= 1 (- (window-nlines lowerwin) n))
          (write-message "Impossible change")
          nil)
         (t
          (window-resize *current-window* n 0)
          (window-resize lowerwin (- n) 0)
          (window-move lowerwin n 0)
          t))
        (cond
         ((>= 1 (- (window-nlines upperwin) n))
          (write-message "Impossible change")
          nil)
         (t
          (window-resize *current-window* n 0)
          (window-move *current-window* (- n) 0)
          (window-resize upperwin (- n) 0)))))))

(define-key *global-keymap* "C-xC-z" 'shrink-window)
(define-command shrink-window (n) ("p")
  (cond
   ((one-window-p)
    (write-message "Only one window")
    nil)
   ((>= 1 (- (window-nlines *current-window*) n))
    (write-message "Impossible change")
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

(define-key *global-keymap* "C-xC-n" 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
    (scroll-up (- n))
    (dotimes (_ n t)
      (when (= (window-cursor-y *current-window*) 0)
        (next-line n))
      (window-scroll *current-window* 1))))

(define-key *global-keymap* "C-xC-p" 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
    (scroll-down (- n))
    (dotimes (_ n t)
      (when (= (window-cursor-y *current-window*)
               (- (window-nlines) 2))
        (prev-line 1))
      (window-scroll *current-window* (- 1)))))
