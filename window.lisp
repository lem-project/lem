(in-package :lem)

(export '(one-window-p
          recenter
          split-window
          get-next-window
          other-window
          delete-other-windows
          delete-current-window
          delete-window
          pop-to-buffer
          popup-string
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
  vtop-linum
  cur-linum
  cur-col
  max-col)

(defun make-window (buffer nlines ncols y x)
  (let ((window
         (make-instance 'window
                        :win (cl-charms/low-level:newwin nlines ncols y x)
                        :nlines nlines
                        :ncols ncols
                        :y y
                        :x x
                        :buffer buffer
                        :vtop-linum 1
                        :cur-linum 1
                        :cur-col 0
                        :max-col 0)))
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

(defun modeline-string (bg-char ncols
                        ronly-p
                        modif-p
                        program-name
                        buffer-name
                        modes
                        linum
                        column
                        line-pos)
  (let ((str (format nil "~C~C ~A:~A ~A (~D, ~D)"
                     (if ronly-p #\% #\-)
                     (if modif-p #\* #\-)
                     program-name
                     buffer-name
                     modes
                     linum
                     column)))
    (format nil "~a~v,,,va ~a ~c~c"
            str
            (- ncols 7 (length str))
            bg-char
            #\space
            line-pos
            bg-char
            bg-char)))

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
                          (window-cur-col window)
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

(defun window-print-line (window y str props offset-column)
  (let ((x 0))
    (loop for (pos . prop) in props do
      (when (eq prop :highlight)
        (cl-charms/low-level:mvwinsstr (window-win window) y x (subseq str x pos))
        (cl-charms/low-level:wattron (window-win window) cl-charms/low-level:a_reverse)
        (cl-charms/low-level:mvwinsch (window-win window) y pos (char-code (schar str pos)))
        (cl-charms/low-level:wattroff (window-win window) cl-charms/low-level:a_reverse)
        (setq x (1+ pos))))
    (let ((rest-str (subseq str x)))
      (cl-charms/low-level:mvwinsstr (window-win window) y x
                                     (concatenate 'string
                                                  rest-str
                                                  (make-string (- (window-ncols window)
                                                                  (str-width rest-str))
                                                               :initial-element #\space))))))

(defun window-refresh-line (window y str props)
  (let ((cury (window-cursor-y window))
        (curx)
        (offset-column 0))
    (when (= cury y)
      (setq curx (str-width str (window-cur-col window))))
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
        (let* ((start (wide-index str (- curx cols -4)))
               (end (window-cur-col window))
               (substr (subseq str start end)))
          (setq offset-column start)
          (setq curx (- cols 2))
          (if (wide-char-p (aref substr (- (length substr) 1)))
              (progn
                (setq str
                      (format nil "$~a $"
                              (subseq substr 0 (1- (length substr)))))
                (decf curx))
              (setq str (format nil "$~a$" substr)))))
       (t
        (let ((start (- curx cols -3)))
          (setq offset-column start)
          (setq str
                (format nil
                        "$~a"
                        (substring-width str start))))
        (setq curx (- cols 1))))
      (window-print-line window y str props offset-column))
    curx))

(defun window-refresh-lines (window)
  (let* ((x 0)
         (buffer (window-buffer window))
         (win-nlines (1- (window-nlines window)))
         (buf-nlines (buffer-nlines buffer)))
    (do ((linum (window-vtop-linum window) (1+ linum))
         (y 0 (1+ y)))
        ((or (<= win-nlines y)
             (< buf-nlines linum)))
      (multiple-value-bind (str props)
          (buffer-line-string buffer linum)
        (let ((curx (window-refresh-line window y str props)))
          (when curx
            (setq x curx)))))
    (cl-charms/low-level:wmove (window-win window)
                               (- (window-cur-linum window)
                                  (window-vtop-linum window))
                               x)))

(defun window-refresh (window)
  (window-refresh-modeline window)
  (window-refresh-lines window)
  (cl-charms/low-level:wnoutrefresh (window-win window)))

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

(defun window-update (window)
  (cl-charms/low-level:werase (window-win window))
  (window-adjust-view window t)
  (window-refresh window))

(defun window-update-all ()
  (dolist (win *window-list*)
    (unless (eq win *current-window*)
      (window-update win)))
  (window-update *current-window*)
  (cl-charms/low-level:doupdate))

(define-key *global-keymap* (kbd "C-x2") 'split-window)
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
      (cl-charms/low-level:wresize
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

(define-key *global-keymap* (kbd "C-xo") 'other-window)
(define-command other-window (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (setq *current-window*
      (get-next-window *current-window*))))

(defun window-set-pos (window y x)
  (cl-charms/low-level:mvwin (window-win window) y x)
  (setf (window-y window) y)
  (setf (window-x window) x))

(defun window-set-size (window nlines ncols)
  (cl-charms/low-level:wresize (window-win window) nlines ncols)
  (setf (window-nlines window) nlines)
  (setf (window-ncols window) ncols))

(defun window-move (window dy dx)
  (window-set-pos window
    (+ (window-y window) dy)
    (+ (window-x window) dx)))

(defun window-resize (window dl dc)
  (window-set-size window
    (+ (window-nlines window) dl)
    (+ (window-ncols window) dc)))

(define-key *global-keymap* (kbd "C-x1") 'delete-other-windows)
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

(define-key *global-keymap* (kbd "C-x0") 'delete-current-window)
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
           (or (find-if (lambda (window)
                          (eq buffer (window-buffer window)))
                        *window-list*)
               (get-next-window *current-window*))))
      (set-buffer buffer)
      (values *current-window* one-p))))

(defun popup (buffer &optional fn (goto-bob-p t) (erase-p t))
  (multiple-value-bind (*current-window* newwin-p)
      (pop-to-buffer buffer)
    (when erase-p
      (erase-buffer))
    (when fn (funcall fn))
    (when goto-bob-p
      (beginning-of-buffer))
    (values *current-window* newwin-p)))

(defun popup-string (buffer string)
  (popup buffer
         (lambda ()
           (insert-string string))
         t
         t))

(define-key *global-keymap* (kbd "C-x^") 'grow-window)
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

(define-key *global-keymap* (kbd "C-xC-z") 'shrink-window)
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

(define-key *global-keymap* (kbd "C-xC-n") 'scroll-down)
(define-command scroll-down (n) ("p")
  (if (minusp n)
    (scroll-up (- n))
    (dotimes (_ n t)
      (when (= (window-cursor-y *current-window*) 0)
        (next-line n))
      (window-scroll *current-window* 1))))

(define-key *global-keymap* (kbd "C-xC-p") 'scroll-up)
(define-command scroll-up (n) ("p")
  (if (minusp n)
    (scroll-down (- n))
    (dotimes (_ n t)
      (when (= (window-cursor-y *current-window*)
               (- (window-nlines) 2))
        (prev-line 1))
      (window-scroll *current-window* (- 1)))))
