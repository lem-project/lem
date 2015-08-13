(in-package :lem)

(defun pdebug (x)
  (with-open-file (out "DEBUG"
		   :direction :output
		   :if-exists :append
		   :if-does-not-exist :create)
    (print x out)))

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defvar *eastasian-full*
  (vector
   '(#x01100 #x0115f) '(#x02329 #x0232a) '(#x02e80 #x02e99) '(#x02e9b #x02ef3)
   '(#x02f00 #x02fd5) '(#x02ff0 #x02ffb) '(#x03000 #x0303e) '(#x03041 #x03096)
   '(#x03099 #x030ff) '(#x03105 #x0312d) '(#x03131 #x0318e) '(#x03190 #x031ba)
   '(#x031c0 #x031e3) '(#x031f0 #x0321e) '(#x03220 #x03247) '(#x03250 #x032fe)
   '(#x03300 #x04dbf) '(#x04e00 #x0a48c) '(#x0a490 #x0a4c6) '(#x0a960 #x0a97c)
   '(#x0ac00 #x0d7a3) '(#x0f900 #x0faff) '(#x0fe10 #x0fe19) '(#x0fe30 #x0fe52)
   '(#x0fe54 #x0fe66) '(#x0fe68 #x0fe6b) '(#x0ff01 #x0ff60) '(#x0ffe0 #x0ffe6)
   '(#x1b000 #x1b001) '(#x1f200 #x1f202) '(#x1f210 #x1f23a) '(#x1f240 #x1f248)
   '(#x1f250 #x1f251) '(#x20000 #x2fffd) '(#x30000 #x3fffd)))

(defun wide-table-cmp (key val)
  (cond ((<= (car val) key (cadr val))
         0)
        ((< key (car val))
         -1)
        (t
         1)))

(defun binary-search (vec val cmp-f)
  (labels ((rec (begin end)
             (when (<= begin end)
               (let* ((i (floor (+ end begin) 2))
                      (result (funcall cmp-f val (aref vec i))))
                 (cond
                   ((plusp result)
                    (rec (1+ i) end))
                   ((minusp result)
                    (rec begin (1- i)))
                   (t
                    (aref vec i)))))))
    (rec 0 (1- (length vec)))))

(defun wide-char-p (c)
  (binary-search *eastasian-full* (char-code c) 'wide-table-cmp))

(defun char-width (c w)
  (cond ((char= c #\tab)
	 (+ (* (floor w *tab-size*) *tab-size*) *tab-size*))
	((or (wide-char-p c) (key::ctrl-p c))
	 (+ w 2))
	(t
	 (+ w 1))))

(defun str-width (str &optional n)
  (when (and n (< n (length str)))
    (setq str (subseq str 0 n)))
  (loop with width = 0
        for c across str
        do (setq width (char-width c width))
	finally (return width)))

(defun wide-index (str goal)
  (loop with w = 0
	for i from 0 below (length str) by 1
	for c across str do
	  (setq w (char-width c w))
	  (when (<= goal w)
	    (return i))))

(defun substring-width (str begin &optional end)
  (let ((wb (wide-index str begin))
	(we (when end
	      (wide-index str end))))
    (subseq str wb we)))

(defun split-string (str delim)
  (labels ((f (str acc length)
              (let ((i (position delim str)))
                (if (null i)
                    (values (nreverse (cons str acc))
                            (1+ length))
                    (f (subseq str (1+ i))
                       (cons (subseq str 0 i) acc)
                       (1+ length))))))
    (f str nil 0)))

(defun join (str strings)
  (do ((rest strings (cdr rest))
       (acc ""))
      ((null (cdr rest))
       (concatenate 'string acc (car rest)))
    (setq acc (concatenate 'string acc (car rest) str))))

(defun replace-string (before after string)
  (let ((i (search before string)))
    (if i
      (values (concatenate 'string
                (subseq string 0 i)
                after
                (replace-string before after
                  (subseq string (+ i (length before)))))
        t)
      (values string nil))))

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil))
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defun random-range (min max)
  (+ min (random (1+ (- max min)))))
