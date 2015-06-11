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
    (t (error "unexpected character ~a" c))))

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
  (cond
    ((< key (car val))
     -1)
    ((> key (cadr val))
     1)
    (t
     0)))

(defun binary-search (vec val cmp-f)
  (labels ((rec (begin end)
             (unless (= begin end)
               (let* ((i (floor (+ end begin) 2))
                      (result (funcall cmp-f val (aref vec i))))
                 (cond
                   ((plusp result)
                    (rec i end))
                   ((minusp result)
                    (rec begin i))
                   (t
                    (aref vec i)))))))
    (rec 0 (1- (length vec)))))

(defun char-width (c w)
  (cond ((char= c #\tab)
	 (+ (* (floor w *tab-size*) *tab-size*) *tab-size*))
	((binary-search *eastasian-full* (char-code c) 'wide-table-cmp)
	 (+ w 2))
	(t
	 (+ w 1))))

(defun %str-width (str)
  (loop with width = 0
        for c across str
        do (setq width (char-width c width))
	finally (return width)))

(defun str-width (str &optional n)
  (%str-width
    (if n
      (subseq str 0 n)
      str)))

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

(defmacro arg-repeat ((arg &optional result) &body body)
  `(dotimes (_ (or ,arg 1) ,result) ,@body))
