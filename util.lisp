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

(defun str-width (str)
  (length str))

(defmacro arg-repeat ((arg &optional result) &body body)
  `(dotimes (_ (or ,arg 1) ,result) ,@body))
