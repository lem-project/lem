(in-package :lem)

(defgeneric file-open (buffer filename))
(defgeneric file-find (buffer arg))
(defgeneric file-save (buffer arg))

(defmethod file-open ((buffer buffer) filename)
  (let ((textbuf (make-textbuf filename filename)))
    (with-open-file (in filename :if-does-not-exist nil)
      (when in
	(do () (nil)
          (multiple-value-bind (str eof-p) (read-line in nil)
	    (if (not eof-p)
	      (textbuf-append-line textbuf str)
	      (progn
	       (textbuf-append-line textbuf (or str ""))
	       (return)))))))
    (buffer-change-textbuf buffer textbuf)
    (buffer-unmark buffer nil)
    t))

(defmethod file-find ((buffer buffer) arg)
  (declare (ignore arg))
  (let ((filename (mb-readline "Find File: ")))
    (file-open buffer filename)
    t))

(defmethod file-save ((buffer buffer) arg)
  (declare (ignore arg))
  (let ((textbuf (buffer-textbuf buffer)))
    (cond
     ((null (textbuf-modif-p textbuf))
      nil)
     ((null (textbuf-filename textbuf))
      (mb-write "No file name")
      nil)
     (t
      (with-open-file (out (textbuf-filename textbuf)
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
	(dolist (str (textbuf-take-lines textbuf 1 (textbuf-nlines textbuf)))
	  (format out "~&~a" str)))
      (buffer-unmark buffer nil)
      (mb-write "Wrote")
      t))))
