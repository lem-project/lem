(in-package :lem)

(defun file-open (filename)
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
    (set-buffer textbuf)
    (unmark-buffer)
    t))

(add-command 'find-file 'find-file "C-xC-f")
(defun find-file (arg)
  (declare (ignore arg))
  (let ((filename (mb-readline "Find File: ")))
    (file-open filename)
    t))

(add-command 'save-file 'save-file "C-xC-s")
(defun save-file (arg)
  (declare (ignore arg))
  (let ((textbuf (window-textbuf)))
    (cond
     ((null (textbuf-modified-p textbuf))
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
      (unmark-buffer)
      (mb-write "Wrote")
      t))))
