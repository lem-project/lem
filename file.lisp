(in-package :lem)

(defun file-open (filename)
  (let ((buffer (make-buffer filename filename)))
    (with-open-file (in filename :if-does-not-exist nil)
      (when in
	(do () (nil)
          (multiple-value-bind (str eof-p) (read-line in nil)
	    (if (not eof-p)
	      (buffer-append-line buffer str)
	      (progn
	       (buffer-append-line buffer (or str ""))
	       (return)))))))
    (set-buffer buffer)
    (unmark-buffer)
    t))

(add-command 'find-file 'find-file "C-xC-f")
(defun find-file (arg)
  (declare (ignore arg))
  (let* ((filename (mb-readline "Find File: "))
         (buf (get-buffer filename)))
    (cond
     ((null buf)
      (file-open filename))
     ((or
       (not (buffer-filename buf))
       (string/= filename (buffer-filename buf)))
      (let ((name (uniq-buffer-name filename)))
        (set-buffer (make-buffer name filename))))
     (t
      (set-buffer buf)))))

(add-command 'save-file 'save-file "C-xC-s")
(defun save-file (arg)
  (declare (ignore arg))
  (let ((buffer (window-buffer)))
    (cond
     ((null (buffer-modified-p buffer))
      nil)
     ((null (buffer-filename buffer))
      (mb-write "No file name")
      nil)
     (t
      (with-open-file (out (buffer-filename buffer)
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
	(dolist (str (buffer-take-lines buffer 1 (buffer-nlines buffer)))
	  (format out "~&~a" str)))
      (unmark-buffer)
      (mb-write "Wrote")
      t))))
