(in-package :lem)

(export '(*find-directory-function*
          *auto-mode-alist*
          expand-file-name
          insert-file-contents
          find-file-buffer
          write-to-file
          changed-disk-p))

(defvar *find-directory-function* nil)
(defvar *auto-mode-alist* nil)

(defun parse-pathname (pathname)
  (let ((path))
    (loop
       (let ((pos (position #\/ pathname)))
	 (when (null pos)
	   (push pathname path)
	   (return))
	 (let ((str (subseq pathname 0 pos)))
	   (setq pathname (subseq pathname (1+ pos)))
	   (cond ((string= str "."))
		 ((string= str "..")
		  (pop path))
		 ((string= str "~")
		  (setq path
			(nreverse
			 (parse-pathname
			  (string-right-trim
			   '(#\/)
			   (namestring
			    (user-homedir-pathname)))))))
		 ((string= str "")
		  (setq path nil))
		 (t
		  (push str path))))))
    (nreverse path)))

(defun expand-file-name (pathname &optional directory)
  (concatenate 'string
               "/"
               (join "/"
                     (parse-pathname
                      (if (and (plusp (length pathname))
                               (char/= #\/ (aref pathname 0)))
                          (format nil "~a~a"
                                  (or directory (buffer-directory))
                                  pathname)
                          pathname)))))

#+lem-use-inquisitor
(defun detect-external-format-from-file (pathname)
  (let ((external-format)
        (end-of-line :lf))
    (with-open-file (in pathname
                        :element-type '(unsigned-byte 8))
      (let ((inquisitor:*detecting-buffer-size* (file-length in)))
        (setq external-format (inquisitor:detect-external-format in :jp))))
    #+sbcl
    (with-open-file (in pathname
                        :element-type '(unsigned-byte 8))
      (let ((result (inquisitor:detect-end-of-line in)))
        (when result
          (setq end-of-line result))))
    (values external-format
            end-of-line)))

#-lem-use-inquisitor
(defun detect-external-format-from-file (pathname)
  (declare (ignore pathnaem))
  (values :utf-8 :lf))

(defun insert-file-contents (point filename)
  (multiple-value-bind (external-format end-of-line)
      (detect-external-format-from-file filename)
    (with-open-stream (output (make-buffer-output-stream point nil))
      (with-open-file (in filename :external-format external-format)
        (loop
	   (multiple-value-bind (str eof-p)
	       (read-line in nil)
	     (cond
	       (eof-p
		(when str
		  (write-string str output))
		(return))
	       (t
		(let ((end nil))
		  #+sbcl
		  (when (and (eq end-of-line :crlf)
			     (< 0 (length str)))
		    (setf end (1- (length str))))
		  (write-line str output :end end)))))))
      (setf (buffer-external-format (point-buffer point))
            (cons external-format end-of-line))
      point)))

(defun prepare-auto-mode (buffer)
  (let* ((filename (file-namestring (buffer-filename buffer)))
         (elt (find-if (lambda (elt)
                         (ppcre:scan (car elt) filename))
                       *auto-mode-alist*)))
    (when elt
      (change-buffer-mode buffer (cdr elt)))))

(defun scan-line-property-list (buffer str)
  (ppcre:do-register-groups (var val)
      ("([a-zA-Z0-9-_]+)\\s*:\\s*([^ ;]+);?" str)
    (cond ((string= (string-downcase var) "mode")
           (let ((mode (find-mode-from-name val)))
             (when mode
               (change-buffer-mode buffer mode))))
          (t
           (setf (get-bvar :file-property-list :buffer buffer)
                 (cons (cons (string-downcase var) val)
                       (get-bvar :file-property-list :buffer buffer)))))))

(defun scan-file-property-list (buffer)
  (with-point ((cur-point (buffer-point buffer)))
    (buffer-start cur-point)
    (when (ppcre:scan "^#!" (line-string-at cur-point))
      (line-offset cur-point 1))
    (loop :until (end-line-p cur-point)
       :for string := (line-string-at cur-point)
       :do (ppcre:register-groups-bind (result)
	       ("-\\*-(.*)-\\*-" string)
	     (when result
	       (scan-line-property-list buffer result)
	       (return)))
       :do (if (string= "" (string-trim '(#\space #\tab) string))
	       (line-offset cur-point 1)
	       (return)))))

(defun find-file-buffer (filename)
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (setf filename (expand-file-name filename))
  (cond ((uiop:directory-pathname-p filename)
         (if *find-directory-function*
             (funcall *find-directory-function* filename)
             (editor-error "~A is a directory" filename)))
        ((find filename (buffer-list) :key #'buffer-filename :test #'equal))
        (t
         (let* ((name (file-namestring filename))
                (buffer (make-buffer (if (get-buffer name)
                                         (uniq-buffer-name name)
                                         name)
                                     :filename filename
                                     :enable-undo-p nil)))
           (when (probe-file filename)
             (insert-file-contents (buffers-start buffer)
                                   filename)
             (buffer-unmark buffer))
           (move-point (buffer-point buffer) (buffers-start buffer))
           (buffer-enable-undo buffer)
           (update-changed-disk-date buffer)
           (prepare-auto-mode buffer)
           (scan-file-property-list buffer)
           (save-excursion
             (setf (current-buffer) buffer)
             (run-hooks 'find-file-hook))
           (values buffer t)))))

(defun write-to-file (buffer filename)
  (flet ((f (out end-of-line)
           (with-open-stream (in (make-buffer-input-stream (buffers-start buffer)))
             (loop
		(multiple-value-bind (str eof-p)
		    (read-line in nil)
		  (unless str (return))
		  (princ str out)
		  (unless eof-p
		    #+sbcl
		    (ecase end-of-line
		      ((:crlf)
		       (princ #\return out)
		       (princ #\newline out))
		      ((:lf)
		       (princ #\newline out))
		      ((:cr)
		       (princ #\return out)))
		    #-sbcl
		    (princ #\newline out)
		    ))))))
    (cond
      ((buffer-external-format buffer)
       (with-open-file (out filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format (car (buffer-external-format
                                                   buffer)))
         (f out
            (cdr (buffer-external-format
                  buffer)))))
      (t
       (with-open-file (out filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
         (f out :lf)))))
  (buffer-unmark buffer)
  (update-changed-disk-date buffer))

(defun save-buffer-internal ()
  (scan-file-property-list (current-buffer))
  (run-hooks 'before-save-hook)
  (write-to-file (current-buffer) (buffer-filename))
  (run-hooks 'after-save-hook)
  t)

(defun file-write-date* (buffer)
  (if (probe-file (buffer-filename buffer))
      (file-write-date (buffer-filename buffer))))

(defun update-changed-disk-date (buffer)
  (setf (buffer-last-write-date buffer)
        (file-write-date* buffer)))

(defun changed-disk-p (buffer)
  (and (buffer-have-file-p buffer)
       (not (eql (buffer-last-write-date buffer)
                 (file-write-date* buffer)))))
