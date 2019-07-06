(in-package :lem-base)

(export '(expand-file-name
          directory-files
          list-directory
          file-size
          virtual-probe-file
          *virtual-file-open*
          with-open-virtual-file))

(defun guess-host-name (filename)
  (declare (ignorable filename))
  #+windows
  (ppcre:register-groups-bind (host)
      ("^(\\w:)" filename)
    (pathname-host (parse-namestring host)))
  #-windows
  nil)

(defun parse-filename (filename path)
  (let* ((host (guess-host-name filename))
         (start0 (if host 2 0))
         (split-chars #+windows '(#\/ #\\) #-windows '(#\/)))
    (flet ((split-char-p (c) (member c split-chars)))
      (loop :for start := start0 :then (1+ pos)
            :for pos := (position-if #'split-char-p filename :start start)
            :unless pos
            :do (return
                  (if (= start (length filename))
                      (make-pathname :directory path :host host)
                      (let ((name (subseq filename start)))
                        (make-pathname :name (pathname-name name)
                                       :type (pathname-type name)
                                       :directory path
                                       :host host))))
            :while pos
            :do (let ((name (subseq filename start pos)))
                  (cond ((string= name "."))
                        ((string= name "..")
                         (setf path (butlast path)))
                        ((string= name "~")
                         (setf path (pathname-directory (truename "~/"))))
                        ((string= name "")
                         (setf path (list :absolute)))
                        (t
                         (setf path (append path (list name))))))))))

(defun expand-file-name (filename &optional (directory (uiop:getcwd)))
  (when (pathnamep filename) (setf filename (namestring filename)))
  (let ((pathname (parse-filename filename (pathname-directory directory))))
    (namestring (merge-pathnames pathname directory))))

(defun probe-file% (x)
  (let ((x2 (probe-file x)))
    (when x2
      (let* ((base "~/")
             (mod (namestring (truename base)))
             (len (length mod)))
        (if (equal (ignore-errors (subseq (namestring x2) 0 len)) mod)
            (make-pathname :defaults (format nil "~A~A" base (subseq (namestring x2) len)))
            x2)))))

(defun virtual-probe-file (pathspec &optional (base-dir pathspec))
  (cond
    ((ppcre:scan "^~/.*" (namestring base-dir)) (probe-file% pathspec))
    (t (probe-file pathspec))))

(defun directory-files (pathspec)
  (if (uiop:directory-pathname-p pathspec)
      (list (pathname pathspec))
      (or (mapcar (lambda (x) (virtual-probe-file x pathspec))
                  (directory pathspec))
          (list pathspec))))

(defun list-directory (directory &key directory-only)
  (delete nil
          (mapcar (lambda (x) (and (virtual-probe-file x directory) x))
                  (append (sort (copy-list (uiop:subdirectories directory))
                                #'string< :key #'namestring)
                          (unless directory-only
                            (sort (copy-list (uiop:directory-files directory))
                                  #'string< :key #'namestring))))))

(defun file-size (pathname)
  #+lispworks
  (system:file-size pathname)
  #+(and (not lispworks) win32)
  (return-from file-size nil)
  #-win32
  (ignore-errors (with-open-file (in pathname) (file-length in))))

(defvar *virtual-file-open* nil)

(defun open-virtual-file (filename &key external-format direction element-type)
  (apply #'values
         (or (loop :for f :in *virtual-file-open*
                   :for result := (funcall f filename
                                           :external-format external-format
                                           :element-type element-type
                                           :direction direction)
                   :when result
                   :do (return result))
             (list (apply #'open filename
                          `(:direction ,direction
                            ,@(when (eql direction :output)
                                '(:if-exists :supersede
                                  :if-does-not-exist :create))
                            ,@(when external-format
                                `(:external-format ,external-format))
                            :element-type ,@(if element-type (list element-type) '(character))))))))

(defmacro with-open-virtual-file ((stream filespec &rest options)
                                  &body body)
  (let ((close/ (gensym)))
    `(multiple-value-bind (,stream ,close/)
         (open-virtual-file ,filespec ,@options)
       (unwind-protect
            (multiple-value-prog1
                (progn ,@body))
         (when ,stream
           (funcall (or ,close/ #'close) ,stream))))))
