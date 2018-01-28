(in-package :lem-base)

(export '(expand-file-name
          directory-files))

(defun guess-host-name (filename)
  #+windows
  (ppcre:register-groups-bind (host)
      ("^(\\w:)" filename)
    (pathname-host (parse-namestring host)))
  #-windows
  nil)

(defun parse-filename (filename)
  (let* ((host (guess-host-name filename))
         (start0 (if host 2 0))
         (split-chars #+windows '(#\/ #\\) #-windows '(#\/)))
    (flet ((split-char-p (c) (member c split-chars)))
      (loop :with path := (list :absolute)
            :for start := start0 :then (1+ pos)
            :for pos := (position-if #'split-char-p filename :start start)
            :unless pos :do (return
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
                         (setf path (pathname-directory (user-homedir-pathname))))
                        ((string= name "")
                         (setf path (list :absolute)))
                        (t
                         (setf path (append path (list name))))))))))

(defun expand-file-name (filename &optional (directory (uiop:getcwd)))
  (when (pathnamep filename) (setf filename (namestring filename)))
  (let ((pathname (parse-filename filename)))
    (merge-pathnames pathname directory)))

(defun directory-files (pathspec)
  (if (null (pathname-name pathspec))
      (list (pathname pathspec))
      (or (directory pathspec)
          (list pathspec))))
