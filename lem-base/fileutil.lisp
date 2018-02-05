(in-package :lem-base)

(export '(expand-file-name
          directory-files
          list-directory
          file-size))

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
    (namestring (merge-pathnames pathname directory))))

(defun directory-files (pathspec)
  (if (uiop:directory-pathname-p pathspec)
      (list (pathname pathspec))
      (or (directory pathspec)
          (list pathspec))))

(defun list-directory (directory)
  (append (uiop:subdirectories directory)
          (uiop:directory-files directory)))

(defun file-size (pathname)
  #+lispworks
  (system:file-size pathname)
  #+(and (not lispworks) win32)
  (return-from file-size nil)
  #-win32
  (ignore-errors (with-open-file (in pathname) (file-length in))))
