(in-package :lem-base)

(export '(expand-file-name))

(defun expand-file-name-1 (filename host)
  (let* ((split-chars #+windows '(#\/ #\\) #-windows '(#\/)))
    (flet ((split-char-p (c) (member c split-chars)))
      (loop :with path := (list :absolute)
            :for start := 0 :then (1+ pos)
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
  (flet ((f (filename) (subseq (namestring (merge-pathnames filename directory)) 2)))
    (setf filename (namestring filename))
    (let ((host #+windows
                (ppcre:register-groups-bind (host)
                    ("^(\\w):" filename)
                  host)
                #-windows
                nil))
      (namestring
       (if host
           (expand-file-name-1 (f (subseq filename 2)) host)
           (expand-file-name-1 (f filename) host))))))
