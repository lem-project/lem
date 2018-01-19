(in-package :lem-base)

(export '(*find-file-hook*
          *before-save-hook*
          *after-save-hook*
          *external-format-function*
          *find-directory-function*
          expand-file-name
          insert-file-contents
          find-file-buffer
          write-to-file
          write-region-to-file
          update-changed-disk-date
          changed-disk-p))

(defvar *find-file-hook* '())
(defvar *before-save-hook* '())
(defvar *after-save-hook* '())

(defvar *external-format-function* nil)
(defvar *find-directory-function* nil)

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

(defun insert-file-contents (point filename)
  (let ((external-format :utf-8)
        (end-of-line :lf))
    (when *external-format-function*
      (multiple-value-setq (external-format end-of-line)
        (funcall *external-format-function* filename)))
    (with-point ((point point :left-inserting))
      (with-open-file (in filename :external-format external-format :element-type 'character)
        (loop
          (multiple-value-bind (str eof-p)
              (read-line in nil)
            (cond
              (eof-p
               (when str
                 (insert-string point str))
               (return))
              (t
               (let ((end nil))
                 #+sbcl
                 (when (and (eq end-of-line :crlf)
                            (< 0 (length str)))
                   (setf end (1- (length str))))
                 (insert-string point
                                (if end
                                    (subseq str 0 end)
                                    str))
                 (insert-character point #\newline))))))))
    (values external-format end-of-line)))

(defun find-file-buffer (filename &key temporary (enable-undo-p t))
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (setf filename (expand-file-name filename))
  (cond ((uiop:directory-pathname-p filename)
         (if *find-directory-function*
             (funcall *find-directory-function* filename)
             (editor-error "~A is a directory" filename)))
        ((and (not temporary)
              (find filename (buffer-list) :key #'buffer-filename :test #'equal)))
        (t
         (let* ((name (file-namestring filename))
                (buffer (make-buffer (if temporary
                                         name
                                         (if (get-buffer name)
                                             (uniq-buffer-name name)
                                             name))
                                     :enable-undo-p nil
                                     :temporary temporary)))
           (setf (buffer-filename buffer) filename)
           (when (probe-file filename)
             (let ((*inhibit-modification-hooks* t))
               (multiple-value-bind (external-format end-of-line)
                   (insert-file-contents (buffer-start-point buffer)
                                         filename)
                 (setf (buffer-external-format buffer)
                       (cons external-format end-of-line))))
             (buffer-unmark buffer))
           (buffer-start (buffer-point buffer))
           (when enable-undo-p (buffer-enable-undo buffer))
           (update-changed-disk-date buffer)
           (run-hooks *find-file-hook* buffer)
           (values buffer t)))))

(defun open-output-file (buffer filename)
  (if (buffer-external-format buffer)
      (open filename
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :external-format (car (buffer-external-format buffer))
            :element-type 'character)
      (open filename
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :element-type 'character)))

(defun write-to-file-1 (buffer filename)
  (flet ((f (out end-of-line)
           (with-point ((point (buffer-start-point buffer)))
             (loop :for eof-p := (end-buffer-p point)
                   :for str := (line-string point)
                   :do (princ str out)
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
                         )
                       (unless (line-offset point 1)
                         (return))))))
    (with-open-stream (out (open-output-file buffer filename))
      (f out
         (if (buffer-external-format buffer)
             (cdr (buffer-external-format
                   buffer))
             :lf)))))

(defmacro with-write-hook (buffer &body body)
  (alexandria:once-only (buffer)
    `(progn
       (run-hooks *before-save-hook* ,buffer)
       ,@body
       (update-changed-disk-date ,buffer)
       (run-hooks *after-save-hook* ,buffer))))

(defun write-to-file (buffer filename)
  (with-write-hook buffer
    (write-to-file-1 buffer filename)
    (buffer-unmark buffer)))

(defun write-region-to-file (start end filename)
  (let ((string (points-to-string start end))
        (buffer (point-buffer start)))
    (with-write-hook buffer
      (with-open-stream (out (open-output-file buffer filename))
        (write-string string out)))))

(defun file-write-date* (buffer)
  (if (probe-file (buffer-filename buffer))
      (file-write-date (buffer-filename buffer))))

(defun update-changed-disk-date (buffer)
  (setf (buffer-last-write-date buffer)
        (file-write-date* buffer)))

(defun changed-disk-p (buffer)
  (and (buffer-filename buffer)
       (probe-file (buffer-filename buffer))
       (not (eql (buffer-last-write-date buffer)
                 (file-write-date* buffer)))))
