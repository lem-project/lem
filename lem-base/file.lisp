(in-package :lem-base)

(export '(*find-file-hook*
          *before-save-hook*
          *after-save-hook*
          *external-format-function*
          *find-directory-function*
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

(defun insert-file-contents (point filename)
  (let ((external-format :utf-8)
        (end-of-line :lf))
    (when *external-format-function*
      (multiple-value-setq (external-format end-of-line)
        (funcall *external-format-function* filename)))
    (with-point ((point point :left-inserting))
      (with-open-virtual-file (in filename
                                  :external-format external-format
                                  :direction :input)
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
    (with-open-virtual-file (out filename
                                 :external-format (first (buffer-external-format buffer))
                                 :direction :output)
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
      (with-open-virtual-file (out filename
                                   :external-format (first (buffer-external-format buffer))
                                   :direction :output)
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
