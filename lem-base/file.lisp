(in-package :lem-base)

(export '(*find-file-hook*
          before-save-hook
          after-save-hook
          *external-format-function*
          *find-directory-function*
          insert-file-contents
          find-file-buffer
          write-to-file
          write-region-to-file
          update-changed-disk-date
          changed-disk-p))

(defvar *find-file-hook* '())

(define-editor-variable before-save-hook '())
(define-editor-variable after-save-hook '())

(defvar *external-format-function* nil)
(defvar *find-directory-function* nil)

(defun %insert-file-contents (external-format in point end-of-line)
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
           (insert-character point #\newline)))))))

(defun insert-file-contents (point filename
                             &key (external-format :utf-8)
                                  (end-of-line :lf))
  (when (and *external-format-function*)
    (multiple-value-setq (external-format end-of-line)
      (funcall *external-format-function* filename)))
  (with-point ((point point :left-inserting))
    (with-open-virtual-file (in filename
                                :element-type nil
                                :external-format external-format
                                :direction :input)
      (%insert-file-contents external-format in point end-of-line)))
  (values external-format end-of-line))

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
  (write-region-to-file (buffer-start-point buffer)
                        (buffer-end-point buffer) filename))

(defun run-before-save-hooks (buffer)
  (alexandria:when-let ((hooks (variable-value 'before-save-hook :buffer buffer)))
    (run-hooks hooks buffer))
  (alexandria:when-let ((hooks (variable-value 'before-save-hook :global)))
    (run-hooks hooks buffer)))

(defun run-after-save-hooks (buffer)
  (alexandria:when-let ((hooks (variable-value 'after-save-hook :buffer buffer)))
    (run-hooks hooks buffer))
  (alexandria:when-let ((hooks (variable-value 'after-save-hook :global)))
    (run-hooks hooks buffer)))

(defun call-with-write-hook (buffer function)
  (run-before-save-hooks buffer)
  (funcall function)
  (update-changed-disk-date buffer)
  (run-after-save-hooks buffer))

(defmacro with-write-hook (buffer &body body)
  `(call-with-write-hook ,buffer (lambda () ,@body)))

(defun write-to-file (buffer filename)
  (with-write-hook buffer
    (write-to-file-1 buffer filename)))

(defun %write-region-to-file (out end-of-line)
  (lambda (string eof-p)
    (princ string out)
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
      (princ #\newline out))))

(defun write-region-to-file (start end filename)
  (let* ((buffer (point-buffer start))
         (end-of-line (if (buffer-external-format buffer)
                          (cdr (buffer-external-format
                                buffer))
                          :lf)))
    (with-write-hook buffer
      (with-open-virtual-file (out filename
                                   :element-type nil
                                   :external-format (first (buffer-external-format buffer))
                                   :direction :output)
        (map-region start end
                    (%write-region-to-file out end-of-line))))))

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
