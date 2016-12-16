(in-package :lem)

(export '(region-beginning
          region-end
          region-string
          region-count
          delete-region
          apply-region-lines))

(defun region-beginning ()
  (let ((point1 (current-point))
        (point2 (mark-point)))
    (if (point< point1 point2)
        point1
        point2)))

(defun region-end ()
  (let ((point1 (current-point))
        (point2 (mark-point)))
    (if (point< point1 point2)
        point2
        point1)))

(defun region-string (begin end &optional (buffer (current-buffer)))
  (with-output-to-string (out)
    (map-region (make-marker buffer begin :kind :temporary)
                (make-marker buffer end :kind :temporary)
                (lambda (string lastp)
                  (write-string string out)
                  (unless lastp
                    (write-char #\newline out))))))

(defun region-count (begin end &optional (buffer (current-buffer)))
  (let ((count 0))
    (map-region (make-marker buffer begin :kind :temporary)
                (make-marker buffer end :kind :temporary)
                (lambda (string lastp)
                  (incf count (length string))
                  (unless lastp
                    (incf count))))
    count))

(defun delete-region (begin end &optional (buffer (current-buffer)))
  (when (point< end begin)
    (rotatef begin end))
  (point-set begin buffer)
  (prog1 (delete-char/marker (buffer-point-marker buffer)
                             (region-count begin end buffer))
    (buffer-mark-cancel buffer)))

(defun apply-region-lines (begin end fn)
  (point-set begin)
  (do () ((point<= end (current-point)))
    (let ((linum (current-linum)))
      (beginning-of-line)
      (funcall fn)
      (when (= linum (current-linum))
        (unless (forward-line 1)
          (return))))))
