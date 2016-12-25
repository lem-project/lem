(in-package :lem)

(defun buffers-start (buffer)
  (make-marker buffer 1 0 :kind :temporary))

(defun buffers-end (buffer)
  (make-marker buffer
               (buffer-nlines buffer)
               (line-length (buffer-tail-line buffer))
               :kind :temporary))

(defun points-to-string (start end)
  (assert (eq (marker-buffer start)
              (marker-buffer end)))
  (with-output-to-string (out)
    (map-region start end
                (lambda (string lastp)
                  (write-string string out)
                  (unless lastp
                    (write-char #\newline out))))))

(defun delete-between-points (start end)
  (assert (eq (marker-buffer start)
              (marker-buffer end)))
  (unless (point< start end)
    (rotatef start end))
  (delete-char/marker start
                      (count-characters start end)))

(defun %map-region (start end function)
  (when (point< end start)
    (rotatef start end))
  (let ((start-line (buffer-get-line (marker-buffer start)
                                     (marker-linum start))))
    (loop :for line := start-line :then (line-next line)
          :for linum :from (marker-linum start) :to (marker-linum end)
          :for firstp := (eq line start-line)
          :for lastp := (= linum (marker-linum end))
          :do (funcall function
                       line
                       (if firstp
                           (marker-charpos start)
                           0)
                       (if lastp
                           (marker-charpos end)
                           nil))))
  (values))

(defun map-region (start end function)
  (%map-region start end
               (lambda (line start end)
                 (funcall function
                          (subseq (line-str line) start end)
                          (not (null end))))))

(defun count-characters (start end)
  (let ((count 0))
    (map-region start
                end
                (lambda (string lastp)
                  (incf count (length string))
                  (unless lastp
                    (incf count))))
    count))

(defun count-lines (start end)
  (assert (eq (marker-buffer start)
              (marker-buffer end)))
  (when (point< end start)
    (rotatef start end))
  (with-marker ((point start))
    (loop :for count :from 0 :do
          (when (point< end point)
            (return count))
          (unless (line-offset point 1)
            (return (1+ count))))))

(defun line-number-at-point (point)
  (count-lines (buffers-start (marker-buffer point)) point))

(defun invoke-save-excursion (function)
  (let ((point (copy-point (current-marker) :temporary))
        (mark (when (buffer-mark-p (current-buffer))
                (copy-point (buffer-mark-marker (current-buffer))
                             :temporary))))
    (unwind-protect (funcall function)
      (setf (current-buffer) (marker-buffer point))
      (move-point (current-marker) point)
      (when mark
        (set-current-mark mark)))))
