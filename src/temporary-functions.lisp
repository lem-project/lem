(in-package :lem)

(defun buffers-start (buffer)
  (make-marker buffer
               (point-min buffer)
               :kind :temporary))

(defun buffers-end (buffer)
  (make-marker buffer
               (point-max buffer)
               :kind :temporary))

(defun points-to-string (start-marker end-marker)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (with-output-to-string (out)
    (map-region start-marker end-marker
                (lambda (string lastp)
                  (write-string string out)
                  (unless lastp
                    (write-char #\newline out))))))

(defun delete-between-points (start-marker end-marker)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (unless (marker< start-marker end-marker)
    (rotatef start-marker end-marker))
  (delete-char/marker (marker-buffer start-marker)
                      (count-characters start-marker end-marker)))

(defun map-region (start-marker end-marker function)
  (when (marker< end-marker start-marker)
    (rotatef start-marker end-marker))
  (let ((start-line (buffer-get-line (marker-buffer start-marker)
                                     (marker-linum start-marker))))
    (loop :for line := start-line :then (line-next line)
          :for linum :from (marker-linum start-marker) :to (marker-linum end-marker)
          :for firstp := (eq line start-line)
          :for lastp := (= linum (marker-linum end-marker))
          :do (funcall function
                       (subseq (line-str line)
                               (if firstp
                                   (marker-charpos start-marker)
                                   0)
                               (if lastp
                                   (marker-charpos end-marker)
                                   nil))
                       lastp)))
  (values))

(defun count-characters (start-marker end-marker)
  (let ((count 0))
    (map-region start-marker
                end-marker
                (lambda (string lastp)
                  (incf count (length string))
                  (unless lastp
                    (incf count))))
    count))

(defun invoke-save-excursion (function)
  (let ((point (copy-marker (current-marker) :temporary))
        (mark (when (buffer-mark-p (current-buffer))
                (copy-marker (buffer-mark-marker (current-buffer))
                             :temporary))))
    (unwind-protect (funcall function)
      (setf (current-buffer) (marker-buffer point))
      (move-point (current-marker) point)
      (when mark
        (set-current-mark mark)))))
