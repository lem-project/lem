(in-package :lem)

(export '(region-beginning
          region-end
          region-string
          region-count
          delete-region
          apply-region-lines))

(defun region-beginning (&optional (buffer (current-buffer)))
  (let ((start (buffer-point-marker buffer))
        (end (buffer-mark-marker buffer)))
    (if (marker< start end)
        start
        end)))

(defun region-end (&optional (buffer (current-buffer)))
  (let ((start (buffer-point-marker buffer))
        (end (buffer-mark-marker buffer)))
    (if (marker< start end)
        end
        start)))

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

(defun apply-region-lines (start end function)
  (with-marker ((start start :right-inserting)
                (end end :right-inserting))
    (move-point (current-marker) start)
    (loop :while (marker< (current-marker) end) :do
          (with-marker ((prev (line-start (current-marker))))
            (funcall function)
            (when (same-line-p (current-marker) prev)
              (unless (nth-value 1 (line-offset (current-marker) 1))
                (return)))))))
