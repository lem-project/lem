(in-package :lem)

(export '(region-beginning
          region-end
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

(defun apply-region-lines (start end function)
  (with-marker ((start start :right-inserting)
                (end end :right-inserting))
    (move-point (current-marker) start)
    (loop :while (marker< (current-marker) end) :do
          (with-marker ((prev (line-start (current-marker))))
            (funcall function)
            (when (same-line-p (current-marker) prev)
              (unless (line-offset (current-marker) 1)
                (return)))))))
