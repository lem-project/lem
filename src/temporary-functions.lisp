(in-package :lem)

(defun buffer-end (buffer)
  (make-marker buffer
               (point-max buffer)
               :kind :temporary))

(defun eobp/marker (marker)
  (marker= marker
           (buffer-end (marker-buffer marker))))

(defun line-offset (marker n)
  (line-start marker)
  (if (plusp n)
      (dotimes (_ n (values marker t))
        (when (last-line-p marker)
          (return (values (line-end marker) nil)))
        (incf (marker-linum marker)))
      (dotimes (_ (- n) (values marker t))
        (when (first-line-p marker)
          (return (values marker nil)))
        (decf (marker-linum marker)))))

(defun points-to-string (start-marker end-marker)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (region-string (marker-point start-marker)
                 (marker-point end-marker)
                 (marker-buffer start-marker)))
