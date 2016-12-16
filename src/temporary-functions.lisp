(in-package :lem)

(defun buffer-start-marker (buffer)
  (make-marker buffer
               (point-min buffer)
               :kind :temporary))

(defun buffer-end-marker (buffer)
  (make-marker buffer
               (point-max buffer)
               :kind :temporary))

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
