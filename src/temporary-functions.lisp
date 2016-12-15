(in-package :lem)

(defun buffer-end (buffer)
  (make-marker buffer
               (point-max buffer)
               :kind :temporary))


(defun character-at (marker)
  (buffer-get-char (marker-buffer marker)
                   (marker-linum marker)
                   (marker-charpos marker)))

(defun line-start (marker)
  (setf (marker-charpos marker) 0)
  marker)

(defun line-end (marker)
  (setf (marker-charpos marker)
        (buffer-line-length (marker-buffer marker)
                            (marker-linum marker)))
  marker)

(defun first-line-p/marker (marker)
  (<= (marker-linum marker) 1))

(defun last-line-p/marker (marker)
  (<= (buffer-nlines (marker-buffer marker))
      (marker-linum marker)))

(defun eobp/marker (marker)
  (marker= marker
           (buffer-end (marker-buffer marker))))

(defun line-offset (marker n)
  (line-start marker)
  (if (plusp n)
      (dotimes (_ n (values marker t))
        (when (last-line-p/marker marker)
          (return (values (line-end marker) nil)))
        (incf (marker-linum marker)))
      (dotimes (_ (- n) (values marker t))
        (when (first-line-p/marker marker)
          (return (values marker nil)))
        (decf (marker-linum marker)))))

(defun %character-offset-positive (marker n)
  (loop
    (when (minusp n)
      (return (values marker nil)))
    (let* ((length (1+ (buffer-line-length (marker-buffer marker)
                                           (marker-linum marker))))
           (w (- length (marker-charpos marker))))
      (when (< n w)
        (incf (marker-charpos marker) n)
        (return (values marker t)))
      (decf n w)
      (unless (nth-value 1 (line-offset marker 1))
        (return (values marker nil))))))

(defun %character-offset-negative (marker n)
  (loop
    (when (minusp n)
      (return (values marker nil)))
    (when (<= n (marker-charpos marker))
      (decf (marker-charpos marker) n)
      (return (values marker t)))
    (decf n (1+ (marker-charpos marker)))
    (cond ((first-line-p/marker marker)
           (return (values (line-start marker) nil)))
          (t
           (line-offset marker -1)
           (line-end marker)))))

(defun character-offset (marker n)
  (if (plusp n)
      (%character-offset-positive marker n)
      (%character-offset-negative marker (- n))))

(defun points-to-string (start-marker end-marker)
  (region-string (marker-point start-marker)
                 (marker-point end-marker)))
