;; -*- Mode: LISP; Package: LEM -*-

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

(defun region-lines (begin end)
  (when (point< end begin)
    (rotatef begin end))
  (with-points (((linum1 col1) begin)
                ((linum2 col2) end))
    (let ((lines
           (buffer-take-lines (current-buffer)
                              linum1
                              (1+ (- linum2 linum1)))))
      (if (= linum1 linum2)
          (list (subseq (car lines) col1 col2))
          (let ((acc
                 (list (subseq (car lines) col1))))
            (do ((rest (cdr lines) (cdr rest)))
                ((null (cdr rest))
                 (when rest
                   (push (subseq (car rest) 0 col2) acc)))
              (push (car rest) acc))
            (nreverse acc))))))

(defun region-string (begin end)
  (join (string #\newline) (region-lines begin end)))

(defun region-count (begin end)
  (let ((count 0))
    (do ((lines (region-lines begin end) (cdr lines)))
        ((null (cdr lines))
         (incf count (length (car lines))))
      (incf count (1+ (length (car lines)))))
    count))

(defun delete-region (begin end)
  (when (point< end begin)
    (rotatef begin end))
  (point-set begin)
  (prog1 (delete-char (region-count begin end) nil)
    (buffer-mark-cancel (current-buffer))))

(defun apply-region-lines (begin end fn)
  (point-set begin)
  (do () ((point<= end (current-point)))
    (let ((linum (current-linum)))
      (beginning-of-line)
      (funcall fn)
      (when (= linum (current-linum))
        (unless (forward-line 1)
          (return))))))
