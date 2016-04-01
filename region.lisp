;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(region-beginning
          region-end
          region-string
          region-count
          copy-region
          kill-region
          delete-region
          apply-region-lines))

(defun region-beginning ()
  (let ((point1 (current-point))
        (point2 (marker-point (buffer-mark-marker))))
    (if (point< point1 point2)
        point1
        point2)))

(defun region-end ()
  (let ((point1 (current-point))
        (point2 (marker-point (buffer-mark-marker))))
    (if (point< point1 point2)
        point2
        point1)))

(defun region-lines (begin end)
  (when (point< end begin)
    (rotatef begin end))
  (with-points (((linum1 col1) begin)
                ((linum2 col2) end))
    (let ((lines
           (buffer-take-lines (window-buffer)
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

(define-key *global-keymap* (kbd "M-w") 'copy-region)
(define-command copy-region (begin end) ("r")
  (let ((lines (region-lines begin end)))
    (with-kill ()
      (kill-push lines)))
  (buffer-mark-cancel (window-buffer))
  (minibuf-print "region copied")
  t)

(define-key *global-keymap* (kbd "C-w") 'kill-region)
(define-command kill-region (begin end) ("r")
  (point-set begin)
  (prog1 (delete-char (region-count begin end))
    (buffer-mark-cancel (window-buffer))))

(defun delete-region (begin end)
  (let ((*kill-disable-p* t))
    (kill-region begin end)))

(defun apply-region-lines (begin end fn)
  (point-set begin)
  (do () ((point<= end (current-point)))
    (let ((linum (window-current-linum)))
      (beginning-of-line)
      (funcall fn)
      (when (= linum (window-current-linum))
        (unless (next-line 1)
          (return))))))
