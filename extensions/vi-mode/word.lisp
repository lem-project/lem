(defpackage :lem-vi-mode/word
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/options
                :vi-option-raw-value)
  (:export :forward-word-begin
           :forward-word-end
           :backward-word-begin
           :word-char-p
           :word-char-type
           :broad-word-char-type
           :blank-char-p
           :a-range-of
           :inner-range-of))
(in-package :lem-vi-mode/word)

(defun word-char-p (char)
  (funcall (cdr (vi-option-raw-value "iskeyword"))
           char))

(defun blank-char-p (char)
  (and (member char '(#\Newline #\Space #\Tab))
       t))

(defun non-blank-char-p (char)
  (not (blank-char-p char)))

(defun word-char-type (char)
  (when char
    (cond
      ((word-char-p char) :word)
      ((blank-char-p char) :blank)
      (t :non-word))))

(defun broad-word-char-type (char)
  (when char
    (cond
      ((blank-char-p char) :blank)
      (t :word))))

(defun forward-word-begin (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    (let ((type (funcall char-type-fn (point-char))))
      (skip-chars-forward point
                          (lambda (char)
                            (eq type (funcall char-type-fn char)))))
    (when (blank-char-p (point-char))
      (skip-chars-forward point #'blank-char-p))))

(defun forward-word-end (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    ;; Boundary
    (unless (eq (funcall char-type-fn (point-char))
                (funcall char-type-fn (character-at point 1)))
      (character-offset point 1))
    (when (blank-char-p (point-char))
      (skip-chars-forward point #'blank-char-p))
    (let ((type (funcall char-type-fn (point-char))))
      (or (zerop
           (skip-chars-forward point
                               (lambda (char)
                                 (eq type (funcall char-type-fn char)))))
          (character-offset point -1)))))

(defun backward-word-begin (char-type-fn &optional (point (current-point)))
  (flet ((point-char () (character-at point)))
    ;; Boundary
    (unless (eq (funcall char-type-fn (point-char))
                (funcall char-type-fn (character-at point -1)))
      (character-offset point -1))
    (when (blank-char-p (point-char))
      (skip-chars-backward point #'blank-char-p)
      (character-offset point -1))
    (let ((type (funcall char-type-fn (point-char))))
      (skip-chars-backward point
                           (lambda (char)
                             (eq type (funcall char-type-fn char)))))))

(defun a-range-of (char-type-fn count &optional (point (current-point)))
  (let* ((char-type (funcall char-type-fn (character-at point)))
         (initial-char-type char-type)
         (check-fn (lambda (c) (eq (funcall char-type-fn c) char-type))))
    (flet ((move-forward (p)
             (loop with buffer-end = (buffer-end-point (point-buffer p))
                   if (or (point= p buffer-end)
                          (char= (character-at p) #\Newline))
                   do (return nil)
                   else if (funcall check-fn (character-at p))
                   do (character-offset p 1)
                   else
                   do (return t)))
           (move-backward (p)
             (loop while (and (< 0 (point-charpos p))
                              (funcall check-fn (character-at p -1)))
                   do (character-offset p -1))
             p))
      (with-point ((start point)
                   (end point))
        (if (eq char-type :blank)
            (progn
              (skip-chars-backward start #'blank-char-p)
              (skip-chars-forward end #'blank-char-p))
            (move-backward start))
        (dotimes (i count)
          (when (or (point= end (buffer-end-point (point-buffer point)))
                    (char= (character-at end) #\Newline))
            (return-from a-range-of nil))
          (skip-chars-forward end #'blank-char-p)
          (setf char-type
                (funcall char-type-fn (character-at end)))
          (move-forward end))
        (unless (eq initial-char-type :blank)
          (if (or (point= end (buffer-end-point (point-buffer point)))
                  (char= (character-at end) #\Newline))
              (skip-chars-backward start #'blank-char-p)
              (skip-chars-forward end #'blank-char-p)))
        (values start end)))))

(defun inner-range-of (char-type-fn count &optional (point (current-point)))
  (let* ((char-type (funcall char-type-fn (character-at point)))
         (check-fn (lambda (c) (eq (funcall char-type-fn c) char-type))))
    (flet ((move-forward (p)
             (loop with buffer-end = (buffer-end-point (point-buffer p))
                   while (and (point/= p buffer-end)
                              (char/= (character-at p) #\Newline)
                              (funcall check-fn (character-at p)))
                   do (character-offset p 1))
             p)
           (move-backward (p)
             (loop while (and (< 0 (point-charpos p))
                              (funcall check-fn (character-at p -1)))
                   do (character-offset p -1))
             p))
      (with-point ((start point)
                   (end point))
        (move-backward start)
        (dotimes (i count)
          (when (or (point= end (buffer-end-point (point-buffer point)))
                    (char= (character-at end) #\Newline))
            (return-from inner-range-of nil))
          (move-forward end)
          (setf char-type (funcall char-type-fn (character-at end))))
        (values start end)))))
