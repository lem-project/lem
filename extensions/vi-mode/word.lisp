(defpackage :lem-vi-mode/word
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :make-range)
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

(defun a-range-of (char-type-fn count start end)
  (let* ((start (or start (copy-point (current-point))))
         (end (or end (copy-point (current-point))))
         (direction (cond
                      ((point< start end) :forward)
                      ((point< end start) :backward)))
         (char-type (if (eq direction :backward)
                        (funcall char-type-fn (character-at end -1))
                        (funcall char-type-fn (character-at end))))
         (initial-char-type char-type)
         (check-fn (lambda (c) (eq (funcall char-type-fn c) char-type)))
         (buffer (point-buffer end))
         aborted)
    (labels ((%move-forward (p)
               (loop with buffer-end = (buffer-end-point buffer)
                     if (or (point= p buffer-end)
                            (char= (character-at p) #\Newline))
                     do (return nil)
                     else if (funcall check-fn (character-at p))
                     do (character-offset p 1)
                     else
                     do (return t)))
             (%move-backward (p)
               (loop while (and (< 0 (point-charpos p))
                                (funcall check-fn (character-at p -1)))
                     do (character-offset p -1))
               p)
             (move-forward ()
               (if (eq direction :backward)
                   (%move-backward end)
                   (%move-forward end)))
             (move-backward ()
               (if (eq direction :backward)
                   (%move-forward start)
                   (%move-backward start)))
             (skip-spaces-forward (&optional include-newline)
               (if (eq direction :backward)
                   (skip-chars-backward end (if include-newline
                                                #'blank-char-p
                                                '(#\Space #\Tab)))
                   (skip-chars-forward end (if include-newline
                                               #'blank-char-p
                                               '(#\Space #\Tab)))))
             (skip-spaces-backward ()
               (if (eq direction :backward)
                   (skip-chars-forward start '(#\Space #\Tab))
                   (skip-chars-backward start '(#\Space #\Tab))))
             (offset ()
               (if (eq direction :backward) -1 0))
             (dead-end-p ()
               (or (point= end (if (eq direction :backward)
                                   (buffer-start-point buffer)
                                   (buffer-end-point buffer)))
                   (char= (character-at end (offset))
                          #\Newline))))
      (if (eq char-type :blank)
          (progn
            (when (null direction)
              (skip-chars-backward start #'blank-char-p))
            (skip-spaces-forward t))
          (move-backward))
      (block abort
        (dotimes (i count)
          (skip-spaces-forward t)
          (when (dead-end-p)
            (setf aborted t)
            (return-from abort))
          (setf char-type
                (funcall char-type-fn (character-at end (offset))))
          (move-forward))
        (unless (eq initial-char-type :blank)
          (if (and (not aborted)
                   (dead-end-p))
              (skip-spaces-backward)
              (skip-spaces-forward))))
      (values (make-range start end) aborted))))

(defun inner-range-of (char-type-fn count start end)
  (declare (ignore direction))
  (let* ((start (or start (copy-point (current-point))))
         (end (or end (copy-point (current-point))))
         (direction (cond
                      ((point< start end) :forward)
                      ((point< end start) :backward)))
         (char-type (funcall char-type-fn (character-at end)))
         (check-fn (lambda (c) (eq (funcall char-type-fn c) char-type)))
         (buffer (point-buffer start))
         (aborted nil))
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
      (block abort
        (if (or (null direction)
                (eq direction :forward))
            (progn
              (move-backward start)
              (dotimes (i count)
                (when (or (point= end (buffer-end-point buffer))
                          (char= (character-at end) #\Newline))
                  (setf aborted t)
                  (return-from abort))
                (move-forward end)
                (setf char-type (funcall char-type-fn (character-at end)))))
            (progn
              (move-forward start)
              (dotimes (i count)
                (when (or (point= end (buffer-start-point buffer))
                          (char= (character-at end -1) #\Newline))
                  (setf aborted t)
                  (return-from abort))
                (move-backward end)
                (setf char-type (funcall char-type-fn (character-at end -1)))))))
      (values (make-range start end) aborted))))
