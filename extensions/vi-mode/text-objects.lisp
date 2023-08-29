(defpackage :lem-vi-mode/text-objects
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :make-range
                :text-object-abort)
  (:import-from :lem-vi-mode/visual
                :visual
                :visual-p
                :visual-char-p
                :visual-range
                :vi-visual-char)
  (:import-from :lem-vi-mode/word
                :word-char-type)
  (:export :text-object
           :function-text-object
           :surrounded-text-object
           :quoted-text-object
           :a-range-of
           :inner-range-of

           :word-object))
(in-package :lem-vi-mode/text-objects)

(defclass text-object () ())

(defclass function-text-object (text-object)
  ((function :type function
             :initarg :function)))

(defclass block-text-object (text-object)
  ((open-char :type character
              :initarg :open-char)
   (close-char :type character
               :initarg :close-char)
   (escape-char :type (or null character)
                :initarg :escape-char
                :initform #\\)))

(defclass quoted-text-object (text-object)
  ((quote-char :type character
               :initarg :quote-char)
   (escape-char :type (or null character)
                :initarg :escape-char
                :initform #\\)))

(defgeneric a-range-of (object state count)
  (:method ((object symbol) state count)
    (a-range-of (make-instance object) state count)))

(defgeneric inner-range-of (object state count)
  (:method ((object symbol) state count)
    (inner-range-of (make-instance object) state count)))

(defun target-region ()
  (if (visual-p)
      (visual-range)
      (list (copy-point (current-point))
            (copy-point (current-point)))))

(defmethod slurp-object ((object function-text-object) point direction)
  (check-type direction (member :forward :backward))
  (with-slots (function) object
    (let* ((char-type (funcall function (character-at point)))
           (check-fn (lambda (c) (eql char-type (funcall function c))))
           (buffer (point-buffer point)))
      (labels ((move-forward (p)
                 (loop with buffer-end = (buffer-end-point buffer)
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
        (if (eq direction :forward)
            (move-forward point)
            (move-backward point))))))

(defun a-range-with-direction (object count beg end direction)
  (check-type direction (member :forward :backward))
  (dotimes (i count)
    (if (eq direction :backward)
        (skip-chars-backward end '(#\Space #\Tab #\Newline))
        (skip-chars-forward end '(#\Space #\Tab #\Newline)))
    (when (or (and (eq direction :backward)
                   (or (point= end (buffer-start-point (point-buffer end)))
                       (char= (character-at end -1) #\Newline)))
              (and (eq direction :forward)
                   (or (point= end (buffer-end-point (point-buffer end)))
                       (char= (character-at end) #\Newline))))
      (error 'text-object-abort
             :range (make-range beg end)))
    (slurp-object object end direction))
  (make-range beg end))

(defmethod a-range-of (object (state visual) count)
  (destructuring-bind (beg end)
      (visual-range)
    (let ((direction (cond
                       ((point< beg end) :forward)
                       ((point< end beg) :backward)))
          (initial-blank (member (character-at end) '(#\Space #\Tab #\Newline))))
      (cond
        (initial-blank
         (when (or (null direction)
                   (eq direction :forward))
           (skip-chars-forward end '(#\Space #\Tab)))
         (when (or (null direction)
                   (eq direction :backward))
           (skip-chars-backward beg '(#\Space #\Tab))))
        ((not (member (character-at beg) '(#\Space #\Tab)))
         (slurp-object object beg (if (eq direction :backward)
                                      :forward
                                      :backward))))
      (prog1
          (a-range-with-direction object count beg end (or direction :forward))
        (unless initial-blank
          (if (or (and (eq direction :backward)
                       (or (point= end (buffer-start-point (point-buffer end)))
                           (char= (character-at end -1) #\Newline)))
                  (and (or (null direction)
                           (eq direction :forward))
                       (or (point= end (buffer-end-point (point-buffer end)))
                           (char= (character-at end) #\Newline))))
              (skip-chars-backward beg '(#\Space #\Tab))
              (skip-chars-forward end '(#\Space #\Tab))))))))

(defmethod a-range-of (object state count)
  (declare (ignore state))
  (with-point ((beg (current-point))
               (end (current-point)))
    (let ((initial-blank (member (character-at end) '(#\Space #\Tab #\Newline))))
      (if initial-blank
          (progn
            (skip-chars-forward end '(#\Space #\Tab))
            (skip-chars-backward beg '(#\Space #\Tab)))
          (slurp-object object beg :backward))
      (prog1
          (a-range-with-direction object count beg end :forward)
        (unless initial-blank
          (if (or (point= end (buffer-end-point (point-buffer end)))
                  (char= (character-at end) #\Newline))
              (skip-chars-backward beg '(#\Space #\Tab))
              (skip-chars-forward end '(#\Space #\Tab))))))))

(defmethod inner-range-of ((object function-text-object) state count)
  (declare (ignore state))
  (with-slots (function) object
    (destructuring-bind (beg end)
        (target-region)
      (let* ((direction (cond
                          ((point< beg end) :forward)
                          ((point< end beg) :backward)))
             (char-type (funcall function (character-at end)))
             (check-fn (lambda (c) (eq (funcall function c) char-type)))
             (buffer (point-buffer beg)))
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
          (if (or (null direction)
                  (eq direction :forward))
              (progn
                (move-backward beg)
                (dotimes (i count)
                  (when (or (point= end (buffer-end-point buffer))
                            (char= (character-at end) #\Newline))
                    (error 'text-object-abort
                           :range (make-range beg end)))
                  (move-forward end)
                  (setf char-type (funcall function (character-at end)))))
              (progn
                (move-forward beg)
                (dotimes (i count)
                  (when (or (point= end (buffer-start-point buffer))
                            (char= (character-at end -1) #\Newline))
                    (error 'text-object-abort
                           :range (make-range beg end)))
                  (move-backward end)
                  (setf char-type (funcall function (character-at end -1))))))
          (make-range beg end))))))

(defmethod a-range-of ((object quoted-text-object) state count)
  (declare (ignore state count))
  (with-slots (quote-char escape-char) object
    (destructuring-bind (beg end)
        (target-region)
      (let ((direction (cond
                         ((point< beg end) :forward)
                         ((point< end beg) :backward))))
        (loop
          (skip-chars-backward beg (lambda (c) (char/= c quote-char)))
          (let ((prev-char (character-at beg -1)))
            (cond
              ;; No quote-char found
              ((null prev-char)
               (keyboard-quit))
              ;; Skip escaped quote-char
              ((and escape-char
                    (char= prev-char escape-char)))
              ;; Successfully found
              (t
               (character-offset beg -1)
               (return)))))
        (loop
          (skip-chars-forward end (lambda (c) (char/= c quote-char)))
          (let ((next-char (character-at end)))
            (cond
              ;; No quote-char found
              ((null next-char)
               (keyboard-quit))
              ;; Skip escaped quote-char
              ((and escape-char
                    (char= (character-at end -1) escape-char)))
              ;; Successfully found
              (t
               (character-offset end 1)
               (return)))))
        (if (member (character-at end) '(#\Space #\Tab))
            (skip-chars-forward end '(#\Space #\Tab))
            (skip-chars-backward beg '(#\Space #\Tab))))
      (make-range beg end))))

(defmethod a-range-of ((object quoted-text-object) (state visual) count)
  (declare (ignore count))
  (with-slots (open-char escape-char) object
    (destructuring-bind (beg end)
        (visual-range)
      (let ((direction (cond
                         ((point< beg end) :forward)
                         ((point< end beg) :backward))))
        (loop
          (skip-chars-backward beg (lambda (c) (char/= c open-char)))
          (unless (char= (character-at beg -1) escape-char)
            (character-offset beg -1)
            (return)))
        (loop
          (skip-chars-forward end (lambda (c) (char/= c open-char)))
          (unless (char= (character-at end -1) escape-char)
            (character-offset end 1)
            (return)))
        (if (member (character-at end) '(#\Space #\Tab))
            (skip-chars-forward end '(#\Space #\Tab))
            (skip-chars-backward beg '(#\Space #\Tab))))
      (make-range beg end))))

(defclass word-object (function-text-object) ()
  (:default-initargs
   :function #'word-char-type))

(defmethod a-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))
