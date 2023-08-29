(defpackage :lem-vi-mode/text-objects
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :make-range
                :range-beginning
                :range-end
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

           :word-object
           :double-quoted-object))
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
            (move-backward point)))))
  point)

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
        ((and (null direction)
              (not (member (character-at beg) '(#\Space #\Tab))))
         (slurp-object object beg :backward)))
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
              (unless direction
                (skip-chars-backward beg '(#\Space #\Tab)))
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

(defmethod inner-range-of (object state count)
  (declare (ignore state))
  (with-point ((beg (current-point))
               (end (current-point)))
    (if (member (character-at beg) '(#\Space #\Tab #\Newline))
        (skip-chars-backward beg '(#\Space #\Tab #\Newline))
        (slurp-object object beg :backward))
    (dotimes (i count)
      (when (or (point= end (buffer-end-point (point-buffer end)))
                (char= (character-at end) #\Newline))
        (error 'text-object-abort
               :range (make-range beg end)))
      (if (member (character-at end) '(#\Space #\Tab #\Newline))
          (skip-chars-forward end '(#\Space #\Tab))
          (slurp-object object end :forward)))
    (make-range beg end)))

(defmethod inner-range-of (object (state visual) count)
  (destructuring-bind (beg end)
      (visual-range)
    (let ((direction (cond
                       ((point< beg end) :forward)
                       ((point< end beg) :backward)))
          (buffer (point-buffer end)))
      (when (null direction)
        (if (member (character-at beg) '(#\Space #\Tab #\Newline))
            (skip-chars-backward beg '(#\Space #\Tab #\Newline))
            (slurp-object object beg :backward)))
      (if (or (null direction)
              (eq direction :forward))
          (progn
            (dotimes (i count)
              (when (or (point= end (buffer-end-point buffer))
                        (char= (character-at end) #\Newline))
                (error 'text-object-abort
                       :range (make-range beg end)))
              (slurp-object object end :forward)))
          (progn
            (slurp-object object beg :forward)
            (dotimes (i count)
              (when (or (point= end (buffer-start-point buffer))
                        (char= (character-at end -1) #\Newline))
                (error 'text-object-abort
                       :range (make-range beg end)))
              (slurp-object object end :backward)))))
    (make-range beg end)))

(defmethod slurp-object ((object quoted-text-object) point direction)
  (with-slots (quote-char escape-char) object
    (ecase direction
      (:backward
       (when (char= (character-at point) quote-char)
         (character-offset point -1))
       (loop
         (skip-chars-backward point (lambda (c) (char/= c quote-char)))
         (let ((prev-char (character-at point -1)))
           (cond
             ;; No quote-char found
             ((null prev-char)
              (keyboard-quit))
             ;; Skip escaped quote-char
             ((and escape-char
                   (char= prev-char escape-char)))
             ;; Successfully found
             (t
              (character-offset point -1)
              (return))))))
      (:forward
       (when (char= (character-at point) quote-char)
         (character-offset point 1))
       (loop
         (skip-chars-forward point (lambda (c) (char/= c quote-char)))
         (let ((next-char (character-at point)))
           (cond
             ;; No quote-char found
             ((null next-char)
              (keyboard-quit))
             ;; Skip escaped quote-char
             ((and escape-char
                   (char= (character-at point -1) escape-char)))
             ;; Successfully found
             (t
              (character-offset point 1)
              (return)))))))))

(defmethod a-range-of ((object quoted-text-object) (state visual) count)
  (with-slots (quote-char escape-char) object
    (destructuring-bind (beg end)
        (visual-range)
      (let* ((region-string (points-to-string beg end))
             (len (length region-string))
             (quote-count 0)
             (direction (cond
                          ((point< beg end) :forward)
                          ((point< end beg) :backward))))
        (when (/= len 0)
          (do ((i 0 (1+ i)))
              ((<= len i))
            (let ((char (aref region-string i)))
              (cond
                ((char= char quote-char)
                 (incf quote-count))
                ((char= char escape-char)
                 (incf i))))))
        (if (= (mod quote-count 2) 1)
            ;; Incomplete object in selected region
            (progn
              (if (eq direction :backward)
                  (progn
                    (skip-chars-backward end (lambda (c) (char/= c quote-char)))
                    (character-offset end -1)
                    (skip-chars-backward end '(#\Space #\Tab)))
                  (progn
                    (skip-chars-forward end (lambda (c) (char/= c quote-char)))
                    (character-offset end 1)
                    (skip-chars-forward end '(#\Space #\Tab))))
              (make-range beg end))
            (call-next-method))))))

(defmethod inner-range-of ((object quoted-text-object) state count)
  (declare (ignore state count))
  (let ((range (call-next-method)))
    (character-offset (range-beginning range) 1)
    (character-offset (range-end range) -1)
    range))

(defclass word-object (function-text-object) ()
  (:default-initargs
   :function #'word-char-type))

(defclass double-quoted-object (quoted-text-object) ()
  (:default-initargs
   :quote-char #\"))

(defmethod a-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))
