(in-package :lem)

(export '(first-line-p
          last-line-p
          bolp
          eolp
          bobp
          eobp
          insert-char
          insert-string
          insert-newline
          delete-char
          set-charpos
          beginning-of-buffer
          end-of-buffer
          beginning-of-line
          end-of-line
          beginning-of-line-point
          end-of-line-point
          goto-position
          forward-line
          shift-position
          mark-point
          current-line-string
          following-char
          preceding-char
          char-after
          char-before
          blank-line-p
          delete-while-whitespaces
          skip-chars-forward
          skip-chars-backward
          get-property
          put-property
          remove-property
          put-attribute
          insert-string-with-attribute
          following-property
          preceding-property
          forward-search-property-end
          backward-search-property-start
          current-column
          move-to-column
          point-to-offset
          shift-point))

(defun first-line-p (marker)
  (<= (marker-linum marker) 1))

(defun last-line-p (marker)
  (<= (buffer-nlines (marker-buffer marker))
      (marker-linum marker)))

(defun start-line-p (marker)
  (zerop (marker-charpos marker)))

(defun end-line-p (marker)
  (= (marker-charpos marker)
     (buffer-line-length (marker-buffer marker)
                         (marker-linum marker))))

(defun start-buffer-p (marker)
  (and (first-line-p marker)
       (start-line-p marker)))

(defun end-buffer-p (marker)
  (and (last-line-p marker)
       (end-line-p marker)))

(defun line-start (marker)
  (setf (marker-charpos marker) 0)
  marker)

(defun line-end (marker)
  (setf (marker-charpos marker)
        (buffer-line-length (marker-buffer marker)
                            (marker-linum marker)))
  marker)

(defun buffer-start (marker)
  (move-point marker (buffer-start-marker (marker-buffer marker))))

(defun buffer-end (marker)
  (move-point marker (buffer-end-marker (marker-buffer marker))))

(defun move-point (marker new-marker)
  (setf (marker-point marker)
        (marker-point new-marker)))

(defun same-line-p (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (= (marker-linum marker1)
     (marker-linum marker2)))

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
    (cond ((first-line-p marker)
           (return (values (line-start marker) nil)))
          (t
           (line-offset marker -1)
           (line-end marker)))))

(defun character-offset (marker n)
  (if (plusp n)
      (%character-offset-positive marker n)
      (%character-offset-negative marker (- n))))

(defun character-at (marker &optional (offset 0))
  (if (zerop offset)
      (buffer-get-char (marker-buffer marker)
                       (marker-linum marker)
                       (marker-charpos marker))
      (with-marker ((temp-marker marker))
        (when (nth-value 1 (character-offset temp-marker offset))
          (character-at temp-marker 0)))))

(defun insert-char-at (marker char &optional (n 1))
  (loop :repeat n :do (insert-char/marker marker char))
  t)

(defun insert-string-at (marker string)
  (insert-string/marker marker string)
  t)

(defun delete-char-at (marker &optional (n 1) killp)
  (when (minusp n)
    (unless (nth-value 1 (character-offset marker n))
      (return-from delete-char-at nil))
    (setf n (- n)))
  (unless (end-buffer-p marker)
    (let ((string (delete-char/marker marker n)))
      (when killp
        (kill-push string))
      t)))

(defun text-property-at (marker key &optional (offset 0))
  (if (zerop offset)
      (buffer-get-property (marker-buffer marker)
                           (marker-point marker)
                           key)
      (with-marker ((temp-marker marker))
        (when (nth-value 1 (character-offset temp-marker offset))
          (text-property-at marker key 0)))))

(defun put-text-property (start-marker end-marker key value)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (buffer-put-property (marker-buffer start-marker)
                       (marker-point start-marker)
                       (marker-point end-marker)
                       key
                       value))

(defun remove-text-property (start-marker end-marker key)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (buffer-remove-property (marker-buffer start-marker)
                          (marker-point start-marker)
                          (marker-point end-marker)
                          key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bolp ()
  (start-line-p (current-marker)))

(defun eolp ()
  (end-line-p (current-marker)))

(defun bobp ()
  (start-buffer-p (current-marker)))

(defun eobp ()
  (end-buffer-p (current-marker)))

(defun insert-char (c &optional (n 1))
  (insert-char-at (current-marker) c n)
  (shift-position n)
  t)

(defun insert-newline (&optional (n 1))
  (insert-char-at (current-marker) #\newline n)
  (forward-line n)
  t)

(defun insert-string (string)
  (insert-string-at (current-marker) string)
  (shift-position (length string)))

(defun delete-char (n &optional killp)
  (delete-char-at (current-marker) n killp))

(defun set-charpos (pos)
  (setf (current-charpos) pos))

(defun beginning-of-buffer ()
  (point-set (point-min)))

(defun end-of-buffer ()
  (point-set (point-max)))

(defun beginning-of-line ()
  (line-start (current-marker))
  t)

(defun end-of-line ()
  (line-end (current-marker))
  t)

(defun beginning-of-line-point (&optional (linum (current-linum)))
  (setf linum (round-linum (current-buffer) linum))
  (make-point linum 0))

(defun end-of-line-point (&optional (linum (current-linum)))
  (setf linum (round-linum (current-buffer) linum))
  (make-point linum
              (buffer-line-length
               (current-buffer)
               linum)))

(defun goto-position (position)
  (check-type position (integer 1 *))
  (beginning-of-buffer)
  (shift-position position))

(defun forward-line (&optional (n 1))
  (nth-value 1 (line-offset (current-marker) n)))

(defun shift-position (n)
  (nth-value 1 (character-offset (current-marker) n)))

(defun check-marked ()
  (unless (buffer-mark-marker (current-buffer))
    (editor-error "Not mark in this buffer")))

(defun mark-point ()
  (when (buffer-mark-marker (current-buffer))
    (marker-point (buffer-mark-marker (current-buffer)))))

(defun (setf mark-point) (point)
  (let ((buffer (current-buffer)))
    (setf (buffer-mark-p buffer) t)
    (if (buffer-mark-marker buffer)
        (setf (marker-point (buffer-mark-marker buffer))
              point)
        (setf (buffer-mark-marker buffer)
              (make-marker buffer point :name "mark")))))

(defun current-line-string ()
  (buffer-line-string (current-buffer)
                      (current-linum)))

(defun following-char ()
  (character-at (current-marker)))

(defun preceding-char ()
  (character-at (current-marker) -1))

;; char-after, char-beforeは引数がemacsと違っていて紛らわしいので変えるつもり
(defun char-after (&optional (n 0))
  (character-at (current-marker) n))

(defun char-before (&optional (n 1))
  (character-at (current-marker)
                (- n)))

(defun delete-while-whitespaces (&optional ignore-newline-p use-kill-ring)
  (let ((n (skip-chars-forward
            (if ignore-newline-p
                '(#\space #\tab)
                '(#\space #\tab #\newline)))))
    (delete-char (- n) use-kill-ring)))

(defun blank-line-p ()
  (let ((string (current-line-string))
        (eof-p (last-line-p (current-marker))))
    (when (string= "" (string-trim '(#\space #\tab) string))
      (+ (length string)
         (if eof-p 0 1)))))

(defun skip-chars-aux (pred not-p step-char at-char)
  (flet ((test (pred not-p char)
           (if (if (consp pred)
                   (member char pred)
                   (funcall pred char))
               (not not-p)
               not-p)))
    (let ((count 0))
      (loop
        (unless (test pred not-p (funcall at-char))
          (return count))
        (if (funcall step-char)
            (incf count)
            (return count))))))

(defun skip-chars-forward (pred &optional not-p)
  (skip-chars-aux pred
                  not-p
                  (lambda () (shift-position 1))
                  #'following-char))

(defun skip-chars-backward (pred &optional not-p)
  (skip-chars-aux pred
                  not-p
                  (lambda () (shift-position -1))
                  #'preceding-char))

(defun get-property (point key)
  (buffer-get-property (current-buffer) point key))

(defun put-property (start end key value)
  (buffer-put-property (current-buffer) start end key value))

(defun remove-property (start end key)
  (buffer-remove-property (current-buffer) start end key))

(defun put-attribute (start end attribute)
  (buffer-put-property (current-buffer) start end :attribute attribute))

(defun insert-string-with-attribute (string attribute)
  (let ((start (current-point)))
    (insert-string string)
    (put-attribute start (current-point) attribute)))

(defun following-property (property-name)
  (%syntax-pos-property (current-charpos) property-name))

(defun preceding-property (property-name)
  (save-excursion
    (shift-position -1)
    (%syntax-pos-property (current-charpos) property-name)))

(defun forward-search-property-end (property-name &optional limit)
  (let ((first-value (following-property property-name))
        (first-point (current-point)))
    (loop
      (unless (eq first-value (following-property property-name))
        (return t))
      (unless (shift-position 1)
        (setf (current-point) first-point)
        (return nil))
      (when (and limit (point<= limit (current-point)))
        (setf (current-point) first-point)
        (return nil)))))

(defun backward-search-property-start (property-name &optional limit)
  (let ((first-value (preceding-property property-name))
        (first-point (current-point)))
    (loop
      (unless (eq first-value (preceding-property property-name))
        (return t))
      (unless (shift-position -1)
        (setf (current-point) first-point)
        (return nil))
      (when (and limit (point<= (current-point) limit))
        (setf (current-point) first-point)
        (return nil)))))

(defun current-column ()
  (string-width (current-line-string)
                0
                (current-charpos)))

(defun move-to-column (column &optional force)
  (check-type column (integer 0 #.most-positive-fixnum))
  (end-of-line)
  (let ((current-column (current-column)))
    (cond ((< column current-column)
           (set-charpos (wide-index (current-line-string) column))
           column)
          (force
           (insert-char #\space (- column current-column))
           (end-of-line)
           column)
          (t
           (end-of-line)
           current-column))))

(defun point-to-offset (point &optional (buffer (current-buffer) bufferp))
  (check-type point point)
  (check-type buffer buffer)
  (save-excursion
    (when bufferp
      (setf (current-buffer) buffer))
    (point-set (point-min))
    (let ((end-linum (point-linum point))
          (end-charpos (point-charpos point))
          (offset 0))
      (loop :repeat (1- end-linum)
            :for linum :from 1
            :do (incf offset
                      (1+ (buffer-line-length (current-buffer)
                                              linum))))
      (+ offset end-charpos))))

(defun shift-point (point offset &optional (buffer (current-buffer)))
  (with-current-buffer (buffer point)
    (shift-position offset)
    (current-point)))
