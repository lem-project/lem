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
          after-property
          before-property
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

(defun line-start (marker)
  (setf (marker-charpos marker) 0)
  marker)

(defun line-end (marker)
  (setf (marker-charpos marker)
        (buffer-line-length (marker-buffer marker)
                            (marker-linum marker)))
  marker)

(defun bolp ()
  (zerop (current-charpos)))

(defun eolp ()
  (= (current-charpos)
     (buffer-line-length
      (current-buffer)
      (current-linum))))

(defun bobp ()
  (and (first-line-p (current-marker)) (bolp)))

(defun eobp ()
  (and (last-line-p (current-marker)) (eolp)))

(defun insert-char (c &optional (n 1))
  (dotimes (_ n t)
    (insert-char/marker (buffer-point-marker (current-buffer)) c)
    (shift-position 1)))

(defun insert-string (str)
  (insert-string/marker (buffer-point-marker (current-buffer)) str)
  (shift-position (length str)))

(defun insert-newline (&optional (n 1))
  (dotimes (_ n)
    (insert-char/marker (buffer-point-marker (current-buffer))
                        #\newline))
  (forward-line n))

(defun delete-char (n &optional killp)
  (when (minusp n)
    (setf n (- n))
    (unless (shift-position (- n))
      (return-from delete-char nil)))
  (if (eobp)
      nil
      (let ((string (delete-char/marker
                     (buffer-point-marker
                      (current-buffer))
                     n)))
        (when killp
          (kill-push string))
        t)))

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
  (beginning-of-line)
  (if (plusp n)
      (dotimes (_ n t)
        (when (last-line-p (current-marker))
          (end-of-line)
          (return))
        (incf (current-linum)))
      (dotimes (_ (- n) t)
        (when (first-line-p (current-marker))
          (return))
        (decf (current-linum)))))

(defun %shift-position-positive (n)
  (loop
    (when (< n 0)
      (return nil))
    (let* ((length (1+ (buffer-line-length (current-buffer) (current-linum))))
           (w (- length (current-charpos))))
      (when (< n w)
        (set-charpos (+ n (current-charpos)))
        (return t))
      (decf n w)
      (unless (forward-line 1)
        (return nil)))))

(defun %shift-position-negative (n)
  (loop
    (when (< n 0)
      (return nil))
    (when (<= n (current-charpos))
      (set-charpos (- (current-charpos) n))
      (return t))
    (decf n (1+ (current-charpos)))
    (cond ((first-line-p (current-marker))
           (beginning-of-line)
           (return nil))
          (t
           (forward-line -1)
           (end-of-line)))))

(defun shift-position (n)
  (cond ((< 0 n)
         (%shift-position-positive n))
        (t
         (setf n (- n))
         (%shift-position-negative n))))

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
  (buffer-get-char (current-buffer)
                   (current-linum)
                   (current-charpos)))

(defun preceding-char ()
  (cond
    ((bobp)
     nil)
    ((bolp)
     (buffer-get-char (current-buffer)
                      (1- (current-linum))
                      (buffer-line-length (current-buffer)
                                          (1- (current-linum)))))
    (t
     (buffer-get-char (current-buffer)
                      (current-linum)
                      (1- (current-charpos))))))

(defun char-after (&optional (n 0))
  (if (zerop n)
      (following-char)
      (let ((point (current-point)))
        (if (shift-position n)
            (prog1 (following-char)
              (shift-position (- n)))
            (progn
              (point-set point)
              nil)))))

(defun char-before (&optional (n 1))
  (if (= n 1)
      (preceding-char)
      (let ((point (current-point)))
        (if (shift-position (- (1- n)))
            (prog1 (preceding-char)
              (shift-position (1- n)))
            (progn
              (point-set point)
              nil)))))

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

(defun after-property (property-name &optional (n 1))
  (save-excursion
    (shift-position n)
    (%syntax-pos-property (current-charpos) property-name)))

(defun before-property (property-name &optional (n 1))
  (save-excursion
    (shift-position (- (1- n)))
    (%syntax-pos-property (current-charpos) property-name)))

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
