(in-package :lem)

(export '(head-line-p
          tail-line-p
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
          put-attribute
          remove-attribute
          current-column
          move-to-column))

(defun head-line-p ()
  (<= (current-linum) 1))

(defun tail-line-p ()
  (<= (buffer-nlines (current-buffer))
      (current-linum)))

(defun bolp ()
  (zerop (current-charpos)))

(defun eolp ()
  (= (current-charpos)
     (buffer-line-length
      (current-buffer)
      (current-linum))))

(defun bobp ()
  (and (head-line-p) (bolp)))

(defun eobp ()
  (and (tail-line-p) (eolp)))

(defun insert-char (c &optional (n 1))
  (dotimes (_ n t)
    (when (buffer-insert-char
           (current-buffer)
           (current-linum)
           (current-charpos)
           c)
      (shift-position 1))))

(defun insert-lines (lines)
  (do ((rest lines (cdr rest)))
      ((null rest))
    (buffer-insert-line
     (current-buffer)
     (current-linum)
     (current-charpos)
     (car rest))
    (shift-position (length (car rest)))
    (when (cdr rest)
      (insert-newline 1))))

(defun insert-string (str)
  (insert-lines (split-string str #\newline)))

(defun insert-newline (&optional (n 1))
  (dotimes (_ n)
    (buffer-insert-newline (current-buffer)
                           (current-linum)
                           (current-charpos)))
  (forward-line n))

(defun delete-char (n &optional killp)
  (when (minusp n)
    (setf n (- n))
    (unless (shift-position (- n))
      (return-from delete-char nil)))
  (if (eobp)
      nil
      (let ((lines
              (buffer-delete-char (current-buffer)
                                  (current-linum)
                                  (current-charpos)
                                  n)))
        (when killp
          (kill-push (join (string #\newline) lines)))
        t)))

(defun set-charpos (pos)
  (setf (current-charpos) pos))

(defun beginning-of-buffer ()
  (point-set (point-min)))

(defun end-of-buffer ()
  (point-set (point-max)))

(defun beginning-of-line ()
  (set-charpos 0)
  t)

(defun end-of-line ()
  (set-charpos (buffer-line-length
                (current-buffer)
                (current-linum)))
  t)

(defun goto-position (position)
  (check-type position (integer 1 *))
  (beginning-of-buffer)
  (shift-position position))

(defun forward-line (&optional (n 1))
  (beginning-of-line)
  (if (plusp n)
      (dotimes (_ n t)
        (when (tail-line-p)
          (end-of-line)
          (return))
        (incf (current-linum)))
      (dotimes (_ (- n) t)
        (when (head-line-p)
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
    (cond ((head-line-p)
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
        (eof-p (tail-line-p)))
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

(defun put-attribute (start end attr)
  (buffer-put-attribute (current-buffer) start end attr))

(defun remove-attribute (start end attr)
  (buffer-remove-attribute (current-buffer) start end attr))

(defun current-column ()
  (str-width (current-line-string)
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
