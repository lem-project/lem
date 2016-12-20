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
          current-column
          move-to-column
          point-to-offset
          ))

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
  (move-point marker (buffers-start (marker-buffer marker))))

(defun buffer-end (marker)
  (move-point marker (buffers-end (marker-buffer marker))))

(defun move-point (marker new-marker)
  (setf (marker-point marker)
        (marker-point new-marker))
  marker)

(defun same-line-p (marker1 marker2)
  (assert (eq (marker-buffer marker1)
              (marker-buffer marker2)))
  (= (marker-linum marker1)
     (marker-linum marker2)))

(defun line-offset (marker n &optional (charpos 0))
  (if (plusp n)
      (dotimes (_ n)
        (when (last-line-p marker)
          (return-from line-offset (values (line-end marker) nil)))
        (incf (marker-linum marker)))
      (dotimes (_ (- n))
        (when (first-line-p marker)
          (return-from line-offset (values (line-start marker) nil)))
        (decf (marker-linum marker))))
  (setf (marker-charpos marker)
        (if (< 0 charpos)
            (min charpos
                 (length (line-string-at marker)))
            0))
  (values marker t))

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
  (cond ((lem.text-property:text-property-p string)
         (let ((str (lem.text-property:text-property-string string)))
           (with-marker ((start-marker marker))
             (insert-string/marker marker str)
             (let ((end-marker (character-offset (copy-marker start-marker :temporary)
                                                 (length str))))
               (loop :for (k v) :on (lem.text-property:text-property-plist string) :by #'cddr
                     :do (put-text-property start-marker end-marker k v))))))
        (t
         (insert-string/marker marker string)))
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
          (text-property-at temp-marker key 0)))))

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

(defun next-single-property-change (marker property-name &optional limit-marker)
  (let ((first-value (text-property-at marker property-name))
        (start-marker (copy-marker marker :temporary))
        moved)
    (loop
      (multiple-value-setq (marker moved) (character-offset marker 1))
      (unless moved
        (move-point marker start-marker)
        (return nil))
      (unless (eq first-value (text-property-at marker property-name))
        (return marker))
      (when (and limit-marker (marker<= limit-marker marker))
        (move-point marker start-marker)
        (return nil)))))

(defun previous-single-property-change (marker property-name &optional limit-marker)
  (let ((first-value (text-property-at marker property-name -1))
        (start-marker (copy-marker marker :temporary))
        moved)
    (loop
      (unless (eq first-value (text-property-at marker property-name -1))
        (return marker))
      (multiple-value-setq (marker moved) (character-offset marker -1))
      (unless moved
        (move-point marker start-marker)
        (return nil))
      (when (and limit-marker (marker>= limit-marker marker))
        (move-point marker start-marker)
        (return nil)))))

(defun line-string-at (marker)
  (buffer-line-string (marker-buffer marker)
                      (marker-linum marker)))

(defun point-column (marker)
  (string-width (line-string-at marker)
                0
                (marker-charpos marker)))

(defun move-to-column (marker column &optional force)
  (line-end marker)
  (let ((cur-column (point-column marker)))
    (cond ((< column cur-column)
           (setf (marker-charpos marker)
                 (wide-index (line-string-at marker) column))
           marker)
          (force
           (insert-char-at marker #\space (- column cur-column))
           (line-end marker))
          (t
           (line-end marker)))))

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

(defun goto-position (position)
  (check-type position (integer 1 *))
  (beginning-of-buffer)
  (shift-position position))

(defun forward-line (&optional (n 1))
  (nth-value 1 (line-offset (current-marker) n)))

(defun shift-position (n)
  (nth-value 1 (character-offset (current-marker) n)))

(defun check-marked ()
  (unless (buffer-mark-p (current-buffer))
    (editor-error "Not mark in this buffer")))

(defun set-current-mark (marker)
  (let ((buffer (marker-buffer marker)))
    (cond ((buffer-mark-p buffer)
           (move-point (buffer-mark-marker buffer)
                       marker))
          (t
           (setf (buffer-mark-p buffer) t)
           (setf (buffer-mark-marker buffer)
                 (copy-marker marker :right-inserting)))))
  marker)

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
  (let ((n (skip-chars-forward (current-marker)
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

(defun skip-chars-internal (point test not-p dir)
  (loop :for count :from 0
        :for c := (character-at point (if dir 0 -1))
        :do
        (unless (if (if (consp test)
                        (member c test)
                        (funcall test c))
                    (not not-p)
                    not-p)
          (return count))
        (character-offset point (if dir 1 -1))))

(defun skip-chars-forward (point test &optional not-p)
  (skip-chars-internal point test not-p t))

(defun skip-chars-backward (point test &optional not-p)
  (skip-chars-internal point test not-p nil))

(defun current-column ()
  (point-column (current-marker)))

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
