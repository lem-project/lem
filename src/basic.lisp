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
          erase-buffer

          region-beginning
          region-end
          apply-region-lines))

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
  (let ((linum (marker-linum marker)))
    (if (plusp n)
        (dotimes (_ n)
          (when (<= (buffer-nlines (marker-buffer marker)) linum)
            (return-from line-offset nil))
          (incf linum))
        (dotimes (_ (- n))
          (when (= linum 1)
            (return-from line-offset nil))
          (decf linum)))
    (setf (marker-linum marker) linum))
  (setf (marker-charpos marker)
        (if (< 0 charpos)
            (min charpos
                 (length (line-string-at marker)))
            0))
  marker)

(defun %character-offset-positive (marker n)
  (let ((charpos (marker-charpos marker))
        (linum (marker-linum marker)))
    (loop
      (when (minusp n)
        (setf (marker-charpos marker) charpos)
        (setf (marker-linum marker) linum)
        (return nil))
      (let* ((length (1+ (buffer-line-length (marker-buffer marker)
                                             (marker-linum marker))))
             (w (- length (marker-charpos marker))))
        (when (< n w)
          (incf (marker-charpos marker) n)
          (return marker))
        (decf n w)
        (unless (line-offset marker 1)
          (setf (marker-charpos marker) charpos)
          (setf (marker-linum marker) linum)
          (return nil))))))

(defun %character-offset-negative (marker n)
  (let ((charpos (marker-charpos marker))
        (linum (marker-linum marker)))
    (loop
      (when (minusp n)
        (setf (marker-charpos marker) charpos)
        (setf (marker-linum marker) linum)
        (return nil))
      (when (<= n (marker-charpos marker))
        (decf (marker-charpos marker) n)
        (return marker))
      (decf n (1+ (marker-charpos marker)))
      (cond ((first-line-p marker)
             (setf (marker-charpos marker) charpos)
             (setf (marker-linum marker) linum)
             (return nil))
            (t
             (line-offset marker -1)
             (line-end marker))))))

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
        (when (character-offset temp-marker offset)
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
    (unless (character-offset marker n)
      (return-from delete-char-at nil))
    (setf n (- n)))
  (unless (end-buffer-p marker)
    (let ((string (delete-char/marker marker n)))
      (when killp
        (kill-push string))
      t)))

(defun erase-buffer (&optional (buffer (current-buffer)))
  (buffer-start (buffer-point-marker buffer))
  (buffer-mark-cancel buffer)
  (delete-char/marker (buffer-point-marker buffer) t))

(defun text-property-at (marker key &optional (offset 0))
  (if (zerop offset)
      (line-search-property (get-line/marker marker) key (marker-charpos marker))
      (with-marker ((temp-marker marker))
        (when (character-offset temp-marker offset)
          (text-property-at temp-marker key 0)))))

(defun put-text-property (start-marker end-marker key value)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (%map-region start-marker end-marker
               (lambda (line start end)
                 (line-add-property line
                                    start
                                    (if (null end)
                                        (line-length line)
                                        end)
                                    key
                                    value
                                    (null end)))))

(defun remove-text-property (start-marker end-marker key)
  (assert (eq (marker-buffer start-marker)
              (marker-buffer end-marker)))
  (%map-region start-marker end-marker
               (lambda (line start end)
                 (line-remove-property line
                                       start
                                       (if (null end)
                                           (line-length line)
                                           end)
                                       key))))

(defun next-single-property-change (marker property-name &optional limit-marker)
  (let ((first-value (text-property-at marker property-name))
        (start-marker (copy-marker marker :temporary)))
    (loop
      (unless (character-offset marker 1)
        (move-point marker start-marker)
        (return nil))
      (unless (eq first-value (text-property-at marker property-name))
        (return marker))
      (when (and limit-marker (marker<= limit-marker marker))
        (move-point marker start-marker)
        (return nil)))))

(defun previous-single-property-change (marker property-name &optional limit-marker)
  (let ((first-value (text-property-at marker property-name -1))
        (start-marker (copy-marker marker :temporary)))
    (loop
      (unless (eq first-value (text-property-at marker property-name -1))
        (return marker))
      (unless (character-offset marker -1)
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
  t)

(defun insert-newline (&optional (n 1))
  (insert-char-at (current-marker) #\newline n)
  t)

(defun insert-string (string)
  (insert-string-at (current-marker) string))

(defun delete-char (n &optional killp)
  (delete-char-at (current-marker) n killp))

(defun beginning-of-buffer ()
  (buffer-start (current-marker)))

(defun end-of-buffer ()
  (buffer-end (current-marker)))

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
  (line-offset (current-marker) n))

(defun shift-position (n)
  (character-offset (current-marker) n))

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
  (line-string-at (current-marker)))

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

(defun blank-line-p (point)
  (let ((string (line-string-at point))
        (eof-p (last-line-p point))
        (count 0))
    (loop :for c :across string :do
          (unless (or (char= c #\space)
                      (char= c #\tab))
            (return-from blank-line-p nil))
          (incf count))
    (if eof-p
        count
        (1+ count))))

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
        (unless (character-offset point (if dir 1 -1))
          (return count))))

(defun skip-chars-forward (point test &optional not-p)
  (skip-chars-internal point test not-p t))

(defun skip-chars-backward (point test &optional not-p)
  (skip-chars-internal point test not-p nil))

(defun current-column ()
  (point-column (current-marker)))

(defun point-to-offset (point)
  (let ((end-linum (marker-linum point))
        (end-charpos (marker-charpos point))
        (buffer (marker-buffer point))
        (offset 0))
    (loop :repeat (1- end-linum)
          :for linum :from 1
          :do (incf offset
                    (1+ (buffer-line-length buffer linum))))
    (+ offset end-charpos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun region-beginning (&optional (buffer (current-buffer)))
  (let ((start (buffer-point-marker buffer))
        (end (buffer-mark-marker buffer)))
    (if (marker< start end)
        start
        end)))

(defun region-end (&optional (buffer (current-buffer)))
  (let ((start (buffer-point-marker buffer))
        (end (buffer-mark-marker buffer)))
    (if (marker< start end)
        end
        start)))

(defun apply-region-lines (start end function)
  (with-marker ((start start :right-inserting)
                (end end :right-inserting))
    (move-point (current-marker) start)
    (loop :while (marker< (current-marker) end) :do
          (with-marker ((prev (line-start (current-marker))))
            (funcall function)
            (when (same-line-p (current-marker) prev)
              (unless (line-offset (current-marker) 1)
                (return)))))))
