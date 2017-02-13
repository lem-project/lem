(in-package :lem-base)

(export '(buffers-start
          buffers-end
          first-line-p
          last-line-p
          start-line-p
          end-line-p
          start-buffer-p
          end-buffer-p
          same-line-p
          line-start
          line-end
          buffer-start
          buffer-end
          move-point
          line-offset
          character-offset
          character-at
          insert-character
          insert-string
          delete-character
          erase-buffer
          region-beginning
          region-end
          apply-region-lines
          map-region
          points-to-string
          count-characters
          delete-between-points
          count-lines
          line-number-at-point
          text-property-at
          put-text-property
          remove-text-property
          next-single-property-change
          previous-single-property-change
          line-string
          point-column
          move-to-column
          bolp
          eolp
          bobp
          eobp
          beginning-of-buffer
          end-of-buffer
          beginning-of-line
          end-of-line
          forward-line
          shift-position
          check-marked
          set-current-mark
          following-char
          preceding-char
          char-after
          char-before
          blank-line-p
          skip-chars-forward
          skip-chars-backward
          current-column
          position-at-point
          move-to-position
          move-to-line))

(defun buffers-start (buffer)
  (buffer-start-point buffer))

(defun buffers-end (buffer)
  (buffer-end-point buffer))

(defun first-line-p (point)
  (null (line-prev (point-line point))))

(defun last-line-p (point)
  (null (line-next (point-line point))))

(defun start-line-p (point)
  (zerop (point-charpos point)))

(defun end-line-p (point)
  (= (point-charpos point)
     (line-length (point-line point))))

(defun start-buffer-p (point)
  (and (first-line-p point)
       (start-line-p point)))

(defun end-buffer-p (point)
  (and (last-line-p point)
       (end-line-p point)))

(defun same-line-p (point1 point2)
  (assert (eq (point-buffer point1)
              (point-buffer point2)))
  (eq (point-line point1) (point-line point2)))

(defun line-string (point)
  (line-str (point-line point)))

(defun %move-to-position (point line charpos)
  (assert (line-alive-p line))
  (assert (eq (point-buffer point) (line-buffer line)))
  (assert (<= 0 charpos))
  (without-interrupts
    (point-change-line point line)
    (setf (point-charpos point) (min (line-length line) charpos)))
  point)

(defun move-point (point new-point)
  (%move-to-position point
                     (point-line new-point)
                     (point-charpos new-point)))

(defun line-start (point)
  (setf (point-charpos point) 0)
  point)

(defun line-end (point)
  (setf (point-charpos point)
        (line-length (point-line point)))
  point)

(defun buffer-start (point)
  (move-point point (buffers-start (point-buffer point))))

(defun buffer-end (point)
  (move-point point (buffers-end (point-buffer point))))

(defun line-offset (point n &optional (charpos 0))
  (cond
    ((plusp n)
     (do ((n n (1- n))
          (line (point-line point) (line-next line)))
         ((null line) nil)
       (when (zerop n)
         (%move-to-position point line charpos)
         (return point))))
    ((minusp n)
     (do ((n n (1+ n))
          (line (point-line point) (line-prev line)))
         ((null line) nil)
       (when (zerop n)
         (%move-to-position point line charpos)
         (return point))))
    (t
     (setf (point-charpos point)
           (if (< charpos 0)
               0
               (min charpos
                    (line-length (point-line point)))))
     point)))

(declaim (inline %character-offset))
(defun %character-offset (point n fn zero-fn)
  (cond ((zerop n) (when zero-fn (funcall zero-fn)))
        ((plusp n)
         (do ((line (point-line point) (line-next line))
              (charpos (point-charpos point) 0))
             ((null line) nil)
           (let ((w (- (line-length line) charpos)))
             (when (<= n w)
               (return (funcall fn line (+ charpos n))))
             (decf n (1+ w)))))
        (t
         (setf n (- n))
         (do* ((line (point-line point) (line-prev line))
               (charpos (point-charpos point) (and line (line-length line))))
             ((null line) nil)
           (when (<= n charpos)
             (return (funcall fn line (- charpos n))))
           (decf n (1+ charpos))))))

(defun character-offset (point n)
  (%character-offset point n
                     (lambda (line charpos)
                       (%move-to-position point line charpos)
                       point)
                     (lambda ()
                       point)))

(defun character-at (point &optional (offset 0))
  (%character-offset point offset
                     (lambda (line charpos)
                       (line-char line charpos))
                     (lambda ()
                       (line-char (point-line point)
                                  (point-charpos point)))))

(defun text-property-at (point key &optional (offset 0))
  (%character-offset point offset
                     (lambda (line charpos)
                       (line-search-property line key charpos))
                     (lambda ()
                       (line-search-property (point-line point)
                                             key
                                             (point-charpos point)))))

(defun put-text-property (start-point end-point key value)
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (%map-region start-point end-point
               (lambda (line start end)
                 (line-add-property line
                                    start
                                    (if (null end)
                                        (line-length line)
                                        end)
                                    key
                                    value
                                    (null end)))))

(defun remove-text-property (start-point end-point key)
  (assert (eq (point-buffer start-point)
              (point-buffer end-point)))
  (%map-region start-point end-point
               (lambda (line start end)
                 (line-remove-property line
                                       start
                                       (if (null end)
                                           (line-length line)
                                           end)
                                       key))))

;; 下の二つの関数next-single-property-change, previous-single-property-changeは
;; 効率がとても悪いので時が来たら書き直す

(defun next-single-property-change (point property-name &optional limit-point)
  (let ((first-value (text-property-at point property-name)))
    (with-point ((curr point))
      (loop
        (unless (character-offset curr 1)
          (return nil))
        (unless (eq first-value (text-property-at curr property-name))
          (return (move-point point curr)))
        (when (and limit-point (point<= limit-point curr))
          (return nil))))))

(defun previous-single-property-change (point property-name &optional limit-point)
  (let ((first-value (text-property-at point property-name -1)))
    (with-point ((curr point))
      (loop
        (unless (eq first-value (text-property-at curr property-name -1))
          (return (move-point point curr)))
        (unless (character-offset curr -1)
          (return nil))
        (when (and limit-point (point>= limit-point curr))
          (return nil))))))

(defun insert-character (point char &optional (n 1))
  (loop :repeat n :do (insert-char/point point char))
  t)

(defun insert-string (point string &rest plist)
  (if (null plist)
      (insert-string/point point string)
      (with-point ((start-point point))
        (insert-string/point point string)
        (let ((end-point (character-offset (copy-point start-point :temporary)
                                           (length string))))
          (loop :for (k v) :on plist :by #'cddr
                :do (put-text-property start-point end-point k v)))))
  t)

(defun delete-character (point &optional (n 1))
  (when (minusp n)
    (unless (character-offset point n)
      (return-from delete-character nil))
    (setf n (- n)))
  (unless (end-buffer-p point)
    (let ((string (delete-char/point point n)))
      string)))

(defun erase-buffer (&optional (buffer (current-buffer)))
  (buffer-start (buffer-point buffer))
  (delete-char/point (buffer-point buffer)
                     (count-characters (buffers-start buffer)
                                       (buffers-end buffer))))

(defun region-beginning (&optional (buffer (current-buffer)))
  (let ((start (buffer-point buffer))
        (end (buffer-mark buffer)))
    (if (point< start end)
        start
        end)))

(defun region-end (&optional (buffer (current-buffer)))
  (let ((start (buffer-point buffer))
        (end (buffer-mark buffer)))
    (if (point< start end)
        end
        start)))

;; この関数はfunctionにpointを渡したほうがいいかもしれない
(defun apply-region-lines (start end function)
  (with-point ((start start :right-inserting)
	       (end end :right-inserting))
    (move-point (current-point) start)
    (loop :while (point< (current-point) end) :do
       (with-point ((prev (line-start (current-point)) :left-inserting))
	 (funcall function)
	 (when (same-line-p (current-point) prev)
	   (unless (line-offset (current-point) 1)
	     (return)))))))

(defun %map-region (start end function)
  (when (point< end start)
    (rotatef start end))
  (let ((start-line (point-line start))
        (end-line (point-line end)))
    (loop :for line := start-line :then (line-next line)
          :for firstp := (eq line start-line)
          :for lastp := (eq line end-line)
          :do (funcall function
                       line
                       (if firstp
                           (point-charpos start)
                           0)
                       (if lastp
                           (point-charpos end)
                           nil))
          :until lastp))
  (values))

(defun map-region (start end function)
  (%map-region start end
               (lambda (line start end)
                 (funcall function
                          (subseq (line-str line) start end)
                          (not (null end))))))

(defun points-to-string (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (with-output-to-string (out)
    (map-region start end
                (lambda (string lastp)
                  (write-string string out)
                  (unless lastp
                    (write-char #\newline out))))))

(defun count-characters (start end)
  (let ((count 0))
    (map-region start
                end
                (lambda (string lastp)
                  (incf count (length string))
                  (unless lastp
                    (incf count))))
    count))

(defun delete-between-points (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (unless (point< start end)
    (rotatef start end))
  (delete-char/point start
		     (count-characters start end)))

(defun count-lines (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (when (point< end start)
    (rotatef start end))
  (do ((line (point-line start) (line-next line))
       (goal (point-line end))
       (count 0 (1+ count)))
      ((eq line goal)
       count)))

(defun line-number-at-point (point)
  (let* ((buffer (point-buffer point))
         (end-point (buffer-end-point buffer))
         (n (line-ord (point-line point)))
         (n2 (line-ord (point-line end-point))))
    (if (< n (- n2 n))
        (1+ (count-lines (buffer-start-point buffer) point))
        (- (buffer-nlines buffer) (count-lines point end-point)))))

(defun point-column (point)
  (string-width (line-string point)
                0
                (point-charpos point)))

(defun move-to-column (point column &optional force)
  (line-end point)
  (let ((cur-column (point-column point)))
    (cond ((< column cur-column)
           (setf (point-charpos point)
                 (wide-index (line-string point) column))
           point)
          (force
           (insert-character point #\space (- column cur-column))
           (line-end point))
          (t
           (line-end point)))))

(defun position-at-point (point)
  (let ((offset (point-charpos point)))
    (do ((line (line-prev (point-line point)) (line-prev line)))
        ((null line) offset)
      (incf offset (1+ (line-length line))))))

(defun move-to-position (point position)
  (character-offset (buffer-start point) position))

(defun move-to-line (point line-number)
  (let ((n (- (buffer-nlines (point-buffer point))
              line-number)))
  (if (< line-number n)
      (line-offset (buffer-start point) (1- line-number))
      (line-offset (buffer-end point) (- n)))))


(defun bolp ()
  (start-line-p (current-point)))

(defun eolp ()
  (end-line-p (current-point)))

(defun bobp ()
  (start-buffer-p (current-point)))

(defun eobp ()
  (end-buffer-p (current-point)))

(defun beginning-of-buffer ()
  (buffer-start (current-point)))

(defun end-of-buffer ()
  (buffer-end (current-point)))

(defun beginning-of-line ()
  (line-start (current-point))
  t)

(defun end-of-line ()
  (line-end (current-point))
  t)

(defun forward-line (&optional (n 1))
  (line-offset (current-point) n))

(defun shift-position (n)
  (character-offset (current-point) n))

(defun check-marked ()
  (unless (buffer-mark (current-buffer))
    (editor-error "Not mark in this buffer")))

(defun set-current-mark (point)
  (let ((buffer (point-buffer point)))
    (setf (buffer-mark-p buffer) t)
    (cond ((buffer-mark buffer)
           (move-point (buffer-mark buffer) point))
          (t
           (setf (buffer-mark buffer)
                 (copy-point point :right-inserting)))))
  point)

(defun following-char ()
  (character-at (current-point)))

(defun preceding-char ()
  (character-at (current-point) -1))

(defun char-after (&optional (point (current-point)))
  (character-at point 0))

(defun char-before (&optional (point (current-point)))
  (character-at point -1))

(defun blank-line-p (point)
  (let ((string (line-string point))
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
  (point-column (current-point)))

(defun invoke-save-excursion (function)
  (let ((point (copy-point (current-point) :right-inserting))
        (mark (when (buffer-mark-p (current-buffer))
                (copy-point (buffer-mark (current-buffer))
                            :right-inserting))))
    (unwind-protect (funcall function)
      (setf (current-buffer) (point-buffer point))
      (move-point (current-point) point)
      (delete-point point)
      (when mark
        (set-current-mark mark)
        (delete-point mark)))))
