(in-package :lem-base)

(defstruct (line (:constructor %make-line))
  prev
  next
  str
  plist
  syntax-context
  points)

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "string: ~S, plist: ~S"
            (line-str object)
            (line-plist object))))

(defun make-line (prev next str)
  (let ((line (%make-line :next next
                          :prev prev
                          :str str)))
    (when next
      (setf (line-prev next) line))
    (when prev
      (setf (line-next prev) line))
    line))

(defun make-empty-line ()
  (make-line nil nil ""))

(defun line-alive-p (line)
  (not (null (line-str line))))

(defun line-char (line i)
  (if (= i (line-length line))
      #\newline
      (char (line-str line) i)))

(defun line-length (line)
  (length (line-str line)))

(defun remove-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       nil)
      ((<= start start1 end end1)
       (iter:collect (list end end1 value1)))
      ((<= start1 start end1 end)
       (iter:collect (list start1 start value1)))
      ((<= start1 start end end1)
       (iter:collect (list start1 start value1))
       (iter:collect (list end end1 value1)))
      (t
       (iter:collect (list start1 end1 value1))))))

(defun normalization-elements (elements)
  (flet ((start (elt) (first elt))
         (end (elt) (second elt))
         (value (elt) (third elt)))
    (setf elements (sort elements #'< :key #'first))
    (iter:iter (iter:until (null elements))
      (cond
        ((and (eql (end (first elements))
                   (start (second elements)))
              (equal (value (first elements))
                     (value (second elements))))
         (iter:collect (list (start (first elements))
                             (end (second elements))
                             (value (first elements))))
         (setf elements (cddr elements)))
        (t
         (iter:collect (first elements))
         (setf elements (cdr elements)))))))

(defun subseq-elements (elements start end)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (cond
      ((<= start start1 end1 end)
       (iter:collect (list (- start1 start) (- end1 start) value1)))
      ((<= start start1 end end1)
       (iter:collect (list (- start1 start) (- end start) value1)))
      ((<= start1 start end1 end)
       (iter:collect (list (- start start) (- end1 start) value1)))
      ((<= start1 start end end1)
       (iter:collect (list (- start start) (- end start) value1))))))

(defun offset-elements (elements n)
  (iter:iter (iter:for (start1 end1 value1) iter:in elements)
    (iter:collect (list (+ n start1) (+ n end1) value1))))

(defun put-elements (elements start end value &optional contp)
  (normalization-elements
   (cons (list start end value contp)
         (remove-elements elements start end))))

(defun merge-plist (plist1 plist2)
  (let ((new-plist '()))
    (flet ((f (plist)
             (loop :for (k v) :on plist :by #'cddr
                   :do (setf (getf new-plist k)
                             (nconc (getf new-plist k) v)))))
      (f plist1)
      (f plist2))
    new-plist))

(defun line-merge (curr-line next-line pos)
  (setf (line-plist curr-line)
        (merge-plist
         (line-plist curr-line)
         (loop :for (key elements) :on (line-plist next-line) :by #'cddr
               :append (let ((new-elements
                               (loop :for (start end value) :in elements
                                     :collect (list (+ start pos)
                                                    (+ end pos)
                                                    value))))
                         (when new-elements
                           (list key new-elements)))))))

(defun line-normalization-plist (line)
  (loop :for (key elements) :on (line-plist line) :by #'cddr
        :collect (cons key (normalization-elements elements))))

(defun line-remove-property (line start end key)
  (setf (getf (line-plist line) key)
        (normalization-elements (remove-elements (getf (line-plist line) key) start end))))

(defun line-add-property (line start end key value contp)
  (assert (<= 0 start (line-length line)))
  (assert (<= 0 end (line-length line)))
  (assert (<= start end))
  (setf (getf (line-plist line) key)
        (put-elements (getf (line-plist line) key)
                      start end value contp)))

(defun line-clear-property (line key)
  (setf (getf (line-plist line) key) nil))

(defun line-search-property (line key pos)
  (loop :for (start end value contp) :in (getf (line-plist line) key)
        :do (when (if contp
                      (<= start pos end)
                      (<= start pos (1- end)))
              (return value))))

(defun line-search-property-range (line key pos-start pos-end)
  (when (null pos-end)
    (setq pos-end most-positive-fixnum))
  (loop :for (start end value contp) :in (getf (line-plist line) key)
        :do (when (or (and (<= pos-start start) (< start pos-end))
                      (if contp
                          (<= start pos-start end)
                          (<= start pos-start (1- end))))
              (return value))))

(defun line-property-insert-pos (line pos offset)
  (loop :for values :in (cdr (line-plist line)) :by #'cddr
        :do (loop :for v :in values
                  :for (start end) := v
                  :do (cond ((<= pos start)
                             (incf (first v) offset)
                             (incf (second v) offset))
                            ((< start pos end)
                             (incf (second v) offset))
                            ((< pos end)
                             (incf (second v) offset))))))

(defun line-property-insert-newline (line next-line pos)
  (let ((new-plist '()))
    (loop :for plist-rest :on (line-plist line) :by #'cddr
          :do (let ((new-values '())
                    (new-values-last nil))
                (setf (cadr plist-rest)
                      (iter:iter
                        (iter:for elt iter:in (cadr plist-rest))
                        (iter:for (start end value) iter:next elt)
                        (cond ((<= pos start)
                               (let ((new-elt (list (list (- start pos) (- end pos) value))))
                                 (cond
                                   (new-values-last
                                    (setf (cdr new-values-last) new-elt)
                                    (setf new-values-last (cdr new-values-last)))
                                   (t
                                    (setf new-values new-elt)
                                    (setf new-values-last new-elt)))))
                              ((<= pos end)
                               (iter:collect (list start pos value)))
                              (t
                               (iter:collect elt)))))
                (unless (null new-values)
                  (setf (getf new-plist (car plist-rest)) new-values))))
    (setf (line-plist next-line) new-plist)))

(defun line-property-delete-pos (line pos n)
  (loop :for plist-rest :on (line-plist line) :by #'cddr
        :do (setf (cadr plist-rest)
                  (loop :for elt :in (cadr plist-rest)
                        :for (start end value) := elt

                        :if (<= pos start end (+ pos n -1))
                        :do (progn)

                        :else :if (<= pos (+ pos n) start)
                        :collect (list (- start n) (- end n) value)

                        :else :if (< pos start (+ pos n))
                        :collect (list pos (- end n) value)

                        :else :if (<= start pos (+ pos n) end)
                        :collect (list start (- end n) value)

                        :else :if (<= start pos end (+ pos n))
                        :collect (list start pos value)

                        :else
                        :collect elt))))

(defun line-property-delete-line (line pos)
  (loop :for plist-rest :on (line-plist line) :by #'cddr
        :do (setf (cadr plist-rest)
                  (loop :for elt :in (cadr plist-rest)
                        :for (start end value) := elt
                        :if (<= pos start)
                        :do (progn)
                        :else :if (<= pos end)
                        :collect (list start pos value)
                        :else
                        :collect elt
                        ))))

(defun line-string/attributes (line)
  (cons (line-str line)
        (alexandria:if-let (sticky-attribute (getf (line-plist line) :sticky-attribute))
          (loop :with attributes := (getf (line-plist line) :attribute)
                :for (start end value contp) :in sticky-attribute
                :do (setf attributes (put-elements attributes start end value contp))
                :finally (return attributes))
          (getf (line-plist line) :attribute))))

(defun line-free (line)
  (when (line-prev line)
    (setf (line-next (line-prev line))
          (line-next line)))
  (when (line-next line)
    (setf (line-prev (line-next line))
          (line-prev line)))
  (setf (line-prev line) nil
        (line-next line) nil
        (line-str line) nil
        (line-points line) nil))
