(defpackage :fatstring
  (:use :cl)
  (:export
   :fatstring
   :make-fatstring
   :fatstring-p
   :copy-fatstring
   :change-font
   :fat-string
   :fat-font-data
   :fat-char
   :fat-upcase
   :fat-downcase
   :fat-capitalize
   :fat-nupcase
   :fat-ndonwcase
   :fat-ncapitalize
   :fat-trim
   :fat-left-trim
   :fat-right-trim
   :fat-equalp
   :fat=
   :fat/=
   :fat<
   :fat>
   :fat<=
   :fat>=
   :fat-fill
   :fat-substring
   :fat-reverse
   :fat-nreverse
   :fat-length
   :fat-find
   :fat-find-if
   :fat-position
   :fat-position-if
   :fat-search
   :fat-mismatch
   :fat-replace
   :fat-concat))

(in-package :fatstring)

(defvar *fat-mark-symbol* (gensym "FATSTRING"))

(defun make-fatstring-internal (str font-data)
  (vector *fat-mark-symbol* str font-data))

(defun fatstring-p (fatstring)
  (and (vectorp fatstring)
       (= 3 (length fatstring))
       (eq *fat-mark-symbol* (aref fatstring 0))))

(defun fat-string (fatstring)
  (assert (fatstring-p fatstring))
  (aref fatstring 1))

(defun (setf fat-string) (new-string fatstring)
  (setf (aref fatstring 1) new-string))

(defun fat-font-data (fatstring)
  (assert (fatstring-p fatstring))
  (aref fatstring 2))

(defun (setf fat-font-data) (new-font-data fatstring)
  (setf (aref fatstring 2) new-font-data))

(defun copy-fatstring (fatstring)
  (make-fatstring-internal (fat-string fatstring)
                           (copy-seq (fat-font-data fatstring))))

(defun make-fatstring (str font &optional (start 0) end)
  (when (fatstring-p str)
    (setq str (fat-string str)))
  (let ((font-data (make-array (length str))))
    (loop :for i :from start :below (or end (length str)) :do
      (setf (aref font-data i) font))
    (make-fatstring-internal str font-data)))

(deftype fatstring ()
  `(satisfies fatstring-p))

(defun change-font (fatstring font op &optional (start 0) end)
  (loop
    :with f := (ecase op
                 (:and #'logand)
                 (:or #'logior)
                 (:xor #'logxor)
                 (:to (constantly font)))
    :for i :from start
    :below (or end (length (fat-string fatstring)))
    :do (setf (aref (fat-font-data fatstring) i)
              (funcall f font (aref (fat-font-data fatstring) i))))
  fatstring)

(defun fat-char (fatstring index)
  (values (schar (fat-string fatstring) index)
          (aref (fat-font-data fatstring) index)))

(defun fat-upcase (fatstring &key (start 0) end)
  (make-fatstring-internal 
   (string-upcase (fat-string fatstring) :start start :end end)
   (fat-font-data fatstring)))

(defun fat-downcase (fatstring &key (start 0) end)
  (make-fatstring-internal
   (string-downcase (fat-string fatstring) :start start :end end)
   (fat-font-data fatstring)))

(defun fat-capitalize (fatstring &key (start 0) end)
  (make-fatstring-internal
   (string-capitalize (fat-string fatstring) :start start :end end)
   (fat-font-data fatstring)))

(defun fat-nupcase (fatstring &key (start 0) end)
  (setf (fat-string fatstring)
        (nstring-upcase (fat-string fatstring) :start start :end end))
  fatstring)

(defun fat-ndowncase (fatstring &key (start 0) end)
  (setf (fat-string fatstring)
        (nstring-downcase (fat-string fatstring) :start start :end end))
  fatstring)

(defun fat-ncapitalize (fatstring &key (start 0) end)
  (setf (fat-string fatstring)
        (nstring-capitalize (fat-string fatstring) :start start :end end))
  fatstring)

(defmacro %trim-pos-form (char-bag string from-end)
  `(position-if #'(lambda (c)
                    (not (find c ,char-bag)))
                ,string
                ,@(when from-end
                    `(:from-end ,from-end))))

(defun %left-trim-pos (char-bag string)
  (or (%trim-pos-form char-bag string nil)
      0))

(defun %right-trim-pos (char-bag string)
  (or (1+ (%trim-pos-form char-bag string t))
      (length string)))

(declaim (ftype function fat-substring))

(defun fat-trim (char-bag fatstring)
  (let ((left-pos (%left-trim-pos char-bag (fat-string fatstring)))
        (right-pos (%right-trim-pos char-bag (fat-string fatstring))))
    (cond ((and left-pos right-pos)
           (fat-substring fatstring left-pos right-pos))
          (left-pos
           (fat-substring fatstring left-pos))
          (right-pos
           (fat-substring fatstring 0 right-pos)))))

(defun fat-left-trim (char-bag fatstring)
  (fat-substring fatstring (%left-trim-pos char-bag (fat-string fatstring))))

(defun fat-right-trim (char-bag fatstring)
  (fat-substring fatstring 0
                 (%right-trim-pos char-bag (fat-string fatstring))))

(defun fat-equalp (fatstring1 fatstring2)
  (and (string= (fat-string fatstring1)
                (fat-string fatstring2))
       (equalp (fat-font-data fatstring1)
               (fat-font-data fatstring2))))

(macrolet ((def (name cmp)
                `(defun ,name (fatstring1
                               fatstring2
                               &key (start1 0) end1 (start2 0) end2)
                   (,cmp (fat-string fatstring1)
                         (fat-string fatstring2)
                         :start1 start1
                         :end1 end1
                         :start2 start2
                         :end2 end2))))
  (def fat= string=)
  (def fat/= string/=)
  (def fat< string<)
  (def fat> string>)
  (def fat<= string<=)
  (def fat>= string>=))

(defun fat-fill (fatstring char &key (start 0) end)
  (setf (fat-string fatstring)
        (fill (fat-string fatstring)
              char
              :start start :end end))
  fatstring)

(defun %fat-wrap (fn fatstring)
  (make-fatstring-internal (funcall fn (fat-string fatstring))
                           (funcall fn (fat-font-data fatstring))))

(defun fat-substring (fatstring start &optional end)
  (%fat-wrap #'(lambda (seq)
                 (subseq seq start end))
             fatstring))

(defun fat-reverse (fatstring)
  (%fat-wrap #'reverse fatstring))

(defun fat-nreverse (fatstring)
  (setf (fat-string fatstring)
        (nreverse (fat-string fatstring)))
  (setf (fat-font-data fatstring)
        (nreverse (fat-font-data fatstring)))
  fatstring)

(defun fat-length (string)
  (if (fatstring-p string)
      (length (fat-string string))
      (length string)))

(defun fat-find (char fatstring &key from-end (start 0) end key test test-not)
  (let ((pos (position char (fat-string fatstring)
                       :from-end from-end
                       :start start
                       :end end
                       :key key
                       :test test
                       :test-not test-not)))
    (fat-char fatstring pos)))

(defun fat-find-if (predicate fatstring &key from-end (start 0) end key)
  (let ((pos (position-if predicate (fat-string fatstring)
                          :from-end from-end
                          :start start
                          :end end
                          :key key)))
    (fat-char fatstring pos)))

(defun fat-position (char fatstring &key from-end (start 0) end key test test-not)
  (position char (fat-string fatstring)
            :from-end from-end
            :start start
            :end end
            :key key
            :test test
            :test-not test-not))

(defun fat-position-if (predicate fatstring &key from-end (start 0) end key)
  (position-if predicate (fat-string fatstring)
               :from-end from-end
               :start start
               :end end
               :key key))

(defun to-string (string)
  (if (fatstring-p string)
      (fat-string string)
      string))

(defun fat-search (string1
                   string2
                   &key from-end
                   (test #'eql) (test-not nil) (start1 0) (end1 nil)
                   (start2 0) (end2 nil) (key nil))
  (search (to-string string1)
          (to-string string2)
          :from-end from-end
          :test test
          :test-not test-not
          :start1 start1
          :end1 end1
          :start2 start2
          :end2 end2
          :key key))

(defun fat-mismatch (string1
                     string2
                     &key from-end
                     (test #'eql) (test-not nil) (start1 0) (end1 nil)
                     (start2 0) (end2 nil) (key nil))
  (mismatch (to-string string1)
            (to-string string2)
            :from-end from-end
            :test test
            :test-not test-not
            :start1 start1
            :end1 end1
            :start2 start2
            :end2 end2
            :key key))

(defun fat-replace (fatstring
                    string
                    &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (replace (fat-string fatstring)
           (to-string string)
           :start1 start1
           :end1 end1
           :start2 start2
           :end2 end2)
  fatstring)

(defun to-fatstring (string)
  (if (fatstring-p string)
      string
      (make-fatstring-internal
       string
       (make-array (length string)))))

(defun %fat-concat (string1 string2)
  (let ((fatstring1 (to-fatstring string1))
        (fatstring2 (to-fatstring string2)))
    (make-fatstring-internal
     (concatenate 'string
                  (fat-string fatstring1)
                  (fat-string fatstring2))
     (concatenate 'vector
                  (fat-font-data fatstring1)
                  (fat-font-data fatstring2)))))

(defun fat-concat (&rest fatstrings)
  (reduce #'%fat-concat fatstrings))
