;;; input

(defstruct foo
  slot-a
  slot-b
  slot-c)

(defstruct foo
  ;; comment1
  a
  ;; comment2
  b
  c ;comment3
  d)

(defstruct foo
  (a 12)
  (b nil)
  (c (let ((x 0))
       (f x)))
  (d 100 :type integer)
  (e nil :type (or nil
                   string))
  (f (progn
       (foo))
     :type symbol)
  (g nil :read-only t)
  (h nil :read-only nil)
  (i nil :read-only (complex expression))
  (j 1
     :type integer
     :read-only t)
  (k 2
     :read-only t
     :type integer))

;;; output

(defclass foo ()
  ((slot-a
    :initarg :slot-a
    :initform nil
    :accessor foo-slot-a)
   (slot-b
    :initarg :slot-b
    :initform nil
    :accessor foo-slot-b)
   (slot-c
    :initarg :slot-c
    :initform nil
    :accessor foo-slot-c)))

(defclass foo ()
  (;; comment1
   (a
    :initarg :a
    :initform nil
    :accessor foo-a)
   ;; comment2
   (b
    :initarg :b
    :initform nil
    :accessor foo-b)
   (c ;comment3
    :initarg :c
    :initform nil
    :accessor foo-c)
   (d
    :initarg :d
    :initform nil
    :accessor foo-d)))

(defclass foo ()
  ((x
    :initarg :x
    :initform 123
    :accessor foo-x
    :type integer)))
