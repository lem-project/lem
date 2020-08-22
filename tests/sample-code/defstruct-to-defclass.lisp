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
  (x 123 :type integer))

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
