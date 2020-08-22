;;; input

(defstruct foo
  slot-a
  slot-b
  slot-c)

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
