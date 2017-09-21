(in-package :lem)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype event-modifier () '(member :left :right t nil))
  (deftype event-keysym () '(unsigned-byte 32)))

(defstruct (event (:constructor make-event))
  (ctrl nil :type event-modifier)
  (shift nil :type event-modifier)
  (meta nil :type event-modifier)
  (super nil :type event-modifier)
  (hypher nil :type event-modifier)
  (keysym 0 :type event-keysym))

(defun event-equal (ev1 ev2)
  (or (eq ev1 ev2)
      (eq t ev2)
      (eq ev2 t)))
