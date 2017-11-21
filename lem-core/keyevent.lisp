(in-package :lem)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype key-event-modifier () '(member :left :right t nil))
  (deftype key-event-keysym () '(unsigned-byte 32)))

(defstruct key-event
  (ctrl nil :type key-event-modifier)
  (shift nil :type key-event-modifier)
  (meta nil :type key-event-modifier)
  (super nil :type key-event-modifier)
  (hypher nil :type key-event-modifier)
  (keysym 0 :type key-event-keysym))
