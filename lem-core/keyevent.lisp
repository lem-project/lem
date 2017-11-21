(in-package :lem)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype key-modifier () '(member :left :right t nil)))

(defstruct key
  (ctrl nil :type key-modifier)
  (meta nil :type key-modifier)
  (super nil :type key-modifier)
  (hypher nil :type key-modifier)
  (sym 0 :type character))
