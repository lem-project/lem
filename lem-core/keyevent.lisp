(in-package :lem)

(export '(make-key
          key-p
          key-ctrl
          key-meta
          key-super
          key-hypher
          key-sym
          key-to-char
          match-key))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype key-modifier () '(member :left :right t nil)))

(defstruct key
  (ctrl nil :type key-modifier)
  (meta nil :type key-modifier)
  (super nil :type key-modifier)
  (hypher nil :type key-modifier)
  (sym 0 :type string))

(defun key-to-char (key)
  (cond ((key-ctrl key)
         (and (= 1 (length (key-sym key)))
              (let ((code (char-code (char-downcase (char (key-sym key) 0)))))
                (and code (<= 64 code 95) (- code 64)))))
        ((= 1 (length (key-sym key)))
         (char (key-sym key) 0))))

(defun match-key (key &key ctrl meta super hypher sym)
  (and (eq (key-ctrl key) ctrl)
       (eq (key-meta key) meta)
       (eq (key-super key) super)
       (eq (key-hypher key) hypher)
       (equal (key-sym key) sym)))
