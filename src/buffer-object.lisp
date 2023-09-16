(in-package :lem-core)

(defun associate-object-with-region (start end object &key attribute)
  (put-text-property start end 'object object)
  (when attribute
    (put-text-property start end :attribute attribute)))

(defun object-at (point)
  (text-property-at point 'object))
