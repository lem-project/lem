(defpackage :lem/directory-mode
  (:use :cl :lem)
  #+sbcl
  (:lock t)
  (:export
   :*default-sort-method*))
