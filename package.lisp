(defpackage :lem-internal
  (:use :cl))

(defpackage :lem
  (:use :cl :lem.fatstring :lem.util :lem-internal))

(defpackage :lem-user
  (:use :cl :lem))
