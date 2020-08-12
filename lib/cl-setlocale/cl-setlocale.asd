(defsystem :cl-setlocale
  :name :cl-setlocale
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "FFI to setlocale and ncurses locale helper"
  :license "2-clause BSD"
  :depends-on (:cffi)
  :defsystem-depends-on (:cffi-grovel)
  :serial t
  :components ((:file "src/package")
               #-win32
               (:cffi-grovel-file "src/constants")
               #+win32
               (:file "src/constants-win32")
               (:file "src/setlocale"))
  :in-order-to ((test-op (load-op "cl-setlocale/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-setlocale-tests :run-tests)))

(defsystem :cl-setlocale/tests
  :name :cl-setlocale/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :serial t
  :components ((:file "tests/package")
               (:file "tests/tests"))
  :depends-on (:cl-setlocale :fiveam))
