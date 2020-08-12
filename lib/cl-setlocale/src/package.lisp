(defpackage cl-setlocale
  (:use #:cl #:cffi)
  (:export #:setlocale
           #:set-all-to-native

           #:+lc-all+
           #:+lc-collate+
           #:+lc-ctype+
           #:+lc-messages+
           #:+lc-monetary+
           #:+lc-numeric
           #:+lc-time+))
