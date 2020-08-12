(in-package :cl-setlocale)

(defun category->constant (category)
  (ecase category
    (:lc-all      +lc-all+)
    (:lc-collate  +lc-collate+)
    (:lc-ctype    +lc-ctype+)
    (:lc-messages +lc-messages+)
    (:lc-monetary +lc-monetary+)
    (:lc-numeric  +lc-numeric+)
    (:lc-time     +lc-time+)))

(defun setlocale (category locale)
  "Set the locale. CATEGORY can be one of :LC-ALL, :LC-COLLATE, :LC-CTYPE,
:LC-MESSAGES, :LC-MONETARY, :LC-NUMERIC or :LC-TIME. Locale is a string
describing the desired locale. If this function succeeds, T is returned and
set locale is returned as the second value. On failure NIL is returned"
  (declare (type string locale))
  (let ((pointer (foreign-funcall
                  "setlocale"
                  :int (category->constant category)
                  :string locale
                  :pointer)))
    (if (not (null-pointer-p pointer))
        (values t (foreign-string-to-lisp pointer)))))

(defun set-all-to-native ()
  "This function sets LC_ALL locale to one of the native environment."
  (setlocale :lc-all ""))
