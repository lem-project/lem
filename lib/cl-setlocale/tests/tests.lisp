(in-package :cl-setlocale-tests)

(def-suite setlocale :description "Test setlocale function")
(in-suite setlocale)

(test setlocale
  (loop for category in '(:lc-all      :lc-collate
                          :lc-ctype    :lc-messages
                          :lc-monetary :lc-numeric
                          :lc-time) do
        (loop for locale in '("C" "") do
              (is-true (setlocale category locale)))))

(defun run-tests ()
  "Run all tests and return T if all tests have passed"
  (let ((status (run 'setlocale)))
    (explain! status)
    (results-status status)))
