(defpackage :lem-tests/input
  (:use :cl :rove :lem))
(in-package :lem-tests/input)

(deftest escape-key-p-test
  (ok (lem-core::escape-key-p (make-key :sym "Escape")))
  (ng (lem-core::escape-key-p (make-key :sym "x")))
  (ng (lem-core::escape-key-p (make-key :meta t :sym "Escape")))
  (ng (lem-core::escape-key-p (make-key :ctrl t :sym "Escape"))))

(deftest add-meta-modifier-test
  (let ((key (lem-core::add-meta-modifier (make-key :sym "x"))))
    (ok (key-meta key))
    (ok (equal "x" (key-sym key))))
  (let ((key (lem-core::add-meta-modifier (make-key :ctrl t :sym "c"))))
    (ok (key-meta key))
    (ok (key-ctrl key))
    (ok (equal "c" (key-sym key)))))

(deftest escape-as-meta-disabled-test
  (lem/common/var:with-global-variable-value (escape-as-meta-prefix nil)
    (let ((escape-key (make-key :sym "Escape"))
          (x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-escape-to-meta
                     escape-key
                     (lambda () x-key))))
        (ok (match-key result :sym "Escape"))))))

(deftest escape-as-meta-enabled-test
  (lem/common/var:with-global-variable-value (escape-as-meta-prefix t)
    (let ((escape-key (make-key :sym "Escape"))
          (x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-escape-to-meta
                     escape-key
                     (lambda () x-key))))
        (ok (key-meta result))
        (ok (equal "x" (key-sym result)))))))

(deftest escape-escape-produces-escape-test
  (lem/common/var:with-global-variable-value (escape-as-meta-prefix t)
    (let ((escape-key (make-key :sym "Escape")))
      (let ((result (lem-core::maybe-convert-escape-to-meta
                     escape-key
                     (lambda () escape-key))))
        (ok (match-key result :sym "Escape"))
        (ng (key-meta result))))))

(deftest escape-with-modifier-preserves-modifiers-test
  (lem/common/var:with-global-variable-value (escape-as-meta-prefix t)
    (let ((escape-key (make-key :sym "Escape"))
          (ctrl-c-key (make-key :ctrl t :sym "c")))
      (let ((result (lem-core::maybe-convert-escape-to-meta
                     escape-key
                     (lambda () ctrl-c-key))))
        (ok (key-meta result))
        (ok (key-ctrl result))
        (ok (equal "c" (key-sym result)))))))

(deftest non-escape-key-passes-through-test
  (lem/common/var:with-global-variable-value (escape-as-meta-prefix t)
    (let ((x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-escape-to-meta
                     x-key
                     (lambda () (error "Should not be called")))))
        (ok (match-key result :sym "x"))))))
