(defpackage :lem-tests/input
  (:use :cl :rove :lem))
(in-package :lem-tests/input)

(deftest keys-equal-p-test
  (ok (lem-core::keys-equal-p (make-key :sym "x") (make-key :sym "x")))
  (ok (lem-core::keys-equal-p (make-key :ctrl t :sym "c") (make-key :ctrl t :sym "c")))
  (ng (lem-core::keys-equal-p (make-key :sym "x") (make-key :sym "y")))
  (ng (lem-core::keys-equal-p (make-key :ctrl t :sym "c") (make-key :sym "c"))))

(deftest meta-prefix-key-p-with-t-test
  (ok (lem-core::meta-prefix-key-p (make-key :sym "Escape") t))
  (ng (lem-core::meta-prefix-key-p (make-key :sym "x") t))
  (ng (lem-core::meta-prefix-key-p (make-key :meta t :sym "Escape") t)))

(deftest meta-prefix-key-p-with-single-key-test
  ;; Note: C-[ is converted to Escape by Lem's key conversion, so we use C-; instead
  (let ((prefix-key (make-key :ctrl t :sym ";")))
    (ok (lem-core::meta-prefix-key-p (make-key :ctrl t :sym ";") prefix-key))
    (ng (lem-core::meta-prefix-key-p (make-key :sym "Escape") prefix-key))
    (ng (lem-core::meta-prefix-key-p (make-key :sym "x") prefix-key))))

(deftest meta-prefix-key-p-with-key-list-test
  (let ((prefix-keys (list (make-key :sym "Escape")
                           (make-key :ctrl t :sym ";"))))
    (ok (lem-core::meta-prefix-key-p (make-key :sym "Escape") prefix-keys))
    (ok (lem-core::meta-prefix-key-p (make-key :ctrl t :sym ";") prefix-keys))
    (ng (lem-core::meta-prefix-key-p (make-key :sym "x") prefix-keys))))

(deftest add-meta-modifier-test
  (let ((key (lem-core::add-meta-modifier (make-key :sym "x"))))
    (ok (key-meta key))
    (ok (equal "x" (key-sym key))))
  (let ((key (lem-core::add-meta-modifier (make-key :ctrl t :sym "c"))))
    (ok (key-meta key))
    (ok (key-ctrl key))
    (ok (equal "c" (key-sym key)))))

(deftest meta-prefix-disabled-test
  (lem/common/var:with-global-variable-value (meta-prefix-keys nil)
    (let ((escape-key (make-key :sym "Escape"))
          (x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-to-meta
                     escape-key
                     (lambda () x-key))))
        (ok (match-key result :sym "Escape"))))))

(deftest meta-prefix-with-t-test
  (lem/common/var:with-global-variable-value (meta-prefix-keys t)
    (let ((escape-key (make-key :sym "Escape"))
          (x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-to-meta
                     escape-key
                     (lambda () x-key))))
        (ok (key-meta result))
        (ok (equal "x" (key-sym result)))))))

(deftest meta-prefix-double-press-test
  (lem/common/var:with-global-variable-value (meta-prefix-keys t)
    (let ((escape-key (make-key :sym "Escape")))
      (let ((result (lem-core::maybe-convert-to-meta
                     escape-key
                     (lambda () escape-key))))
        (ok (match-key result :sym "Escape"))
        (ng (key-meta result))))))

(deftest meta-prefix-preserves-modifiers-test
  (lem/common/var:with-global-variable-value (meta-prefix-keys t)
    (let ((escape-key (make-key :sym "Escape"))
          (ctrl-c-key (make-key :ctrl t :sym "c")))
      (let ((result (lem-core::maybe-convert-to-meta
                     escape-key
                     (lambda () ctrl-c-key))))
        (ok (key-meta result))
        (ok (key-ctrl result))
        (ok (equal "c" (key-sym result)))))))

(deftest non-prefix-key-passes-through-test
  (lem/common/var:with-global-variable-value (meta-prefix-keys t)
    (let ((x-key (make-key :sym "x")))
      (let ((result (lem-core::maybe-convert-to-meta
                     x-key
                     (lambda () (error "Should not be called")))))
        (ok (match-key result :sym "x"))))))

(deftest custom-prefix-key-test
  ;; Note: C-[ is converted to Escape by Lem's key conversion, so we use C-; instead
  (let ((custom-key (make-key :ctrl t :sym ";")))
    (lem/common/var:with-global-variable-value (meta-prefix-keys custom-key)
      (let ((x-key (make-key :sym "x")))
        ;; C-; + x -> M-x
        (let ((result (lem-core::maybe-convert-to-meta
                       custom-key
                       (lambda () x-key))))
          (ok (key-meta result))
          (ok (equal "x" (key-sym result))))
        ;; Escape should not trigger with custom key
        (let ((escape-key (make-key :sym "Escape")))
          (let ((result (lem-core::maybe-convert-to-meta
                         escape-key
                         (lambda () (error "Should not be called")))))
            (ok (match-key result :sym "Escape"))))))))

(deftest multiple-prefix-keys-test
  ;; Note: C-[ is converted to Escape by Lem's key conversion, so we use C-; instead
  (let ((prefix-keys (list (make-key :sym "Escape")
                           (make-key :ctrl t :sym ";"))))
    (lem/common/var:with-global-variable-value (meta-prefix-keys prefix-keys)
      (let ((x-key (make-key :sym "x")))
        ;; Escape + x -> M-x
        (let ((result (lem-core::maybe-convert-to-meta
                       (make-key :sym "Escape")
                       (lambda () x-key))))
          (ok (key-meta result))
          (ok (equal "x" (key-sym result))))
        ;; C-; + x -> M-x
        (let ((result (lem-core::maybe-convert-to-meta
                       (make-key :ctrl t :sym ";")
                       (lambda () x-key))))
          (ok (key-meta result))
          (ok (equal "x" (key-sym result))))))))
