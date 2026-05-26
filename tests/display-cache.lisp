(defpackage :lem-tests/display-cache
  (:use :cl :rove :lem-core))
(in-package :lem-tests/display-cache)


(deftest test-mix-hashes
  ;; 1. The Commutativity Check (Does order matter?)
  (ok (not (= (lem-core::mix-hashes 1 2 3)
              (lem-core::mix-hashes 3 2 1))))
  
  ;; 2. The Cancellation Check (Do duplicate values erase each other?)
  (ok (not (= (lem-core::mix-hashes 42 42)
              (lem-core::mix-hashes 0))))
  
  ;; 3. Type Safety Check (Does it always evaluate to a raw machine integer?)
  (ok (typep (lem-core::mix-hashes "string" 100 'lem-symbol '(:keyword 5))
             'integer)))

(deftest test-compute-line-fingerprint
  (let* ((line-base (lem-core::make-logical-line :string "foo"
                                                 :attributes nil
                                                 :end-of-line-cursor-attribute nil
                                                 :extend-to-end nil
                                                 :line-end-overlay nil))
         (line-a (lem-core::copy-logical-line line-base))
         (line-b (lem-core::copy-logical-line line-base))
         (line-c (lem-core::copy-logical-line line-base)))
    
    ;; Set up line-a and line-b to be deep, structural twins
    (setf (lem-core::logical-line-attributes line-a) 
          '((0 1 color) (0 2 color) (0 3 color) (0 4 color) (0 5 color) (0 6 no-highlight)))
    (setf (lem-core::logical-line-attributes line-b) 
          '((0 1 color) (0 2 color) (0 3 color) (0 4 color) (0 5 color) (0 6 no-highlight)))
    
    ;; Set up line-c to diverge past the 5th element (Old sxhash shallow-hash limit)
    (setf (lem-core::logical-line-attributes line-c) 
          '((0 1 color) (0 2 color) (0 3 color) (0 4 color) (0 5 color) (0 6 paredit-highlight)))
    
    ;; 1. Identity Check: Twin lines under twin scroll values must yield twin hashes
    (ok (= (lem-core::compute-line-fingerprint line-a 10 5)
           (lem-core::compute-line-fingerprint line-b 10 5)))
    
    ;; 2. Shallow Hash Fix Check: Verifies deep changes (like Paredit) are caught
    (ok (not (= (lem-core::compute-line-fingerprint line-a 10 5)
                (lem-core::compute-line-fingerprint line-c 10 5))))
    
    ;; 3. Parameter Commutativity Check: Swapping scroll-start and layout widths shifts the hash
    (ok (not (= (lem-core::compute-line-fingerprint line-a 10 5)
                (lem-core::compute-line-fingerprint line-a 5 10))))
    
    ;; 4. Parameter Cancellation Check: Matching coordinate variables don't zero out
    (ok (not (= (lem-core::compute-line-fingerprint line-a 15 15)
                (lem-core::compute-line-fingerprint line-a 30 30))))))
