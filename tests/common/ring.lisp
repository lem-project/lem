(defpackage :lem-tests/common/ring
  (:use :cl :rove :lem-common.ring))
(in-package :lem-tests/common/ring)

(deftest push-back
  (let ((ring (make-ring 10)))
    (ok (string= (with-output-to-string (out)
                   (loop :for v :across "abcdefghijklmnopqrstuvwxyz"
                         :do (push-back ring v)
                             (format out "~A~%" ring)))
                 "#<RING data: #(a 0 0 0 0 0 0 0 0 0) front: 0 rear: 1>
#<RING data: #(a b 0 0 0 0 0 0 0 0) front: 0 rear: 2>
#<RING data: #(a b c 0 0 0 0 0 0 0) front: 0 rear: 3>
#<RING data: #(a b c d 0 0 0 0 0 0) front: 0 rear: 4>
#<RING data: #(a b c d e 0 0 0 0 0) front: 0 rear: 5>
#<RING data: #(a b c d e f 0 0 0 0) front: 0 rear: 6>
#<RING data: #(a b c d e f g 0 0 0) front: 0 rear: 7>
#<RING data: #(a b c d e f g h 0 0) front: 0 rear: 8>
#<RING data: #(a b c d e f g h i 0) front: 0 rear: 9>
#<RING data: #(a b c d e f g h i j) front: 0 rear: 0>
#<RING data: #(k b c d e f g h i j) front: 1 rear: 1>
#<RING data: #(k l c d e f g h i j) front: 2 rear: 2>
#<RING data: #(k l m d e f g h i j) front: 3 rear: 3>
#<RING data: #(k l m n e f g h i j) front: 4 rear: 4>
#<RING data: #(k l m n o f g h i j) front: 5 rear: 5>
#<RING data: #(k l m n o p g h i j) front: 6 rear: 6>
#<RING data: #(k l m n o p q h i j) front: 7 rear: 7>
#<RING data: #(k l m n o p q r i j) front: 8 rear: 8>
#<RING data: #(k l m n o p q r s j) front: 9 rear: 9>
#<RING data: #(k l m n o p q r s t) front: 0 rear: 0>
#<RING data: #(u l m n o p q r s t) front: 1 rear: 1>
#<RING data: #(u v m n o p q r s t) front: 2 rear: 2>
#<RING data: #(u v w n o p q r s t) front: 3 rear: 3>
#<RING data: #(u v w x o p q r s t) front: 4 rear: 4>
#<RING data: #(u v w x y p q r s t) front: 5 rear: 5>
#<RING data: #(u v w x y z q r s t) front: 6 rear: 6>
"))))

(deftest empty-p
  (let ((ring (make-ring 3)))
    (ok (empty-p ring))
    (loop :for i :from 1 :to 10
          :do (push-back ring i)
              (ok (not (empty-p ring))))))

(deftest size-of
  (let ((ring (make-ring 3)))
    (ok (= 0 (size-of ring)))
    (loop :for i :from 1 :to 10
          :do (push-back ring i)
              (ok (= (min 3 i)
                     (size-of ring)))))
  (let ((ring (make-ring 10)))
    (setf (lem-common.ring::ring-front ring) 5
          (lem-common.ring::ring-rear ring) 3
          (lem-common.ring::ring-empty-p ring) nil)
    (ok (= 8 (size-of ring)))))

(deftest peek-back
  (let ((ring (make-ring 10)))
    (loop :for i :from 1 :to 5
          :do (push-back ring i))
    (ok (eql 5 (peek-back ring 0)))
    (ok (eql 4 (peek-back ring 1)))
    (ok (eql 3 (peek-back ring 2)))
    (ok (eql 2 (peek-back ring 3)))
    (ok (eql 1 (peek-back ring 4)))
    (ok (signals (peek-back ring -1) 'invalid-index-error))
    (loop :for i :from 5 :to 20
          :do (ok (signals (peek-back ring i) 'invalid-index-error)))))
