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
  (ok (typep (lem-core::mix-hashes "string" 100 'some-symbol '(:keyword 5))
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

(deftest test-evict-line-fingerprints-from
  ;; When the tail of a window is blanked by clear-to-end-of-window (e.g. after
  ;; deleting a large region), the fingerprint entries for those rows must be
  ;; dropped.  Otherwise undoing the deletion restores content whose fingerprint
  ;; matches the stale entry, the render is skipped, and the row stays blank on
  ;; persistent-texture frontends (SDL2).  `evict-line-fingerprints-from` is an
  ;; unexported display-layer internal, accessed here for a white-box unit test.
  (let ((cache (make-hash-table :test 'eql)))
    (setf (gethash 0 cache) (cons 111 10))
    (setf (gethash 10 cache) (cons 222 10))
    (setf (gethash 20 cache) (cons 333 10))
    ;; `lem-core::` reaches an unexported internal on purpose: this helper
    ;; has no public equivalent and is exercised directly as a white-box test.
    (lem-core::evict-line-fingerprints-from cache 10)
    ;; Rows at or below the cleared y are gone; rows above are kept.
    (ok (nth-value 1 (gethash 0 cache)))
    (ng (nth-value 1 (gethash 10 cache)))
    (ng (nth-value 1 (gethash 20 cache)))
    (ok (= 1 (hash-table-count cache)))))

(deftest test-remove-drawing-cache-entries-from
  ;; The drawing-object cache (keyed by screen y) has the same stale-tail
  ;; hazard as the fingerprint cache: rows blanked by clear-to-end-of-window
  ;; must be dropped so a later frame whose restored objects match a stale
  ;; entry does not pass validate-cache-p and skip the render (SDL2 invisible
  ;; text after undoing a large deletion).  `remove-drawing-cache-entries-from`
  ;; is an unexported display-layer internal, accessed here for a white-box
  ;; unit test over the (y height objects) entry list.
  (let ((entries (list (list 0 10 nil) (list 10 10 nil) (list 20 10 nil))))
    ;; `lem-core::` reaches an unexported internal on purpose: this helper
    ;; has no public equivalent and is exercised directly as a white-box test.
    ;; Only the row above the cleared y survives.
    (ok (equal (lem-core::remove-drawing-cache-entries-from entries 10)
               (list (list 0 10 nil))))
    ;; A y past all entries removes nothing.
    (ok (= 3 (length (lem-core::remove-drawing-cache-entries-from entries 30))))
    ;; A y of 0 removes everything.
    (ok (null (lem-core::remove-drawing-cache-entries-from entries 0)))))

(deftest test-fingerprint-detects-attribute-mutation
  ;; An attribute mutated in place (e.g. recoloring the shared `cursor`
  ;; attribute via SET-ATTRIBUTE, as vi-mode/skk-mode do) keeps the same
  ;; object identity while its content changes.  Because SXHASH on a
  ;; standard-object is identity-based in SBCL, the fingerprint would not
  ;; change and the line would be skipped on redraw, leaving stale pixels on
  ;; persistent textures (SDL2 ghosting).  The fingerprint must change.

  ;; Mutation referenced through the attributes list.
  (let* ((attribute (lem-core::make-attribute :foreground "#FF0000"))
         (line (lem-core::make-logical-line
                :string "foo"
                :attributes (list (list 0 3 attribute))
                :end-of-line-cursor-attribute nil
                :extend-to-end nil
                :line-end-overlay nil))
         (before (lem-core::compute-line-fingerprint line 0 0)))
    (lem-core::set-attribute attribute :background "#00FF00")
    (ok (not (= before (lem-core::compute-line-fingerprint line 0 0)))))

  ;; Mutation referenced through the end-of-line cursor attribute.
  (let* ((cursor (lem-core::make-attribute :background "#FFFFFF"))
         (line (lem-core::make-logical-line
                :string "foo"
                :attributes nil
                :end-of-line-cursor-attribute cursor
                :extend-to-end nil
                :line-end-overlay nil))
         (before (lem-core::compute-line-fingerprint line 0 0)))
    (lem-core::set-attribute cursor :background "#000000")
    (ok (not (= before (lem-core::compute-line-fingerprint line 0 0))))))
