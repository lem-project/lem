(defpackage :lem-tests/syntax-scanner
  (:use :cl :rove)
  (:import-from :lem))
(in-package :lem-tests/syntax-scanner)

;;;; Tests for viewport-based syntax scanning (src/syntax-scanner.lisp)
;;;;
;;;; The scanned region tracking mechanism allows efficient syntax highlighting
;;;; by only scanning regions that haven't been scanned yet within the same
;;;; buffer modification tick.

;;; Helper to access internal functions
(defun buffer-scanned-region (buffer)
  (lem-core::buffer-scanned-region buffer))

(defun (setf buffer-scanned-region) (value buffer)
  (setf (lem-core::buffer-scanned-region buffer) value))

(defun update-scanned-region (buffer start-line end-line)
  (lem-core::update-scanned-region buffer start-line end-line))

(defun viewport-needs-scan-p (buffer start-line end-line)
  (lem-core::viewport-needs-scan-p buffer start-line end-line))

;;;; Unit Tests for buffer-scanned-region

(deftest buffer-scanned-region-initial-value
  (testing "Initially, buffer has no scanned region"
    (let ((buffer (lem:make-buffer "*test-scanned-region*" :temporary t)))
      (unwind-protect
          (ok (null (buffer-scanned-region buffer)))
        (lem:delete-buffer buffer)))))

(deftest buffer-scanned-region-set-and-get
  (testing "Can set and get scanned region"
    (let ((buffer (lem:make-buffer "*test-scanned-region*" :temporary t)))
      (unwind-protect
          (progn
            (setf (buffer-scanned-region buffer) '(1 10 20))
            (let ((region (buffer-scanned-region buffer)))
              (ok (equal region '(1 10 20)))))
        (lem:delete-buffer buffer)))))

;;;; Unit Tests for update-scanned-region

(deftest update-scanned-region-new-region
  (testing "Creates new region when none exists"
    (let ((buffer (lem:make-buffer "*test-update-region*" :temporary t)))
      (unwind-protect
          (let ((tick (lem:buffer-modified-tick buffer)))
            (update-scanned-region buffer 10 30)
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (first region) tick))
              (ok (= (second region) 10))
              (ok (= (third region) 30))))
        (lem:delete-buffer buffer)))))

(deftest update-scanned-region-extends-range
  (testing "Extends existing region when tick matches"
    (let ((buffer (lem:make-buffer "*test-extend-region*" :temporary t)))
      (unwind-protect
          (let ((tick (lem:buffer-modified-tick buffer)))
            ;; Initial scan: lines 10-30
            (update-scanned-region buffer 10 30)
            ;; Extend upward: lines 5-20
            (update-scanned-region buffer 5 20)
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (first region) tick))
              (ok (= (second region) 5) "Start should extend to 5")
              (ok (= (third region) 30) "End should remain 30"))
            ;; Extend downward: lines 25-50
            (update-scanned-region buffer 25 50)
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (second region) 5) "Start should remain 5")
              (ok (= (third region) 50) "End should extend to 50")))
        (lem:delete-buffer buffer)))))

(deftest update-scanned-region-resets-on-tick-change
  (testing "Resets region when tick changes (buffer modified)"
    (let ((buffer (lem:make-buffer "*test-reset-region*" :temporary t)))
      (unwind-protect
          (progn
            ;; Initial scan: lines 10-30
            (update-scanned-region buffer 10 30)
            (let ((old-tick (first (buffer-scanned-region buffer))))
              ;; Modify buffer (this increments tick)
              (lem:insert-string (lem:buffer-point buffer) "hello")
              (let ((new-tick (lem:buffer-modified-tick buffer)))
                (ok (/= old-tick new-tick) "Tick should change after edit")
                ;; Scan new region: lines 1-5
                (update-scanned-region buffer 1 5)
                (let ((region (buffer-scanned-region buffer)))
                  (ok (= (first region) new-tick) "Should use new tick")
                  (ok (= (second region) 1) "Start should be 1")
                  (ok (= (third region) 5) "End should be 5")))))
        (lem:delete-buffer buffer)))))

;;;; Unit Tests for viewport-needs-scan-p

(deftest viewport-needs-scan-when-no-region
  (testing "Needs scan when no region exists"
    (let ((buffer (lem:make-buffer "*test-needs-scan*" :temporary t)))
      (unwind-protect
          (ok (viewport-needs-scan-p buffer 1 30))
        (lem:delete-buffer buffer)))))

(deftest viewport-needs-scan-when-tick-changed
  (testing "Needs scan when tick has changed"
    (let ((buffer (lem:make-buffer "*test-needs-scan*" :temporary t)))
      (unwind-protect
          (progn
            (update-scanned-region buffer 1 30)
            ;; Modify buffer
            (lem:insert-string (lem:buffer-point buffer) "x")
            ;; Same viewport should need scan because tick changed
            (ok (viewport-needs-scan-p buffer 1 30)))
        (lem:delete-buffer buffer)))))

(deftest viewport-needs-scan-when-outside-scanned-range
  (testing "Needs scan when viewport is outside scanned range"
    (let ((buffer (lem:make-buffer "*test-needs-scan*" :temporary t)))
      (unwind-protect
          (progn
            (update-scanned-region buffer 10 30)
            ;; Viewport before scanned region
            (ok (viewport-needs-scan-p buffer 1 9)
                "Should need scan for lines before scanned region")
            ;; Viewport after scanned region
            (ok (viewport-needs-scan-p buffer 31 50)
                "Should need scan for lines after scanned region")
            ;; Viewport overlapping start
            (ok (viewport-needs-scan-p buffer 5 20)
                "Should need scan when start is before scanned region")
            ;; Viewport overlapping end
            (ok (viewport-needs-scan-p buffer 25 40)
                "Should need scan when end is after scanned region"))
        (lem:delete-buffer buffer)))))

(deftest viewport-no-scan-when-within-scanned-range
  (testing "Does not need scan when viewport is within scanned range"
    (let ((buffer (lem:make-buffer "*test-no-scan*" :temporary t)))
      (unwind-protect
          (progn
            (update-scanned-region buffer 1 50)
            ;; Viewport fully within scanned region
            (ng (viewport-needs-scan-p buffer 10 30)
                "Should not need scan when fully within scanned region")
            ;; Exact match
            (ng (viewport-needs-scan-p buffer 1 50)
                "Should not need scan when exactly matching scanned region")
            ;; Single line within range
            (ng (viewport-needs-scan-p buffer 25 25)
                "Should not need scan for single line within range"))
        (lem:delete-buffer buffer)))))

;;;; Integration Tests

(deftest syntax-scan-workflow
  (testing "Complete workflow: open -> scroll -> edit -> scroll"
    (let ((buffer (lem:make-buffer "*test-workflow*" :temporary t)))
      (unwind-protect
          (progn
            ;; Insert some content
            (let ((point (lem:buffer-point buffer)))
              (dotimes (i 100)
                (lem:insert-string point (format nil "Line ~D~%" i))))

            ;; Clear the region to simulate fresh state
            (setf (buffer-scanned-region buffer) nil)

            ;; Step 1: Initial viewport (lines 1-30)
            (ok (viewport-needs-scan-p buffer 1 30)
                "Initial viewport needs scan")
            (update-scanned-region buffer 1 30)
            (ng (viewport-needs-scan-p buffer 1 30)
                "Same viewport should not need scan after update")

            ;; Step 2: Scroll down (lines 20-50) - extends beyond scanned
            (ok (viewport-needs-scan-p buffer 20 50)
                "Scrolled viewport needs scan (extends beyond scanned)")
            (update-scanned-region buffer 20 50)
            ;; Region should now be 1-50
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (second region) 1))
              (ok (= (third region) 50)))

            ;; Step 3: Scroll within scanned range (lines 10-40)
            (ng (viewport-needs-scan-p buffer 10 40)
                "Viewport within scanned range should not need scan")

            ;; Step 4: Scroll to adjacent region (lines 45-75) - contiguous with 1-50
            (ok (viewport-needs-scan-p buffer 45 75)
                "Viewport extending beyond scanned range needs scan")
            (update-scanned-region buffer 45 75)
            ;; Region should now be 1-75 (extended because contiguous)
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (second region) 1))
              (ok (= (third region) 75)))

            ;; Step 5: Jump to non-contiguous region (lines 90-100)
            ;; This should NOT extend but replace (gap between 75 and 90)
            (ok (viewport-needs-scan-p buffer 90 100)
                "Non-contiguous viewport needs scan")
            (update-scanned-region buffer 90 100)
            ;; Region should now be 90-100 (replaced, not extended)
            (let ((region (buffer-scanned-region buffer)))
              (ok (= (second region) 90) "Start should be 90 (replaced)")
              (ok (= (third region) 100) "End should be 100 (replaced)"))

            ;; Step 6: Previous region (1-75) now needs scan again
            (ok (viewport-needs-scan-p buffer 1 30)
                "Previous region needs scan after jump to non-contiguous area"))
        (lem:delete-buffer buffer)))))

(deftest syntax-scan-after-edit
  (testing "After buffer edit, viewport needs rescan"
    (let ((buffer (lem:make-buffer "*test-edit*" :temporary t)))
      (unwind-protect
          (progn
            ;; Setup: insert content and scan initial viewport
            (let ((point (lem:buffer-point buffer)))
              (dotimes (i 50)
                (lem:insert-string point (format nil "Line ~D~%" i))))
            (setf (buffer-scanned-region buffer) nil)

            ;; Scan lines 1-30
            (update-scanned-region buffer 1 30)
            (ng (viewport-needs-scan-p buffer 1 30)
                "Initial scan complete")

            ;; Edit buffer - tick changes
            (lem:buffer-start (lem:buffer-point buffer))
            (lem:insert-string (lem:buffer-point buffer) "EDIT\n")

            ;; After edit, same viewport needs scan (tick changed)
            (ok (viewport-needs-scan-p buffer 1 30)
                "After edit, viewport needs scan due to tick change")

            ;; Scan the viewport again
            (update-scanned-region buffer 1 30)
            (ng (viewport-needs-scan-p buffer 1 30)
                "After rescan, viewport should not need scan")

            ;; Different viewport also needs scan
            (ok (viewport-needs-scan-p buffer 35 50)
                "Unscanned viewport needs scan"))
        (lem:delete-buffer buffer)))))

(deftest scanned-region-boundary-cases
  (testing "Boundary cases for scanned region"
    (let ((buffer (lem:make-buffer "*test-boundary*" :temporary t)))
      (unwind-protect
          (progn
            ;; Single line scan
            (update-scanned-region buffer 10 10)
            (ng (viewport-needs-scan-p buffer 10 10)
                "Single line should be marked as scanned")
            (ok (viewport-needs-scan-p buffer 9 10)
                "Adjacent line before should need scan")
            (ok (viewport-needs-scan-p buffer 10 11)
                "Adjacent line after should need scan")

            ;; Extend to adjacent lines
            (update-scanned-region buffer 9 11)
            (ng (viewport-needs-scan-p buffer 9 11)
                "Extended range should be scanned"))
        (lem:delete-buffer buffer)))))

