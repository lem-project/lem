(defpackage :lem-pixel-demo/tests
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-pixel-demo/tests)

(deftest commands-defined
  (testing "All demo commands are defined"
    (ok (fboundp 'lem-pixel-demo:pixel-demo-animate))
    (ok (fboundp 'lem-pixel-demo:pixel-demo-follow-mouse))
    (ok (fboundp 'lem-pixel-demo:pixel-demo-debug))
    (ok (fboundp 'lem-pixel-demo:pixel-demo-compare))
    (ok (fboundp 'lem-pixel-demo:pixel-demo-toggle-mode))
    (ok (fboundp 'lem-pixel-demo:pixel-demo-stop))))

(deftest demo-buffer-creation
  (testing "Demo buffer can be created and updated"
    (lem:with-current-buffers ()
      (with-fake-interface ()
        (let ((buffer (lem-pixel-demo::make-demo-buffer "*Test*" "Hello")))
          (ok (not (null buffer)))
          (ok (string= "*Test*" (lem:buffer-name buffer)))
          ;; Check content
          (ok (search "Hello" (lem:buffer-text buffer))))))))

(deftest cleanup-demo
  (testing "Cleanup works without errors"
    (lem:with-current-buffers ()
      (with-fake-interface ()
        ;; Should not error even when nothing is running
        (ok (null (lem-pixel-demo::cleanup-demo)))))))

(deftest floating-window-pixel-api
  (testing "Floating window pixel API is available"
    (lem:with-current-buffers ()
      (with-fake-interface ()
        (let* ((buffer (lem:make-buffer "*Test Floating*" :temporary t))
               (window (lem:make-floating-window
                        :buffer buffer
                        :x 5 :y 3
                        :width 20 :height 10
                        :pixel-x 100 :pixel-y 60
                        :pixel-width 400 :pixel-height 200)))
          ;; Check pixel coordinates are set
          (ok (= 100 (lem:floating-window-pixel-x window)))
          (ok (= 60 (lem:floating-window-pixel-y window)))
          (ok (= 400 (lem:floating-window-pixel-width window)))
          (ok (= 200 (lem:floating-window-pixel-height window)))
          ;; Test set-pixel-position
          (lem:floating-window-set-pixel-position window 150 80)
          (ok (= 150 (lem:floating-window-pixel-x window)))
          (ok (= 80 (lem:floating-window-pixel-y window)))
          ;; Test set-pixel-size
          (lem:floating-window-set-pixel-size window 500 300)
          (ok (= 500 (lem:floating-window-pixel-width window)))
          (ok (= 300 (lem:floating-window-pixel-height window)))
          ;; Test pixel-bounds
          (multiple-value-bind (px py pw ph)
              (lem:floating-window-pixel-bounds window)
            (ok (= 150 px))
            (ok (= 80 py))
            (ok (= 500 pw))
            (ok (= 300 ph)))
          ;; Cleanup
          (lem:delete-window window))))))
