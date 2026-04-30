(defpackage :lem-tests/listener-mode
  (:use :cl
        :rove
        :lem)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :lem/listener-mode
                :listener-mode
                :input-start-point
                :listener-set-prompt-function
                :listener-check-input-function
                :listener-execute-function
                :start-listener-mode
                :refresh-prompt
                :clamp-cursor-to-input-area))
(in-package :lem-tests/listener-mode)

(defun setup-listener-buffer ()
  "Create a buffer with listener-mode and a simple prompt."
  (let ((buffer (make-buffer "listener-test")))
    (setf (current-buffer) buffer)
    (setf (variable-value 'listener-set-prompt-function :buffer buffer)
          (lambda (point)
            (insert-string point "> ")))
    (setf (variable-value 'listener-check-input-function :buffer buffer)
          (lambda (point) (declare (ignore point)) t))
    (setf (variable-value 'listener-execute-function :buffer buffer)
          (lambda (point string) (declare (ignore point string))))
    (start-listener-mode)
    (refresh-prompt buffer)
    buffer))

(deftest clamp-cursor-to-input-area
  (with-fake-interface ()
    (with-current-buffers ()
      (let ((buffer (setup-listener-buffer)))
        (testing "cursor at input-start-point is not moved"
          (move-point (current-point) (input-start-point buffer))
          (lem/listener-mode:clamp-cursor-to-input-area)
          (ok (point= (current-point) (input-start-point buffer))))

        (testing "cursor before input-start-point on same line is clamped"
          (line-start (current-point))
          (ok (point< (current-point) (input-start-point buffer)))
          (lem/listener-mode:clamp-cursor-to-input-area)
          (ok (point= (current-point) (input-start-point buffer))))

        (testing "cursor after input-start-point is not moved"
          (buffer-end (current-point))
          (insert-string (current-point) "hello")
          (let ((pos (copy-point (current-point) :temporary)))
            (lem/listener-mode:clamp-cursor-to-input-area)
            (ok (point= (current-point) pos))))

        (testing "cursor on previous line is not clamped"
          ;; Add a newline in the input area so cursor can go to a previous line
          (move-point (current-point) (input-start-point buffer))
          (character-offset (current-point) -1)
          ;; cursor is now on a line before input-start-point's line
          (unless (same-line-p (current-point) (input-start-point buffer))
            (lem/listener-mode:clamp-cursor-to-input-area)
            (ok (not (point= (current-point) (input-start-point buffer))))))))))
