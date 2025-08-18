(defpackage :lem-tests/window
  (:use :cl :rove)
  (:import-from :lem-fake-interface
                :fake-interface
                :with-fake-interface))
(in-package :lem-tests/window)

(deftest delete-window-test
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (assert (lem:one-window-p))
      (ok (equal "Can not delete this window"
                 (handler-case (lem:delete-window (lem:current-window))
                   (error (e) (princ-to-string e)))))
      (ok (not (lem-core::window-deleted-p (lem:current-window)))))))

(defun pop-attached-window (buffer)
  (let ((window (lem:pop-to-buffer buffer)))
    (lem:switch-to-window window)))

(defun make-attached-buffer ()
  (let ((buffer (lem:make-buffer "test" :enable-undo-p nil))
        (attached-buffer (lem:make-buffer (format nil "~A (attached)" "test")
                                          :temporary t)))
    (lem:attach-buffer buffer attached-buffer)
    buffer))

(deftest attached-window/delete-other-windows
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (pop-attached-window (make-attached-buffer))

      (assert (= 2 (length (lem:window-list))))
      (assert (lem:attached-window-p (lem:current-window)))

      (lem:delete-other-windows)

      (ok (= 1 (length (lem:window-list))))
      (ok (lem:attached-window-p (lem:current-window))))))

(deftest attached-window/switch-to-buffer/change-attached-to-normal
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (pop-attached-window (make-attached-buffer))
      (assert (= 2 (length (lem:window-list))))
      (assert (lem:attached-window-p (lem:current-window)))
      (let* ((attached-window (lem:current-window))
             (attached-buffer (lem:window-buffer attached-window))
             (parent-window (lem-core::attached-window-parent-window attached-window))
             (new-buffer (lem:make-buffer "foo")))
        (assert (lem-core::attached-buffer-p attached-buffer))
        (assert parent-window)

        (lem:switch-to-buffer new-buffer)

        (ok (null (lem-core::attached-window-parent-window attached-window)))
        (ok (lem:deleted-window-p attached-window))
        (ok (eq attached-buffer (lem:window-buffer attached-window)))
        (ok (null (lem-core::window-attached-window parent-window)))
        (ok (eq new-buffer (lem:window-buffer parent-window)))))))

(deftest attached-window/switch-to-buffer/change-normal-to-attached
  (lem:with-current-buffers ()
    (with-fake-interface ()
      (let ((buffer (make-attached-buffer)))
        (assert (= 1 (length (lem:window-list))))

        (lem:switch-to-buffer buffer)

        (ok (= 1 (length (lem:window-list))))
        (ok (lem:attached-window-p (lem:current-window)))
        (ok (eq (lem-core::buffer-attached-buffer buffer)
                (lem:window-buffer (lem:current-window))))))))
