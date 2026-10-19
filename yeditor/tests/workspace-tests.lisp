;;;; SPDX-License-Identifier: MIT
(defpackage :yeditor-tx/workspace-tests
  (:use :cl :rove))
(in-package :yeditor-tx/workspace-tests)

(defun queue-append (buffer text)
  (let ((installed-p nil)
        (position nil))
    (yeditor-context:effect
     :workspace :buffer-modification
     (lambda ()
       (let ((point (lem:buffer-point buffer)))
         (lem:buffer-end point)
         (setf position (lem:position-at-point point))
         (lem:with-inhibit-undo () (lem:insert-string point text))
         (setf installed-p t)))
     :undo-thunk
     (lambda ()
       (when installed-p
         (let ((point (lem:buffer-point buffer)))
           (lem:move-to-position point position)
           (lem:with-inhibit-undo () (lem:delete-character point (length text)))
           (setf installed-p nil)))))))

(deftest workspace-preserves-native-undo-and-redo
  (dotimes (cycle 100)
    (let* ((buffer (lem:make-buffer "workspace-transaction-test" :temporary t))
           (point (lem:buffer-point buffer))
           (log (yeditor-tx:make-transaction-log)))
      (lem:with-current-buffer buffer
        (unwind-protect
             (progn
               (lem:insert-string point "human")
               (lem:buffer-undo-boundary buffer)
               (lem:insert-string point "-redo")
               (lem:buffer-undo-boundary buffer)
               (lem:buffer-undo point)
               (ok (equal "human" (lem:buffer-text buffer)))
               (yeditor-tx:with-transaction (log)
                 (yeditor-tx:with-workspace-transaction (:workspace)
                   (queue-append buffer "-agent")
                   (queue-append buffer "-group")))
               (yeditor-tx:commit log)
               (ok (equal "human-agent-group" (lem:buffer-text buffer)))
               (yeditor-tx:rollback log)
               (ok (equal "human" (lem:buffer-text buffer)))
               ;; Native redo survived both the forward edits and rollback.
               (lem:buffer-redo point)
               (ok (equal "human-redo" (lem:buffer-text buffer)))
               (lem:buffer-undo point)
               (ok (equal "human" (lem:buffer-text buffer)))
               ;; The pre-existing human undo group still works as well.
               (lem:buffer-undo point)
               (ok (equal "" (lem:buffer-text buffer))))
          (lem:delete-buffer buffer))))))

(deftest transaction-api-regressions
  (ok (yeditor-tx/commit-tests:run-tests)))
