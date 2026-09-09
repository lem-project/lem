(defpackage :lem/named-point
  (:use :cl
        :lem-core)
  (:import-from :alexandria
                :if-let
                :when-let)
  (:export :set-named-point
           :get-named-point
           :delete-named-point
           :goto-named-point))

(in-package :lem/named-point)

(let ((stored-points (make-hash-table :test 'equal)))

  (defun set-named-point (name &optional (point (current-point)))
    "Store point with name. Default point is current point."
    (let ((old (gethash name stored-points)))
      (when old (delete-point old)))
    (setf (gethash name stored-points) (copy-point point)))

  (defun get-named-point (name)
    "Retrieve a point stored with name if exists and is still alive"
    (when-let ((point (gethash name stored-points)))
      (if (and (not (deleted-buffer-p (point-buffer point)))
               (alive-point-p point))
          point
          (progn (delete-named-point name)
                 nil))))

  (defun delete-named-point (name)
    "Delete point with name"
    (when-let ((point (gethash name stored-points)))
      (delete-point point)
      (remhash name stored-points))))

(defun goto-named-point (name &key (global t))
  "Go to named point"
  (when-let ((point (get-named-point name)))
    (let* ((b (point-buffer point))
           (same-buffer (eq b (current-buffer))))
      (if same-buffer
          (move-point (current-point) point)
          (when global
            (switch-to-buffer b)
            (move-point (current-point) point))))))
