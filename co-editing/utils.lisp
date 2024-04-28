(defpackage :lem/co-editing/utils
  (:use :cl)
  (:export :hash
           :random-uuid
           :with-hash-bindings
           :pretty-json
           :find-or-create-buffer
           :insert-string
           :delete-string
           :replace-text
           :move-point))
(in-package :lem/co-editing/utils)

(defun hash (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun random-uuid ()
  (fuuid:to-string (fuuid:make-v4)))

(defun construct-bindings (vars params)
  (loop :for var :in vars
        :collect `(,var (gethash ,(string-downcase var) ,params))))

(defmacro with-hash-bindings ((&rest vars) params &body body)
  (alexandria:once-only (params)
    `(let ,(construct-bindings vars params)
       ,@body)))

(defun pretty-json (params)
  (with-output-to-string (stream)
    (yason:encode params (yason:make-json-output-stream stream))))

(defun find-or-create-buffer (buffer-or-name)
  (etypecase buffer-or-name
    (lem:buffer buffer-or-name)
    (string (or (lem:get-buffer buffer-or-name)
                (lem:make-buffer buffer-or-name :enable-undo-p nil)))))

(defun insert-string (buffer-or-name line character string)
  (let ((buffer (find-or-create-buffer buffer-or-name)))
    (lem:with-point ((point (lem:buffer-point buffer)))
      (lem:move-to-line point line)
      (lem:line-offset point 0 character)
      (lem:insert-string point string))
    buffer))

(defun delete-string (buffer-or-name line character n)
  (let ((buffer (find-or-create-buffer buffer-or-name)))
    (lem:with-point ((point (lem:buffer-point buffer)))
      (lem:move-to-line point line)
      (lem:line-offset point 0 character)
      (lem:delete-character point n))
    buffer))

(defun replace-text (buffer-or-name text)
  (let* ((buffer (find-or-create-buffer buffer-or-name))
         (positions
           (loop :for point :in (lem/buffer/internal::buffer-points buffer)
                 :unless (member point
                                 (list (lem:buffer-start-point buffer)
                                       (lem:buffer-end-point buffer)))
                 :collect (list point
                                (lem:line-number-at-point point)
                                (lem:point-charpos point)))))
    (lem:buffer-disable-undo-boundary buffer)
    (lem:delete-between-points (lem:buffer-start-point buffer)
                               (lem:buffer-end-point buffer))
    (assert (equal "" (lem:buffer-text buffer)))
    (lem:insert-string (lem:buffer-point buffer) text)
    (lem:buffer-enable-undo-boundary buffer)
    (loop :for (point line character) :in positions
          :do (move-point point line character))
    buffer))

(defun move-point (point line character)
  (lem:move-to-line point line)
  (lem:line-offset point 0 character))
