(cl-lsp/defpackage:defpackage :cl-lsp/editor
  (:use :cl)
  (:export :make-file-contents-position
           :make-file-contents-range
           :open-file-contents
           :close-file-contents
           :replace-file-contents
           :edit-file-contents))
(in-package :cl-lsp/editor)

(defgeneric open-file-contents-using-editor (editor uri text))
(defgeneric close-file-contents-using-editor (editor file-contents))
(defgeneric replace-file-contents-using-editor (editor file-contents text))
(defgeneric edit-file-contents-using-editor (editor file-contents range text))

(defclass editor () ())

(defclass lem (editor) ())

(defstruct file-contents-position
  line
  character)

(defstruct file-contents-range
  start
  end)

(defmethod open-file-contents-using-editor ((editor lem) uri text)
  (let ((buffer
          (lem-base:make-buffer uri
                                :temporary t
                                :syntax-table lem-lisp-syntax:*syntax-table*)))
    (lem-base:insert-string (lem-base:buffer-point buffer) text)
    buffer))

(defmethod close-file-contents-using-editor ((editor lem) file-contents)
  (check-type file-contents lem-base:buffer)
  (lem-base:delete-buffer file-contents)
  (values))

(defun move-to-position (point position)
  (lem-base:move-to-line point (1- (file-contents-position-line position)))
  (lem-base:line-offset point 0 (file-contents-position-character position)))

(defmethod replace-file-contents-using-editor ((editor lem) file-contents text)
  (let ((buffer file-contents))
    (lem-base:erase-buffer buffer)
    (lem-base:insert-string (lem-base:buffer-point buffer) text)))

(defmethod edit-file-contents-using-editor ((editor lem) file-contents range text)
  (let* ((buffer file-contents)
         (point (lem-base:buffer-point buffer)))
    (move-to-position point (file-contents-range-start range))
    (lem-base:with-point ((end point))
      (move-to-position end (file-contents-range-end range))
      (lem-base:delete-between-points point end))
    (lem-base:insert-string point text)))


(defvar *editor* (make-instance 'lem))

(defun open-file-contents (uri text)
  (open-file-contents-using-editor *editor* uri text))

(defun close-file-contents (file-contents)
  (close-file-contents-using-editor *editor* file-contents))

(defun replace-file-contents (file-contents text)
  (replace-file-contents-using-editor *editor* file-contents text))

(defun edit-file-contents (file-contents range text)
  (edit-file-contents-using-editor *editor* file-contents range text))
