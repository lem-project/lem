(cl-lsp/defpackage:defpackage :cl-lsp/formatting
  (:use :cl
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/logger
        :lem-base)
  (:shadow :indent-line)
  (:import-from :lem-lisp-syntax
                :calc-indent)
  (:export :on-type-formatting
           :range-formatting
           :buffer-formatting))
(in-package :cl-lsp/formatting)

(defun indent-line (p &optional editp)
  (unless (blank-line-p p)
    (let ((line-number (1- (line-number-at-point p)))
          (old-charpos (point-charpos (back-to-indentation p)))
          (new-column (calc-indent (copy-point p :temporary))))
      (when new-column
        (when editp
          (line-start p)
          (delete-character p old-charpos)
          (insert-character p #\space new-column))
        (convert-to-hash-table
         (make-instance '|TextEdit|
                        :|range| (make-instance
                                  '|Range|
                                  :|start| (make-instance '|Position|
                                                          :|line| line-number
                                                          :|character| 0)
                                  :|end| (make-instance '|Position|
                                                        :|line| line-number
                                                        :|character| old-charpos))
                        :|newText| (make-string new-column :initial-element #\space)))))))

(defun set-formatting-options (options)
  (declare (ignore options))
  ;; TODO
  ;; (setf (tab-size) (slot-value options '|tabSize|))
  )

(defun on-type-formatting (point ch options)
  (declare (ignore ch))
  (set-formatting-options options)
  (let ((edit (indent-line point)))
    (if edit
        (list edit)
        (vector))))

(defun range-formatting (start end options)
  (set-formatting-options options)
  (let ((buffer (point-buffer start))
        (edits '()))
    (buffer-enable-undo buffer)
    (apply-region-lines start end
                        (lambda (point)
                          (multiple-value-bind (edit)
                              (indent-line point t)
                            (when edit
                              (push edit edits)))))
    (buffer-undo start)
    (buffer-disable-undo buffer)
    (list-to-object[] (nreverse edits))))

(defun buffer-formatting (buffer options)
  (with-point ((start (buffer-start-point buffer))
               (end (buffer-end-point buffer)))
    (range-formatting start end options)))
