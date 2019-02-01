(defpackage :lem-lisp-mode.misc-commands
  (:use :cl :lem :lem-lisp-mode))
(in-package :lem-lisp-mode.misc-commands)

(defparameter *defpackage-names*
  '("defpackage"
    "cl:defpackage"
    "common-lisp:defpackage"
    "define-package"
    "uiop:define-package"))

(define-key lem-lisp-mode:*lisp-mode-keymap* "C-c C-q" 'lisp-quickload)

(define-command lisp-quickload (system-name)
    ((list (prompt-for-symbol-name "System: " (lem-lisp-mode::buffer-package (current-buffer)))))
  (check-connection)
  (eval-with-transcript `(ql:quickload ,(string system-name))))

(defun go-to-defpackage-form (point)
  (buffer-start point)
  (loop
    (unless (scan-lists point 1 -1 t)
      (return nil))
    (skip-whitespace-forward point)
    (if (member (symbol-string-at-point point) *defpackage-names*
                :test #'string-equal)
        (return (scan-lists point -1 1))
        (scan-lists point 1 1 t))))

(defun collect-export-mark-symbols (buffer)
  (with-open-stream (stream (make-buffer-input-stream (buffer-start-point buffer)))
    (let ((symbols '()))
      (handler-case
          (loop :for form := (read stream nil nil)
                :while form
                :do (when (eq form :export)
                      (let ((form (read stream nil nil)))
                        (when form
                          (push (cl-annot.doc::definition-form-symbol form)
                                symbols)))))
        (end-of-file ()))
      (nreverse symbols))))

(defun fresh-line* (point)
  (unless (with-point ((p point))
            (skip-whitespace-backward p t)
            (start-line-p p))
    (insert-character point #\newline)))

(define-command lisp-set-export-to-defpackage () ()
  (let ((symbols (collect-export-mark-symbols (current-buffer)))
        (point (current-point)))
    (when (go-to-defpackage-form point)
      (with-point ((limit point))
        (form-offset limit 1)
        (cond ((search-forward-symbol point ":export" limit)
               (with-point ((limit point :left-inserting))
                 (scan-lists limit 1 1)
                 (scan-lists limit -1 -1)
                 (delete-between-points point limit)))
              (t
               (form-offset point 1)
               (scan-lists point -1 -1)
               (insert-character point #\newline)
               (indent-line point)
               (insert-string point "(:export)")
               (character-offset point -1)))
        (dolist (symbol symbols)
          (fresh-line* point)
          (indent-line point)
          (insert-string point (format nil ":~(~A~)" symbol)))))))
