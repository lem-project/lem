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

(defun go-to-defpackage-spec-form (point spec-name &optional move-to-last)
  (when (go-to-defpackage-form point)
    (with-point ((limit point))
      (form-offset limit 1)
      (cond ((search-forward-symbol point spec-name limit)
             (values point t))
            (t
             (form-offset point 1)
             (scan-lists point -1 -1)
             (insert-character point #\newline)
             (indent-line point)
             (insert-string point (format nil "(~A)" spec-name))
             (character-offset point -1)
             (values point nil))))))

(define-command lisp-set-export-to-defpackage () ()
  (let ((symbols (collect-export-mark-symbols (current-buffer)))
        (point (current-point)))
    (multiple-value-bind (point exists)
        (go-to-defpackage-spec-form point ":export")
      (when point
        (when exists
          (with-point ((end point :left-inserting))
            (scan-lists end 1 1)
            (scan-lists end -1 -1)
            (delete-between-points point end)))
        (dolist (symbol symbols)
          (fresh-line* point)
          (indent-line point)
          (insert-string point (format nil ":~(~A~)" symbol)))))
    #+(or)
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

(defun find-symbol-matchies (symbol-name)
  (let ((symbols '()))
    (do-all-symbols (s)
      (when (and (string-equal s symbol-name) (fboundp s))
        (pushnew s symbols)))
    symbols))

(defun select-menu (items)
  (let (selected-item)
    (lem-if:display-popup-menu (implementation)
                               items
                               :print-spec #'princ-to-string
                               :action-callback (lambda (item)
                                                  (setf selected-item item)))
    (loop
      (redraw-display)
      (let ((key (read-key)))
        (cond ((or (match-key key :sym "Down")
                   (match-key key :ctrl t :sym "n"))
               (lem-if:popup-menu-down (implementation)))
              ((or (match-key key :sym "Up")
                   (match-key key :ctrl t :sym "p"))
               (lem-if:popup-menu-up (implementation)))
              ((match-key key :sym "Return")
               (lem-if:popup-menu-select (implementation))
               (lem-if:popup-menu-quit (implementation))
               (return selected-item))
              ((match-key key :sym "q")
               (lem-if:popup-menu-quit (implementation))
               (return nil)))))))

(define-command lisp-add-missing-import-from (symbol-name)
    ((list (prompt-for-symbol-name "Symbol: " (symbol-string-at-point (current-point)))))
  (multiple-value-bind (symbol external-p symbol-name package)
      (swank::parse-symbol symbol-name)
    (let ((point (current-point)))
      (multiple-value-bind (point exists)
          (go-to-defpackage-spec-form point ":import-from")
        (when point
          )))))
