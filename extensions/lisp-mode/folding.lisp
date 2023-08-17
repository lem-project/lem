(defpackage :lem-lisp-mode/folding
  (:use :cl :lem))
(in-package :lem-lisp-mode/folding)

(define-key lem-lisp-mode/internal:*lisp-mode-keymap* "C-x C-t" 'toggle-current-definition)
(define-key lem-lisp-mode/internal:*lisp-mode-keymap* "C-x T" 'fold-or-unfold-buffer-definitions)

(defparameter *folding-text* " ...")

(define-attribute folding-attribute (t))

(defun call-with-toplevel-form-points (point function)
  (with-point ((start point :right-inserting)
               (end point :right-inserting))
    (loop :while (backward-up-list start t))
    (form-offset (move-point end start) 1)
    (funcall function start end)))

(defmacro with-toplevel-form-points ((start end point) &body body)
  `(call-with-toplevel-form-points ,point (lambda (,start ,end) ,@body)))

(defun fold-form-p (point)
  (with-toplevel-form-points (start end point)
    (not (null (next-single-property-change start :folding-text end)))))

(defun fold-at-point (point)
  (unless (fold-form-p point)
    (with-toplevel-form-points (form-start form-end point)
      (with-point ((folding-start point)
                   (folding-end point))
        (form-offset (forward-down-list (move-point folding-start form-start)) 2)
        (character-offset (move-point folding-end form-end) -1)
        (let ((folding-text (points-to-string folding-start folding-end)))
          (delete-between-points folding-start folding-end)
          (insert-string folding-start *folding-text* :folding-region t)
          (put-text-property form-start form-end :read-only t)
          (put-text-property form-start form-end :folding-text folding-text))
        (with-point ((start point)
                     (end point))
          (line-start start)
          (line-end end)
          (put-text-property start end :sticky-attribute 'folding-attribute))
        (move-point point folding-start)))))

(defun unfold-at-point (point)
  (when (fold-form-p point)
    (with-toplevel-form-points (start end point)
      (let ((folding-text (text-property-at start :folding-text)))
        (remove-text-property start end :read-only)
        (remove-text-property start end :folding-text)
        (assert (next-single-property-change start :folding-region end))
        (with-point ((replace-start start)
                     (replace-end start))
          (character-offset replace-end (length *folding-text*))
          (delete-between-points replace-start replace-end)
          (insert-string replace-start folding-text)))
      (move-point point start))))

(defun fold-reference (reference)
  (let ((point (lem/detective:reference-point reference)))
    (fold-at-point point)))

(define-command toggle-current-definition () ()
  (lem/detective::check-change :force t :buffer (current-buffer))
  (if (fold-form-p (current-point))
      (unfold-at-point (current-point))
      (fold-at-point (current-point))))

(defun foldable-reference-p (reference)
  (typep reference
         '(or
           lem/detective:function-reference
           lem/detective:class-reference
           lem/detective:variable-reference
           lem/detective:misc-reference)))

(defun get-buffer-references (buffer)
  (let ((references (lem/detective:buffer-references buffer)))
    (sort (remove-if-not #'foldable-reference-p
                         (lem/detective::references-all references))
          #'point>
          :key #'lem/detective:reference-point)))

(define-command fold-buffer-definitions () ()
  (let ((buffer (current-buffer)))
    (lem/detective::check-change :force t :buffer buffer)
    (dolist (reference (get-buffer-references buffer))
      (fold-reference reference))))

(define-command unfold-buffer-definitions () ()
  (let ((buffer (current-buffer)))
    (lem/detective::check-change :force t :buffer buffer)
    (dolist (reference (get-buffer-references buffer))
      (unfold-at-point (lem/detective:reference-point reference)))))

(define-command fold-or-unfold-buffer-definitions (arg) ("P")
  (if arg
      (unfold-buffer-definitions)
      (fold-buffer-definitions)))
