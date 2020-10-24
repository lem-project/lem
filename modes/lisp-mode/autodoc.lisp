(defpackage :lem-lisp-mode.autodoc
  (:use :cl :lem :lem-lisp-mode))
(in-package :lem-lisp-mode.autodoc)

(define-key *lisp-mode-keymap* "C-c C-d C-a" 'lisp-autodoc)
(define-key *lisp-mode-keymap* "M-a" 'lisp-autodoc)
(define-key *lisp-mode-keymap* "Space" 'lisp-insert-space-and-autodoc)

(let ((autodoc-symbol nil))
  (defun autodoc-symbol ()
    (or autodoc-symbol
        (setf autodoc-symbol (intern "AUTODOC" :swank)))))

(defun autodoc (function)
  (let ((context (lem-lisp-syntax:parse-for-swank-autodoc (current-point))))
    (lisp-eval-async
     `(,(autodoc-symbol) ',context)
     (lambda (doc)
       (trivia:match doc
         ((list doc _)
          (unless (eq doc :not-available)
            (let* ((buffer (make-buffer "*swank:autodoc-fontity*"
                                        :temporary t :enable-undo-p nil))
                   (point (buffer-point buffer)))
              (erase-buffer buffer)
              (change-buffer-mode buffer 'lisp-mode)
              (insert-string point doc)
              (buffer-start point)
              (multiple-value-bind (result string)
                  (search-forward-regexp point "(?====> (.*) <===)")
                (when result
                  (with-point ((start point))
                    (character-offset point 5)
                    (search-forward point "<===")
                    (delete-between-points start point)
                    (insert-string point string :attribute 'region))))
              (buffer-start (buffer-point buffer))
              (setf (variable-value 'truncate-lines :buffer buffer) nil)
              (funcall function buffer)))))))))

(define-command lisp-autodoc () ()
  (autodoc #'message-buffer))

(define-command lisp-insert-space-and-autodoc (n) ("p")
  (loop :repeat n :do (insert-character (current-point) #\space))
  (lisp-autodoc))
