(defpackage :lem-lisp-mode/misc-commands
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/misc-commands)

(defun find-symbol-matchies (symbol-name)
  (let ((symbols '()))
    (do-all-symbols (s)
      (when (and (string-equal s symbol-name) (fboundp s))
        (pushnew s symbols)))
    symbols))


(defun find-utopian-route (point)
  (when (in-string-p point)
    (with-point ((start point)
                 (end point))
      (maybe-beginning-of-string start)
      (move-point end start)
      (character-offset start 1)
      (form-offset end 1)
      (character-offset end -1)
      (let* ((route (points-to-string start end))
             (parts (uiop:split-string route :separator ":")))
        (when (= 2 (length parts))
          (destructuring-bind (path name)
              parts
            (let ((filename (expand-file-name
                             (format nil "../controllers/~A.lisp" path)
                             (buffer-directory (current-buffer)))))
              (unless (probe-file filename)
                (editor-error "~A does not exists" filename))
              (lem/language-mode:make-xref-location
               :filespec (probe-file filename)
               :position (let ((buffer (find-file-buffer filename
                                                         :temporary t
                                                         :enable-undo-p nil)))
                           (with-point ((point (buffer-point buffer)))
                             (buffer-start point)
                             (search-forward-regexp
                              point
                              `(:sequence
                                "(def"
                                (:greedy-repetition 1 nil (:CHAR-CLASS :WORD-CHAR-CLASS #\-))
                                (:greedy-repetition 1 nil :whitespace-char-class)
                                ,name
                                :whitespace-char-class))
                             (line-start point)
                             (position-at-point point)))))))))))

(pushnew 'find-utopian-route lem-lisp-mode/internal:*find-definitions*)


(define-command lisp-defstruct-to-defclass () ()
  (lem-lisp-syntax:defstruct-to-defclass (current-point)))


(defun move-to-deftest-toplevel-form (point)
  (or (looking-at point "\\(deftest\\s")
      (progn (lisp-beginning-of-defun point 1)
             (looking-at point "\\(deftest\\s"))))

(defun get-deftest-name (point)
  (with-point ((point point))
    (when (and (move-to-deftest-toplevel-form point)
               (scan-lists point 1 -1 t)
               (form-offset point 1)
               (skip-whitespace-forward point))
      (with-point ((start point)
                   (end point))
        (form-offset end 1)
        (points-to-string start end)))))

(defparameter *run-test-function-name* "rove:run-test")

(define-command lisp-run-test () ()
  (let* ((package-name (buffer-package (current-buffer)))
         (test-name (get-deftest-name (current-point)))
         (form-string (format nil "(~A '~A::~A)" *run-test-function-name* package-name test-name)))
    (start-lisp-repl)
    (buffer-end (current-point))
    (insert-string (current-point) form-string)
    (lem/listener-mode:listener-return)))
