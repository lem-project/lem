(defpackage :lem-lisp-mode/misc-commands
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/misc-commands)

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


(defparameter *run-test-function-name* "rove:run-test")
(defparameter *run-suite-test-function-name* "rove:run-suite")

(defun %send-test-reference (package test)
  (lem-lisp-mode/internal::lisp-switch-to-repl-buffer)
  (buffer-end (current-point))
  (insert-string
   (current-point)
   (format nil "(~A '~A::~A)"
           *run-test-function-name*
           (remove #\#
                   (remove #\: (lem/detective:reference-name package)))
           (lem/detective:reference-name test)))
  (lem/listener-mode:listener-return))

(defun %send-test-suite (suite-package)
  (lem-lisp-mode/internal::lisp-switch-to-repl-buffer)
  (buffer-end (current-point))
  (insert-string
   (current-point)
   (format nil "(~A ~A)"
           *run-suite-test-function-name*
           (lem/detective:reference-name suite-package)))
  (lem/listener-mode:listener-return))

(define-command lisp-test-run-buffer () ()
  (lem/detective::check-change :force t)
  (alexandria:when-let*
      ((buffer-references (lem/detective:buffer-references
                           (current-buffer)))
       (package (first (lem/detective:references-packages buffer-references)))
       (test-references
        (remove-if-not #'(lambda (x)
                           (string-equal "deftest" x))
                       (lem/detective:references-misc buffer-references)
                       :key #'lem/detective:misc-custom-type))
       (selected-test
        (lem/detective::%get-reference test-references
                                       :prompt "Test: ")))
    (%send-test-reference package selected-test)))

(define-command lisp-test-run-current () ()
  (lem/detective::check-change :force t)
  (let* ((buffer-references (lem/detective:buffer-references
                             (current-buffer)))
         (package (first (lem/detective:references-packages buffer-references)))
         (reference (lem/detective::current-reference)))
    ;;TODO: Make a regex for the test posiblities
    (if (and (typep reference 'lem/detective:misc-reference)
             (string-equal (lem/detective:misc-custom-type reference)
                           "deftest"))
        (%send-test-reference package reference)
        (message "Current reference is not a test."))))

(define-command lisp-test-run-suite () ()
  (lem/detective::check-change)
  (let* ((buffer-references (lem/detective:buffer-references
                             (current-buffer)))
         (package (first (lem/detective:references-packages buffer-references))))
    (%send-test-suite package)))
