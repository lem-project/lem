(defpackage :lem-lisp-mode/defpackage-editor
  (:use :cl :lem))
(in-package :lem-lisp-mode/defpackage-editor)

(deftype package-name-case ()
  '(member :keyword :unintern))

(defparameter *package-name-case* :unintern)

(defun convert-to-symbol (name)
  (ecase *package-name-case*
    (:keyword
     (intern (string-upcase name) :keyword))
    (:unintern
     (make-symbol (string-upcase name)))))

(defun current-buffer-package (buffer)
  (or (lem-lisp-mode/package-inferred-system::infer-package-name (buffer-filename buffer))
      (let ((project-root-directory
              (uiop:pathname-parent-directory-pathname
               (lem-core/commands/project:find-root (buffer-directory buffer)))))
        (ppcre:regex-replace
         "\\.lisp$"
         (enough-namestring (buffer-filename buffer) project-root-directory)
         ""))))

(define-command lisp-defpackage-editor/insert-defpackage () ()
  (lem-lisp-mode/package-inferred-system::insert-defpackage-and-in-package
   (current-point)
   (convert-to-symbol (current-buffer-package (current-buffer)))))

(define-command lisp-defpackage-editor/add-import-from () ()
  (prompt-for-string "Symbol: "
                     :completion-function 'symbol-completion))

(defun symbol-completion (string)
  (loop :for item :in (lem-lisp-mode:lisp-eval `(micros:apropos-list-for-emacs ,string))
        :collect (destructuring-bind (&key designator package-name &allow-other-keys) item
                   (lem/completion-mode:make-completion-item
                    :label designator
                    :detail package-name))))
