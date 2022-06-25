(cl-lsp/defpackage:defpackage :cl-lsp/swank
  (:use :cl)
  (:export :swank-init
           :fuzzy-completions
           :describe-symbol
           :xrefs
           :operator-arglist
           :find-definitions
           :swank-apropos-list
           :swank-compile-file))
(in-package :cl-lsp/swank)

(defvar *fuzzy-completions* nil)

(defmacro with-swank ((&key (package (find-package "CL-USER"))
                            (readtable '*readtable*))
                      &body body)
  `(let ((swank::*buffer-package* ,package)
         (swank::*buffer-readtable* ,readtable))
     ,@body))

(defun swank-init ()
  (swank:swank-require '("SWANK-TRACE-DIALOG"
                         "SWANK-PACKAGE-FU"
                         "SWANK-PRESENTATIONS"
                         "SWANK-FUZZY"
                         "SWANK-FANCY-INSPECTOR"
                         "SWANK-C-P-C"
                         "SWANK-ARGLISTS"
                         "SWANK-REPL"))
  (setf *fuzzy-completions* (intern "FUZZY-COMPLETIONS" :SWANK))
  (values))

(defun fuzzy-completions (string package)
  (with-swank ()
    (funcall *fuzzy-completions*
             string package)))

(defun describe-symbol (symbol-name package)
  (ignore-errors
   (with-swank (:package package)
     (swank:describe-symbol symbol-name))))

(defun xrefs (name package)
  (with-swank (:package package)
    (swank:xrefs '(:calls :macroexpands :binds :references :sets :specializes)
                 name)))

(defun operator-arglist (symbol-string package)
  (swank:operator-arglist symbol-string package))

(defun find-definitions (name package)
  (multiple-value-bind (symbol found)
      (with-swank (:package package)
        (swank::find-definitions-find-symbol-or-package name))
    (when found
      (ignore-errors (swank::find-definitions symbol)))))

(defun swank-apropos-list (name package)
  (with-swank (:package package)
    (mapcar #'cadr (swank:apropos-list-for-emacs name))))

(defun swank-compile-file (filename loadp)
  (with-swank ()
    (swank:compile-file-for-emacs filename loadp)))
