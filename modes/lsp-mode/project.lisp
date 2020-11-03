(defpackage :lem-lsp-mode/project
  (:use :cl)
  (:export :local-nickname))
(in-package :lem-lsp-mode/project)

(defmacro local-nickname (local-nickname actual-package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (trivial-package-local-nicknames:add-package-local-nickname ,local-nickname ,actual-package)))
