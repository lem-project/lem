(defpackage :lem-lsp-mode/markdown
  (:use :cl)
  (:import-from :3bmd))
(in-package :lem-lsp-mode/markdown)

(defun parse (file)
  (let ((3bmd-tables:*tables* t)
        (3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (out)
      (3bmd:parse-and-print-to-stream file out))))
