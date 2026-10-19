;;;; SPDX-License-Identifier: MIT
(require :asdf)
(let ((root (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:load-asd (merge-pathnames "yeditor-context.asd" root))
  (asdf:load-asd (merge-pathnames "yeditor-tx.asd" root)))
(asdf:test-system "yeditor-tx")
