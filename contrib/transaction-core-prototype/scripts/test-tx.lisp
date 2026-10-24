;;;; SPDX-License-Identifier: MIT
(require :asdf)
(let ((root (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:load-asd (merge-pathnames "yeditor-prototype-context.asd" root))
  (asdf:load-asd (merge-pathnames "yeditor-prototype-tx.asd" root)))
(asdf:test-system "yeditor-prototype-tx")
