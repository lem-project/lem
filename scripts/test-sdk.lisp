;;;; SPDX-License-Identifier: MIT
(require :asdf)
(let ((root (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))
  (dolist (name '("yeditor-tx" "yeditor-context" "yeditor-sdk"))
    (asdf:load-asd (merge-pathnames (concatenate 'string name ".asd") root))))
(asdf:test-system "yeditor-sdk")
