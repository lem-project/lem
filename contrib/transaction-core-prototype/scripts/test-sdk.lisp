;;;; SPDX-License-Identifier: MIT
(require :asdf)
(let ((root (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))
  (dolist (name '("yeditor-prototype-tx" "yeditor-prototype-context" "yeditor-prototype-sdk"))
    (asdf:load-asd (merge-pathnames (concatenate 'string name ".asd") root))))
(asdf:test-system "yeditor-prototype-sdk")
