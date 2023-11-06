(defpackage #:lem-vi-mode/lisp-setup
  (:use #:cl #:lem))
(in-package #:lem-vi-mode/lisp-setup)

(add-hook lem-vi-mode/core:*enable-hook*
          (lambda ()
            (add-hook lem-lisp-mode:*lisp-mode-hook*
                      (lambda ()
                        (setf (lem-vi-mode:option-value "iskeyword")
                              (set-difference (lem-vi-mode:option-value "iskeyword")
                                              '("-" "/" "." ":")
                                              :test #'string=))
                        (setf (lem-vi-mode:option-value "isseparator")
                              (union (lem-vi-mode:option-value "isseparator")
                                     '("(" ")" "\"")
                                     :test #'string=))))))