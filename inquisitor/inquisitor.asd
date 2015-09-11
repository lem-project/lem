#|
  This file is a part of inquisitor project.
  Copyright (c) 2015 gray (shinichi.tanaka45@gmail.com)
|#

#|
  Encoding/end-of-line detecter and wrapper of external-format for Common Lisp

  Author: gray (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage inquisitor-asd
  (:use :cl :asdf))
(in-package :inquisitor-asd)

(defsystem inquisitor
  :version "0.5"
  :author "gray"
  :license "MIT"
  :depends-on (:alexandria
               :anaphora
               :metabang-bind)
  :components ((:module "src"
                :components
                ((:module "encoding"
                  :components
                  ((:file "keyword")
                   (:file "dfa" :depends-on ("keyword"))
                   (:file "table" :depends-on ("dfa"))
                   (:file "guess" :depends-on ("dfa" "table"))))
                 (:file "eol")
                 (:file "keyword" :depends-on ("encoding" "eol"))
                 (:file "util")
                 (:file "inquisitor" :depends-on ("util")))))
  :description "Encoding/end-of-line detecter and of external-format wrapper for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op inquisitor-test))))
