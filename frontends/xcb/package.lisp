;;;; package.lisp

(defpackage #:xcb
  (:use #:cffi #:cl)
  (:export #:*font-path-normal*
	   #:*font-path-bold*) )

(defpackage #:lem-xcb
  (:use #:xcb #:cffi #:cl)
  (:export))





