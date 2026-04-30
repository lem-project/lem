(defpackage :lem-tests/interface
  (:use :cl :rove :lem))
(in-package :lem-tests/interface)

(deftest set-frame-color-default-is-noop
  (lem-fake-interface:with-fake-interface ()
    (ok (null (set-frame-color :dark))
        "set-frame-color returns NIL on frontends that don't implement it")
    (ok (null (set-frame-color :light))
        "set-frame-color returns NIL for :light as well")))
