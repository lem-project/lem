;; Tabbar configuration stub.
;;
;; The tabbar implementation lives in frontends/server/tabbar.lisp and is only
;; loaded by frontends that depend on lem-server (webview, server). Defining
;; the :lem/tabbar package + *enable-tabbar-on-startup* here in lem/core lets
;; users portably write
;;
;;   (setf lem/tabbar:*enable-tabbar-on-startup* nil)
;;
;; in their init file without triggering a read-time
;; "Package LEM/TABBAR does not exist" error on frontends (e.g. ncurses) that
;; don't load the tabbar implementation. On those frontends the variable is
;; just an inert defvar.

(uiop:define-package :lem/tabbar
  (:use :cl)
  (:export :*enable-tabbar-on-startup*))

(in-package :lem/tabbar)

(defvar *enable-tabbar-on-startup* t
  "When non-NIL (default), the tabbar is enabled automatically after init in
frontends that support it (currently webview and server). Set this in your
init file to opt out:

  (setf lem/tabbar:*enable-tabbar-on-startup* nil)

Setting it on a frontend without tabbar support (e.g. ncurses) is a no-op.")
