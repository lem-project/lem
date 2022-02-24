(defpackage :lem
  (:use :cl :lem-base :lem-utils/class)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym))))
  #+sbcl
  (:lock t))

(defpackage :lem-interface
  (:nicknames :lem-if)
  (:use)
  (:export
   :invoke
   :display-background-mode
   :update-foreground
   :update-background
   :display-width
   :display-height
   :make-view
   :delete-view
   :clear
   :set-view-size
   :set-view-pos
   :print
   :print-modeline
   :clear-eol
   :clear-eob
   :redraw-view-after
   :update-display
   :scroll
   :set-first-view
   :split-window-horizontally
   :split-window-vertically
   :display-popup-menu
   :popup-menu-update
   :popup-menu-quit
   :popup-menu-down
   :popup-menu-up
   :popup-menu-first
   :popup-menu-last
   :popup-menu-select
   :display-popup-message
   :display-menu
   :update-menu
   :delete-popup-message
   :clipboard-paste
   :clipboard-copy))

(defpackage :lem-user
  (:use :cl :lem))

(defpackage :lem-restart
  (:use)
  (:export :message
           :call-function))
