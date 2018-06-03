(defpackage :lem
  (:use :cl :lem-base)
  (:export . #.(loop :for sym :being :the :external-symbols :of (find-package :lem-base)
                     :collect (make-symbol (string sym)))))

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
   :redraw-window
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
   :display-popup-message))

(defpackage :lem-user
  (:use :cl :lem))
