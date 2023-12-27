(defpackage :lem-ncurses/view
  (:use :cl)
  (:export :make-ncurses-view
           :ncurses-view-window
           :ncurses-view-border
           :ncurses-view-scrwin
           :ncurses-view-modeline-scrwin
           :ncurses-view-x
           :ncurses-view-y
           :ncurses-view-width
           :ncurses-view-height))
(in-package :lem-ncurses/view)

(defstruct ncurses-view
  window
  border
  scrwin
  modeline-scrwin
  x
  y
  width
  height)
