(in-package :lem)

(export '(*window-list*
          *current-window*
          *buffer-list*
          *tab-size*))

(defvar *program-name* "Lem")

(defvar *window-list*)
(defvar *current-window*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *universal-argument* nil)

(defstruct flags
  kill
  undo
  abbrev)
