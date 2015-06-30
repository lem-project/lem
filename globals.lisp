(in-package :lem)

(defvar *program-name* "Lem")

(defvar *window-list*)
(defvar *current-window*)
(defvar *prev-buffer*)
(defvar *buffer-list* nil)

(defvar *tab-size* 8)

(defvar *last-flags* nil)
(defvar *curr-flags* nil)

(defvar *universal-argument* nil)

(defstruct flags
  kill)
