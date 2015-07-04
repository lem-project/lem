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

(defvar *continue-command-flags* (list :kill :undo :abbrev))

(defun make-flags ()
  (mapcar (lambda (sym)
            (cons sym nil))
          *continue-command-flags*))
