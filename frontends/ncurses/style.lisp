(uiop:define-package :lem-ncurses/style
  (:use :cl
        :lem)
  (:export :border-horizontal
           :border-vertical
           :border-down-and-right
           :border-down-and-left
           :border-up-and-left
           :border-up-and-right
           :border-vertical-and-right
           :border-vertical-and-left
           :border-vertical-and-horizontal
           :border-up-and-horizontal
           :border-down-and-horizontal
           :border-attribute))
(in-package :lem-ncurses/style)

(define-attribute space-border-color
  (:light :foreground "gray" :reverse t)
  (:dark :foreground "#666666" :reverse t))

(define-attribute ruled-border-color)

(defstruct style
  border-shapes
  border-attribute)

(defstruct box-shapes
  (horizontal "─")
  (vertical "│")
  (down-and-right "╭")
  (down-and-left "╮")
  (up-and-left "╯")
  (up-and-right "╰")
  (vertical-and-right "├")
  (vertical-and-left "┤")
  (vertical-and-horizontal "┼")
  (up-and-horizontal "┴")
  (down-and-horizontal "┬"))

(defparameter *style*
  (make-style :border-shapes (make-box-shapes)
              :border-attribute 'ruled-border-color))

(defun border-horizontal () (box-shapes-horizontal (style-border-shapes *style*)))
(defun border-vertical () (box-shapes-vertical (style-border-shapes *style*)))
(defun border-down-and-right () (box-shapes-down-and-right (style-border-shapes *style*)))
(defun border-down-and-left () (box-shapes-down-and-left (style-border-shapes *style*)))
(defun border-up-and-left () (box-shapes-up-and-left (style-border-shapes *style*)))
(defun border-up-and-right () (box-shapes-up-and-right (style-border-shapes *style*)))
(defun border-vertical-and-right () (box-shapes-vertical-and-right (style-border-shapes *style*)))
(defun border-vertical-and-left () (box-shapes-vertical-and-left (style-border-shapes *style*)))
(defun border-vertical-and-horizontal () (box-shapes-vertical-and-horizontal (style-border-shapes *style*)))
(defun border-up-and-horizontal () (box-shapes-up-and-horizontal (style-border-shapes *style*)))
(defun border-down-and-horizontal () (box-shapes-down-and-horizontal (style-border-shapes *style*)))
(defun border-attribute () (style-border-attribute *style*))
