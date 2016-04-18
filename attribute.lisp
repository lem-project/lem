;; -*- mode:lisp; package:lem -*-

(in-package :lem)

(export '(set-attr
          get-attr
          make-attr))

(defvar *attribute-name-table* (make-hash-table))

(defun get-attr (name)
  (check-type name symbol)
  (multiple-value-bind (value found)
      (gethash name *attribute-name-table*)
    (assert found)
    value))

(defun set-attr (name attr)
  (unless (integerp attr)
    (setq attr (get-attr name)))
  (check-type attr integer)
  (setf (gethash name *attribute-name-table*) attr))

(defvar *color-name-table* (make-hash-table :test 'equal))
(defvar *color-initialized-p* nil)
(defvar *init-color-hooks* nil)

(defun get-color (color)
  (if (null color)
      0
      (or (gethash color *color-name-table*)
          (editor-error "Unknown color name: ~A" color))))

(defun init-colors ()
  (when (and (/= 0 (charms/ll:has-colors))
             (not *color-initialized-p*))
    (charms/ll:start-color)
    (charms/ll:use-default-colors)
    (let ((n 0))
      (flet ((add-color (name color)
               (incf n)
               (charms/ll:init-pair n color -1)
               (setf (gethash name *color-name-table*) (charms/ll:color-pair n))))
        (add-color "yellow" charms/ll:color_yellow)
        (add-color "green" charms/ll:color_green)
        (add-color "blue" charms/ll:color_blue)
        (add-color "magenta" charms/ll:color_magenta)
        (add-color "red" charms/ll:color_red)
        (add-color "cyan" charms/ll:color_cyan)
        (add-color "white" charms/ll:color_white)
        (add-color "black" charms/ll:color_black)))
    (setf *color-initialized-p* t)
    (mapc #'funcall *init-color-hooks*)
    (setf *init-color-hooks* nil)
    t))

(defmacro define-attribute (name &key color reverse-p bold-p underline-p)
  (check-type name symbol)
  (let ((make-attr-form
          `(make-attr :color ,color
                      :reverse-p ,reverse-p
                      :bold-p ,bold-p
                      :underline-p ,underline-p)))
    `(if *color-initialized-p*
         (set-attr ',name ,make-attr-form)
         (push (lambda ()
                 (set-attr ',name ,make-attr-form))
               *init-color-hooks*))))

(defun make-attr (&key color reverse-p bold-p underline-p)
  (logior (get-color color)
          (if reverse-p
              charms/ll:a_reverse
              0)
          (if bold-p
              charms/ll:a_bold
              0)
          (if underline-p
              charms/ll:a_underline
              0)))
