;; -*- mode:lisp; package:lem-internal -*-

(in-package :lem)

(export '(make-attribute))

(defvar *color-name-table* (make-hash-table :test 'equal))

(defun get-color (color)
  (if (null color)
      0
      (or (gethash color *color-name-table*)
          (editor-error "Unknown color name: ~A" color))))

(defun init-colors ()
  (when (/= 0 (charms/ll:has-colors))
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
    t))

(defstruct (attribute (:constructor %make-attribute))
  color
  reverse-p
  bold-p
  underline-p)

(defun make-attribute (color &key reverse-p bold-p underline-p)
  (%make-attribute :color color
                   :reverse-p reverse-p
                   :bold-p bold-p
                   :underline-p underline-p))

(defun attribute-to-bits (attribute)
  (logior (get-color (attribute-color attribute))
          (if (attribute-reverse-p attribute)
              charms/ll:a_reverse
              0)
          (if (attribute-bold-p attribute)
              charms/ll:a_bold
              0)
          (if (attribute-underline-p attribute)
              charms/ll:a_underline
              0)))

(let ((raw-mode))
  (defun raw-p ()
    raw-mode)
  (defun raw ()
    (setq raw-mode t)
    (charms/ll:raw))
  (defun noraw ()
    (setq raw-mode nil)
    (charms/ll:noraw)))

(defmacro with-raw (raw-p &body body)
  (let ((g-old-raw (gensym))
        (g-new-raw (gensym)))
    `(let ((,g-old-raw (raw-p))
           (,g-new-raw ,raw-p))
       (if ,g-new-raw
           (raw)
           (noraw))
       (unwind-protect (progn ,@body)
         (if ,g-old-raw
             (raw)
             (noraw))))))

(defmacro with-allow-interrupt (flag &body body)
  `(with-raw (not ,flag) ,@body))

(defun term-init (&optional fd)
  (if fd
      nil
      (charms/ll:initscr))
  (init-colors)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1))
