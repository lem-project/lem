;; -*- mode:lisp; package:lem.term -*-

(in-package :lem.term)

(defvar *color-name-table* (make-hash-table :test 'equal))

(defun get-color (color)
  (if (null color)
      0
      (gethash color *color-name-table* 0)))

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

;;;


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
;;;


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

;;;

(cffi:defcfun "fopen" :pointer (path :string) (mode :string))
(cffi:defcfun "fclose" :int (fp :pointer))
(cffi:defcfun "fileno" :int (fd :pointer))

(cffi:defcstruct winsize
  (ws-row :unsigned-short)
  (ws-col :unsigned-short)
  (ws-xpixel :unsigned-short)
  (ws-ypixel :unsigned-short))

(cffi:defcfun ioctl :int
  (fd :int)
  (cmd :int)
  &rest)

(defvar *term-io* nil)

(defun resize-term ()
  (when *term-io*
    (cffi:with-foreign-object (ws '(:struct winsize))
      (when (= 0 (ioctl (fileno *term-io*) 21523 :pointer ws))
        (cffi:with-foreign-slots ((ws-row ws-col) ws (:struct winsize))
          (charms/ll:resizeterm ws-row ws-col))))))

(defun term-init-tty (tty-name)
  (let* ((io (fopen tty-name "r+")))
    (setf *term-io* io)
    (cffi:with-foreign-string (term "xterm")
      (charms/ll:newterm term io io))))

(defun term-init (&optional tty-name)
  (if tty-name
      (term-init-tty tty-name)
      (charms/ll:initscr))
  (init-colors)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1))

(defun term-finallize ()
  (when *term-io*
    (fclose *term-io*)
    (setf *term-io* nil))
  (charms/ll:endwin)
  (charms/ll:delscreen charms/ll:*stdscr*))

(defun term-resize ()
  )

(defun term-get-char-1 ()
  (loop :for code := (charms/ll:getch) :do
    (if (= code 410)
        (term-resize)
        (return code))))

(defun term-get-char (timeout)
  (charms/ll:doupdate)
  (etypecase timeout
    (integer
     (charms/ll:timeout timeout)
     (prog1 (let ((c (term-get-char-1)))
              (if (= -1 c) nil c))
       (charms/ll:timeout -1)))
    (null
     (term-get-char-1))))
