;; workaround for windows pdcurses

(in-package :cl-charms/low-level)

(export '(getch
          color-pair
          resizeterm
          *escdelay*
          *pdcurses-version-number*
          *pdcurses-version-info*
          PDC-save-key-modifiers))

;; add missing definitions
(defun getch () (wgetch *stdscr*))
(defun color-pair (n) (logand (ash n 24) #xff000000))
(setf (fdefinition 'resizeterm) #'resize-term)
(defvar *escdelay-dummy* 0)
(define-symbol-macro *escdelay* *escdelay-dummy*)

;; pdcurses version number
;;  (this is floating-point number (e.g. 3.9))
(defvar *pdcurses-version-number*
  (read-from-string (charms/ll::curses-version) t nil :start 9))

;; pdcurses version information
(cffi:defcstruct pdcversioninfo
  (flags :short)
  (build :short)
  (major :unsigned-char)
  (minor :unsigned-char)
  (csize :unsigned-char)
  (bsize :unsigned-char))
;(define-exported-cfuns ("PDC_get_version") :void (ver-info :pointer))
(defun PDC-get-version (ver-info)
  ;; PDC_get_version has been added in pdcurses version 3.8
  ;(if (cffi:foreign-symbol-pointer "PDC_get_version")
  (if (>= *pdcurses-version-number* 3.8)
      (cffi:foreign-funcall "PDC_get_version" :pointer ver-info :void)
      (progn
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'flags) 0)
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'build) 0)
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'major) 0)
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'minor) 0)
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'csize) 0)
        (setf (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'bsize) 0)
        nil)))
(defvar *pdcurses-version-info*
  (cffi:with-foreign-object (ver-info '(:struct pdcversioninfo))
    (PDC-get-version ver-info)
    (list (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'flags)
          (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'build)
          (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'major)
          (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'minor)
          (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'csize)
          (cffi:foreign-slot-value ver-info '(:struct pdcversioninfo) 'bsize))))

;; enable modifier keys
(define-exported-constant PDC_KEY_MODIFIER_SHIFT   1)
(define-exported-constant PDC_KEY_MODIFIER_CONTROL 2)
(define-exported-constant PDC_KEY_MODIFIER_ALT     4)
(define-exported-constant PDC_KEY_MODIFIER_NUMLOCK 8)
(define-exported-cfuns ("PDC_get_key_modifiers") :unsigned-long)
(define-exported-cfuns ("PDC_return_key_modifiers") :int (flag bool))
;(define-exported-cfuns ("PDC_save_key_modifiers") :int (flag bool))
(defun PDC-save-key-modifiers (flag)
  ;; PDC_save_key_modifiers has been removed in pdcurses version 3.9
  ;(if (cffi:foreign-symbol-pointer "PDC_save_key_modifiers")
  (if (< *pdcurses-version-number* 3.9)
      (cffi:foreign-funcall "PDC_save_key_modifiers" bool flag :int)
      0))

;; for extracting a character code
(define-exported-constant A_CHARTEXT #x0000ffff)

;; for mouse
(define-exported-constant BUTTON4_PRESSED #x00010000) ; wheel up
(define-exported-constant BUTTON5_PRESSED #x00200000) ; wheel down
(cffi:defcfun ("nc_getmouse" %getmouse) :int (event :pointer))
