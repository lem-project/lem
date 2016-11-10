(in-package :cl-user)
(defpackage :lem.term
  (:use :cl)
  (:export
   :get-color-pair
   :with-allow-interrupt
   :term-init
   :term-finallize
   :term-set-tty))
(in-package :lem.term)

(defvar *colors* nil)
(defvar *color-pair-table* (make-hash-table :test 'equal))

(defstruct color
  number
  name
  r
  g
  b)

(defun get-color (string)
  (let ((string (string-trim " " string)))
    (cond ((zerop (length string))
           nil)
          ((and (char= #\# (aref string 0))
                (= 6 (length string)))
           (let ((r (parse-integer string :start 0 :end 2 :radix 16 :junk-allowed t))
                 (g (parse-integer string :start 2 :end 4 :radix 16 :junk-allowed t))
                 (b (parse-integer string :start 4 :end 6 :radix 16 :junk-allowed t)))
             (if (not (and r g b))
                 nil
                 (let (found-color
                       (min most-positive-fixnum))
                   (dolist (color *colors*)
                     (let ((dr (- (color-r color) r))
                           (dg (- (color-g color) g))
                           (db (- (color-b color) b)))
                       (let ((dist (+ (* dr dr) (* dg dg) (* db db))))
                         (when (< dist min)
                           (setf min dist)
                           (setf found-color color)))))
                   (assert (not (null found-color)))
                   found-color))))
          (t
           (dolist (color *colors* nil)
             (when (string= string (color-name color))
               (return (color-number color))))))))

(defun get-color-pair (fg-color-name bg-color-name)
  (let ((fg-color (if (null fg-color-name) -1 (get-color fg-color-name)))
        (bg-color (if (null bg-color-name) -1 (get-color bg-color-name))))
    (if (and fg-color bg-color)
        (gethash (cons fg-color bg-color)
                 *color-pair-table*
                 0)
        0)))

(defun init-colors ()
  (when (/= 0 (charms/ll:has-colors))
    (charms/ll:start-color)
    (charms/ll:use-default-colors)

    (setf *colors* nil)
    (flet ((add-color (color-number color-name r g b &optional builtin)
                      (unless builtin
                        (charms/ll:init-color color-number r g b))
                      (push (make-color :number color-number
                                        :name color-name
                                        :r r
                                        :g g
                                        :b b)
                            *colors*)))
      (add-color charms/ll:color_black "black" #x00 #x00 #x00 t)
      (add-color charms/ll:color_red "red" #xcd #x00 #x00 t)
      (add-color charms/ll:color_green "green" #x00 #xcd #x00 t)
      (add-color charms/ll:color_yellow "yellow" #xcd #xcd #x00 t)
      (add-color charms/ll:color_blue "blue" #x00 #x00 #xee t)
      (add-color charms/ll:color_magenta "magenta" #xcd #x00 #xcd t)
      (add-color charms/ll:color_cyan "cyan" #x00 #xcd #xcd t)
      (add-color charms/ll:color_white "white" #xe5 #xe5 #xe5 t))
    (setf *colors* (nreverse *colors*))

    (clrhash *color-pair-table*)
    (let ((pair 0))
      (flet ((add-pair (fg-color-number bg-color-number)
                       (incf pair)
                       (charms/ll:init-pair pair
                                            fg-color-number
                                            bg-color-number)
                       (setf (gethash (cons fg-color-number bg-color-number)
                                      *color-pair-table*)
                             (charms/ll:color-pair pair))))
        (dolist (fg-color *colors*)
          (dolist (bg-color *colors*)
            (add-pair (color-number fg-color)
                      (color-number bg-color)))
          (add-pair (color-number fg-color) -1)
          (add-pair -1 (color-number fg-color)))))
    t))

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

(defvar *tty-name* nil)
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

(defun term-init ()
  (if *tty-name*
      (term-init-tty *tty-name*)
      (charms/ll:initscr))
  (init-colors)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1))

(defun term-set-tty (tty-name)
  (setf *tty-name* tty-name))

(defun term-finallize ()
  (when *term-io*
    (fclose *term-io*)
    (setf *term-io* nil))
  (charms/ll:endwin)
  (charms/ll:delscreen charms/ll:*stdscr*))
