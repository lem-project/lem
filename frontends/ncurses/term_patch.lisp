;; workaround for windows pdcurses

(in-package :lem.term)

(export '(*pos-adjust-mode* get-mouse-mode enable-mouse disable-mouse))

;; position adjustment mode
;;   =0: no conversion
;;   =1: wide character adjustment
;;       (for ConEmu utf-8 mode (chcp 65001) (incomplete (unstable))
;;   =2: wide character adjustment except cursor
;;       (for mintty)
(defvar *pos-adjust-mode*
  (cond 
    #+sbcl
    ((sb-unix::posix-getenv "ConEmuBuild") 1)
    (t 2)))

;; mouse mode
;;   =0: not use mouse
;;   =1: use mouse
(defvar *mouse-mode* 1)

;; for mouse
(defun get-mouse-mode ()
  *mouse-mode*)
(defun enable-mouse ()
  (setf *mouse-mode* 1)
  (charms/ll:mousemask (logior charms/ll:all_mouse_events
                               charms/ll:report_mouse_position)))
(defun disable-mouse ()
  (setf *mouse-mode* 0)
  (charms/ll:mousemask 0))

;; workaround for windows pdcurses
(defun term-init ()
  #+(or (and ccl unix) (and lispworks unix))
  (lem-setlocale/cffi:setlocale lem-setlocale/cffi:+lc-all+ "")
  (if *tty-name*
      (term-init-tty *tty-name*)
      (charms/ll:initscr))
  (when (zerop (charms/ll:has-colors))
    (charms/ll:endwin)
    (write-line "Please execute TERM=xterm-256color and try again.")
    (return-from term-init nil))
  (charms/ll:start-color)

  ;; enable default color code (-1)
  (charms/ll:use-default-colors)

  (init-colors charms/ll:*colors*)
  (set-default-color nil nil)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (charms/ll:raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (setf charms/ll::*escdelay* 0)
					;(charms/ll:curs-set 0)

  ;; for mouse
  (when (= *mouse-mode* 1)
    (enable-mouse))

  t)
