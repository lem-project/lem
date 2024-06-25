(defpackage :lem-terminal/ffi
  (:use :cl))
(in-package :lem-terminal/ffi)

(pushnew (asdf:system-source-directory :lem-terminal)
         cffi:*foreign-library-directories*
         :test #'uiop:pathname-equal)

(cffi:define-foreign-library terminal
  (:unix "terminal.so"))

(cffi:use-foreign-library terminal)

(defconstant VTERM_KEY_NONE 0)
(defconstant VTERM_KEY_ENTER 1)
(defconstant VTERM_KEY_TAB 2)
(defconstant VTERM_KEY_BACKSPACE 3)
(defconstant VTERM_KEY_ESCAPE 4)
(defconstant VTERM_KEY_UP 5)
(defconstant VTERM_KEY_DOWN 6)
(defconstant VTERM_KEY_LEFT 7)
(defconstant VTERM_KEY_RIGHT 8)
(defconstant VTERM_KEY_INS 9)
(defconstant VTERM_KEY_DEL 10)
(defconstant VTERM_KEY_HOME 11)
(defconstant VTERM_KEY_END 12)
(defconstant VTERM_KEY_PAGEUP 13)
(defconstant VTERM_KEY_PAGEDOWN 14)
(defconstant VTERM_KEY_FUNCTION_0 256)
(defconstant VTERM_KEY_FUNCTION_MAX 511)
(defconstant VTERM_KEY_KP_0 512)
(defconstant VTERM_KEY_KP_1 513)
(defconstant VTERM_KEY_KP_2 514)
(defconstant VTERM_KEY_KP_3 515)
(defconstant VTERM_KEY_KP_4 516)
(defconstant VTERM_KEY_KP_5 517)
(defconstant VTERM_KEY_KP_6 518)
(defconstant VTERM_KEY_KP_7 519)
(defconstant VTERM_KEY_KP_8 520)
(defconstant VTERM_KEY_KP_9 521)
(defconstant VTERM_KEY_KP_MULT 522)
(defconstant VTERM_KEY_KP_PLUS 523)
(defconstant VTERM_KEY_KP_COMMA 524)
(defconstant VTERM_KEY_KP_MINUS 525)
(defconstant VTERM_KEY_KP_PERIOD 526)
(defconstant VTERM_KEY_KP_DIVIDE 527)
(defconstant VTERM_KEY_KP_ENTER 528)
(defconstant VTERM_KEY_KP_EQUAL 529)
(defconstant VTERM_KEY_MAX 530)
(defconstant VTERM_N_KEYS 530)

(cffi:defcfun ("terminal_new" terminal-new) :pointer
  (rows :int)
  (cols :int))

(cffi:defcfun ("terminal_delete" terminal-delete) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_input_char" terminal-input-char) :void
  (terminal :pointer)
  (c :uint32)
  (mod :int))

(cffi:defcfun ("terminal_input_key" terminal-input-key) :void
  (terminal :pointer)
  (key :int)
  (mod :int))

(cffi:defcfun ("terminal_process_input_nonblock" terminal-process-input-nonblock) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_process_input" terminal-process-input) :void
  (terminal :pointer))

(cffi:defcfun ("terminal_query_cell" terminal-query-cell) :pointer
  (terminal :pointer)
  (x :int)
  (y :int))

(cffi:defcfun ("terminal_last_cell_chars" terminal-last-cell-chars) :pointer
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_width" terminal-last-cell-width) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_bold" terminal-last-cell-attrs-bold) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_underline" terminal-last-cell-attrs-underline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_italic" terminal-last-cell-attrs-italic) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_blink" terminal-last-cell-attrs-blink) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_reverse" terminal-last-cell-attrs-reverse) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_conceal" terminal-last-cell-attrs-conceal) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_strike" terminal-last-cell-attrs-strike) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_font" terminal-last-cell-attrs-font) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dwl" terminal-last-cell-attrs-dwl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_dhl" terminal-last-cell-attrs-dhl) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_small" terminal-last-cell-attrs-small) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_attrs_baseline" terminal-last-cell-attrs-baseline) :unsigned-int
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_red" terminal-last-cell-fg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_green" terminal-last-cell-fg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_fg_blue" terminal-last-cell-fg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_red" terminal-last-cell-bg-red) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_green" terminal-last-cell-bg-green) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_last_cell_bg_blue" terminal-last-cell-bg-blue) :uint8
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_row" terminal-cursor-row) :int
  (terminal :pointer))

(cffi:defcfun ("terminal_cursor_col" terminal-cursor-col) :int
  (terminal :pointer))
