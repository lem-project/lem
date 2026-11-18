(defpackage :lem-ncurses/term
  (:use :cl)
  (:export :get-color-pair
           :window-background
           :set-window-background
           :update-foreground-color
           :update-background-color
           :background-color
           :term-init
           :term-finalize
           :term-set-tty
           :term-set-color
           ;;win32 patch
           :get-mouse-mode
           :enable-mouse
           :disable-mouse
           :update-cursor-shape
           :get-display-width
           :get-display-height))
(in-package :lem-ncurses/term)

(cffi:defcvar ("COLOR_PAIRS" *COLOR-PAIRS* :library charms/ll::libcurses) :int)

;; mouse mode
;;   =0: not use mouse
;;   =1: use mouse
(defvar *mouse-mode* #+win32 1 #-win32 0)

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


;; Palette state for OSC-4 exact-color redefinition: the color table plus
;; the next free grid slot (16+) to hand out. The initial next-redefine-slot
;; of 256 intentionally disables allocation: it is only ever valid before
;; init-colors runs (which resets it to 16), and 256 is out of range for
;; any palette array.
(defstruct (palette (:conc-name palette-))
  (colors nil)
  (next-redefine-slot 256)
  ;; Number of leading COLORS entries holding real lem:color values.
  ;; init-colors fills the 8 scheme colors, the 8 brights when N >= 16,
  ;; the 240-slot 256-grid when N >= 256, and the 72-slot 88-grid
  ;; (rxvt-unicode / xterm-88color layout) when N = 88. For other
  ;; 17-255-color terminals the grid values are unknowable from
  ;; ncurses, so slots 16..N-1 stay raw: %exact-slot and the
  ;; quantization scan stop at FILLED.
  (filled 0)
  ;; Bit per palette slot: 1 = the slot has been handed out to a color
  ;; (by exact match, allocation, or quantization). A handed-out slot is
  ;; never redefined afterwards: cached index entries and already
  ;; allocated color pairs point at it, so changing its RGB would alter
  ;; the color of every cell that resolved to it.
  (used-slots nil))

(defvar *palette*)

;; --- direct color (truecolor) support ---
;; When the terminfo entry has the boolean "RGB" capability (xterm-direct,
;; Ghostty's shipped xterm-ghostty, ...), ncurses runs in direct-color
;; mode: a "color number" is the packed 24-bit RGB value itself, and pairs
;; are allocated with init_extended_pair (init_pair's short parameters
;; cannot hold packed RGB). Theme colors then render exactly, with no
;; palette slots, quantization, or OSC-4 redefinition involved.
(defvar *direct-color* nil)
;; The direct-color mode of the previous term-init (:unknown before the
;; first one). Color pair numbers are only reusable across inits within
;; the same mode: packed-RGB pair definitions and palette-slot pair
;; definitions are incompatible, so a mode switch renumbers pairs.
(defvar *last-direct-color* :unknown)
;; Theme background in direct-color mode (assume_default_colors cannot
;; carry packed RGB, so the default pair stays -1/-1 and per-cell pairs
;; carry the colors; this records the background for background-color
;; and for get-color-pair's nil-name substitution).
(defvar *direct-background* nil)
;; Theme foreground in direct-color mode. Like *direct-background*, it
;; is only recorded (the default pair stays -1/-1) and is used by
;; get-color-pair when a cell names no explicit foreground.
(defvar *direct-foreground* nil)
;; Memoize color-name -> palette-slot. The Lab quantization scan is 256
;; iterations of float math; without this cache it runs for every distinct
;; color name on every get-color-pair call (i.e. per rendered cell, twice).
;; Entries are only valid for the palette built by the current term-init;
;; init-colors clears this table.
(defvar *color-index-cache* (make-hash-table :test 'equal))
(defvar *pair-counter* 0)
(defvar *color-pair-table* (make-hash-table :test 'equal))

(defparameter *enable-palette-redefinition* t
  "Allow OSC-4 exact-color redefinition of palette slots 16+. Set to nil
on terminals that advertise \"ccc\" but silently ignore OSC 4: there the
exact-slot path records a match that the terminal still renders as the
256-color grid color, which can be much further off than the Lab nearest
match; nil disables redefinition and quantization is used instead.")

;; Cached can-change-color result. A constant property of the terminal;
;; queried once at init instead of on every color allocation (each cache
;; miss in %allocate-exact-color already pays two FFI calls).
(defvar *can-change-color* nil)

(defun term-set-color (index r g b &optional (call-init-color t))
  "Record RGB for palette slot INDEX (emitting init_color unless
CALL-INIT-COLOR is nil). The write is bounded by the palette array.
Entries at or beyond FILLED are recorded but remain invisible to
%exact-slot and quantization, which only scan known slots — extending
FILLED here would expose the uninitialized slots between as raw values
and crash the scans."
  (when *palette*
    (let ((colors (palette-colors *palette*)))
      (when (and (<= 0 index) (< index (length colors)))
        (when call-init-color
          (charms/ll:init-color index
                                (round (* r 1000/255))
                                (round (* g 1000/255))
                                (round (* b 1000/255))))
        (setf (aref colors index) (lem:make-color r g b))))))

(defun init-colors (n)
  "Build the palette for a terminal with N colors: fill PALETTE-COLORS
with the classic xterm 256-color grid (the first 16 entries are only
approximations — see %match-base), and set the OSC-4 redefinition
allocator to start at slot 16. No init_color call is made: the
terminal's own palette is left untouched so themes render in the colors
the user's terminal actually defines. Also clears *color-index-cache*,
whose entries are only valid for the palette built here."
  ;; limit max colors
  (if (> n 256) (setf n 256))
  ;; A constant property of the terminal; queried once instead of on
  ;; every color allocation.
  (setf *can-change-color* (charms/ll:can-change-color))
  ;; Cached color slots refer to the palette built here; a previous
  ;; term-init's entries (or a direct<->non-direct switch) would be stale.
  (clrhash *color-index-cache*)
  ;; Do not call init_color for any slot: the terminal's own palette (the
  ;; 16 scheme colors and the 256-color grid) is left untouched, so themes
  ;; render in the colors the user's terminal actually defines.
  (let ((counter 0))
    (declare (type fixnum counter))
    (flet ((add-color (r g b)
             ;; Classic-xterm approximation. For slots 0-15 this is NOT
             ;; the terminal's actual scheme: color_content cannot query
             ;; the terminal (see %match-base), so these entries are only
             ;; used for matching on palettes too small to redefine.
             ;; Bounded by the array: terminfo entries advertising fewer
             ;; than 8 colors must not overflow it either.
             (when (< counter n)
               (setf (aref (palette-colors *palette*) counter)
                     (lem:make-color r g b))
               (incf counter))))
      (setf *palette* (make-palette :colors (make-array n)
                                    :next-redefine-slot 16
                                    :used-slots (make-array n :element-type 'bit
                                                            :initial-element 0)))
      (add-color #x00 #x00 #x00)
      (add-color #xcd #x00 #x00)
      (add-color #x00 #xcd #x00)
      (add-color #xcd #xcd #x00)
      (add-color #x00 #x00 #xee)
      (add-color #xcd #x00 #xcd)
      (add-color #x00 #xcd #xcd)
      (add-color #xe5 #xe5 #xe5)
      (when (<= 16 n)
        (add-color #x7f #x7f #x7f)
        (add-color #xff #x00 #x00)
        (add-color #x00 #xff #x00)
        (add-color #xff #xff #x00)
        (add-color #x5c #x5c #xff)
        (add-color #xff #x00 #xff)
        (add-color #x00 #xff #xff)
        (add-color #xff #xff #xff))
      (when (<= 256 n)
        (add-color #x00 #x00 #x00)
        (add-color #x00 #x00 #x5f)
        (add-color #x00 #x00 #x87)
        (add-color #x00 #x00 #xaf)
        (add-color #x00 #x00 #xd7)
        (add-color #x00 #x00 #xff)
        (add-color #x00 #x5f #x00)
        (add-color #x00 #x5f #x5f)
        (add-color #x00 #x5f #x87)
        (add-color #x00 #x5f #xaf)
        (add-color #x00 #x5f #xd7)
        (add-color #x00 #x5f #xff)
        (add-color #x00 #x87 #x00)
        (add-color #x00 #x87 #x5f)
        (add-color #x00 #x87 #x87)
        (add-color #x00 #x87 #xaf)
        (add-color #x00 #x87 #xd7)
        (add-color #x00 #x87 #xff)
        (add-color #x00 #xaf #x00)
        (add-color #x00 #xaf #x5f)
        (add-color #x00 #xaf #x87)
        (add-color #x00 #xaf #xaf)
        (add-color #x00 #xaf #xd7)
        (add-color #x00 #xaf #xff)
        (add-color #x00 #xd7 #x00)
        (add-color #x00 #xd7 #x5f)
        (add-color #x00 #xd7 #x87)
        (add-color #x00 #xd7 #xaf)
        (add-color #x00 #xd7 #xd7)
        (add-color #x00 #xd7 #xff)
        (add-color #x00 #xff #x00)
        (add-color #x00 #xff #x5f)
        (add-color #x00 #xff #x87)
        (add-color #x00 #xff #xaf)
        (add-color #x00 #xff #xd7)
        (add-color #x00 #xff #xff)
        (add-color #x5f #x00 #x00)
        (add-color #x5f #x00 #x5f)
        (add-color #x5f #x00 #x87)
        (add-color #x5f #x00 #xaf)
        (add-color #x5f #x00 #xd7)
        (add-color #x5f #x00 #xff)
        (add-color #x5f #x5f #x00)
        (add-color #x5f #x5f #x5f)
        (add-color #x5f #x5f #x87)
        (add-color #x5f #x5f #xaf)
        (add-color #x5f #x5f #xd7)
        (add-color #x5f #x5f #xff)
        (add-color #x5f #x87 #x00)
        (add-color #x5f #x87 #x5f)
        (add-color #x5f #x87 #x87)
        (add-color #x5f #x87 #xaf)
        (add-color #x5f #x87 #xd7)
        (add-color #x5f #x87 #xff)
        (add-color #x5f #xaf #x00)
        (add-color #x5f #xaf #x5f)
        (add-color #x5f #xaf #x87)
        (add-color #x5f #xaf #xaf)
        (add-color #x5f #xaf #xd7)
        (add-color #x5f #xaf #xff)
        (add-color #x5f #xd7 #x00)
        (add-color #x5f #xd7 #x5f)
        (add-color #x5f #xd7 #x87)
        (add-color #x5f #xd7 #xaf)
        (add-color #x5f #xd7 #xd7)
        (add-color #x5f #xd7 #xff)
        (add-color #x5f #xff #x00)
        (add-color #x5f #xff #x5f)
        (add-color #x5f #xff #x87)
        (add-color #x5f #xff #xaf)
        (add-color #x5f #xff #xd7)
        (add-color #x5f #xff #xff)
        (add-color #x87 #x00 #x00)
        (add-color #x87 #x00 #x5f)
        (add-color #x87 #x00 #x87)
        (add-color #x87 #x00 #xaf)
        (add-color #x87 #x00 #xd7)
        (add-color #x87 #x00 #xff)
        (add-color #x87 #x5f #x00)
        (add-color #x87 #x5f #x5f)
        (add-color #x87 #x5f #x87)
        (add-color #x87 #x5f #xaf)
        (add-color #x87 #x5f #xd7)
        (add-color #x87 #x5f #xff)
        (add-color #x87 #x87 #x00)
        (add-color #x87 #x87 #x5f)
        (add-color #x87 #x87 #x87)
        (add-color #x87 #x87 #xaf)
        (add-color #x87 #x87 #xd7)
        (add-color #x87 #x87 #xff)
        (add-color #x87 #xaf #x00)
        (add-color #x87 #xaf #x5f)
        (add-color #x87 #xaf #x87)
        (add-color #x87 #xaf #xaf)
        (add-color #x87 #xaf #xd7)
        (add-color #x87 #xaf #xff)
        (add-color #x87 #xd7 #x00)
        (add-color #x87 #xd7 #x5f)
        (add-color #x87 #xd7 #x87)
        (add-color #x87 #xd7 #xaf)
        (add-color #x87 #xd7 #xd7)
        (add-color #x87 #xd7 #xff)
        (add-color #x87 #xff #x00)
        (add-color #x87 #xff #x5f)
        (add-color #x87 #xff #x87)
        (add-color #x87 #xff #xaf)
        (add-color #x87 #xff #xd7)
        (add-color #x87 #xff #xff)
        (add-color #xaf #x00 #x00)
        (add-color #xaf #x00 #x5f)
        (add-color #xaf #x00 #x87)
        (add-color #xaf #x00 #xaf)
        (add-color #xaf #x00 #xd7)
        (add-color #xaf #x00 #xff)
        (add-color #xaf #x5f #x00)
        (add-color #xaf #x5f #x5f)
        (add-color #xaf #x5f #x87)
        (add-color #xaf #x5f #xaf)
        (add-color #xaf #x5f #xd7)
        (add-color #xaf #x5f #xff)
        (add-color #xaf #x87 #x00)
        (add-color #xaf #x87 #x5f)
        (add-color #xaf #x87 #x87)
        (add-color #xaf #x87 #xaf)
        (add-color #xaf #x87 #xd7)
        (add-color #xaf #x87 #xff)
        (add-color #xaf #xaf #x00)
        (add-color #xaf #xaf #x5f)
        (add-color #xaf #xaf #x87)
        (add-color #xaf #xaf #xaf)
        (add-color #xaf #xaf #xd7)
        (add-color #xaf #xaf #xff)
        (add-color #xaf #xd7 #x00)
        (add-color #xaf #xd7 #x5f)
        (add-color #xaf #xd7 #x87)
        (add-color #xaf #xd7 #xaf)
        (add-color #xaf #xd7 #xd7)
        (add-color #xaf #xd7 #xff)
        (add-color #xaf #xff #x00)
        (add-color #xaf #xff #x5f)
        (add-color #xaf #xff #x87)
        (add-color #xaf #xff #xaf)
        (add-color #xaf #xff #xd7)
        (add-color #xaf #xff #xff)
        (add-color #xd7 #x00 #x00)
        (add-color #xd7 #x00 #x5f)
        (add-color #xd7 #x00 #x87)
        (add-color #xd7 #x00 #xaf)
        (add-color #xd7 #x00 #xd7)
        (add-color #xd7 #x00 #xff)
        (add-color #xd7 #x5f #x00)
        (add-color #xd7 #x5f #x5f)
        (add-color #xd7 #x5f #x87)
        (add-color #xd7 #x5f #xaf)
        (add-color #xd7 #x5f #xd7)
        (add-color #xd7 #x5f #xff)
        (add-color #xd7 #x87 #x00)
        (add-color #xd7 #x87 #x5f)
        (add-color #xd7 #x87 #x87)
        (add-color #xd7 #x87 #xaf)
        (add-color #xd7 #x87 #xd7)
        (add-color #xd7 #x87 #xff)
        (add-color #xd7 #xaf #x00)
        (add-color #xd7 #xaf #x5f)
        (add-color #xd7 #xaf #x87)
        (add-color #xd7 #xaf #xaf)
        (add-color #xd7 #xaf #xd7)
        (add-color #xd7 #xaf #xff)
        (add-color #xd7 #xd7 #x00)
        (add-color #xd7 #xd7 #x5f)
        (add-color #xd7 #xd7 #x87)
        (add-color #xd7 #xd7 #xaf)
        (add-color #xd7 #xd7 #xd7)
        (add-color #xd7 #xd7 #xff)
        (add-color #xd7 #xff #x00)
        (add-color #xd7 #xff #x5f)
        (add-color #xd7 #xff #x87)
        (add-color #xd7 #xff #xaf)
        (add-color #xd7 #xff #xd7)
        (add-color #xd7 #xff #xff)
        (add-color #xff #x00 #x00)
        (add-color #xff #x00 #x5f)
        (add-color #xff #x00 #x87)
        (add-color #xff #x00 #xaf)
        (add-color #xff #x00 #xd7)
        (add-color #xff #x00 #xff)
        (add-color #xff #x5f #x00)
        (add-color #xff #x5f #x5f)
        (add-color #xff #x5f #x87)
        (add-color #xff #x5f #xaf)
        (add-color #xff #x5f #xd7)
        (add-color #xff #x5f #xff)
        (add-color #xff #x87 #x00)
        (add-color #xff #x87 #x5f)
        (add-color #xff #x87 #x87)
        (add-color #xff #x87 #xaf)
        (add-color #xff #x87 #xd7)
        (add-color #xff #x87 #xff)
        (add-color #xff #xaf #x00)
        (add-color #xff #xaf #x5f)
        (add-color #xff #xaf #x87)
        (add-color #xff #xaf #xaf)
        (add-color #xff #xaf #xd7)
        (add-color #xff #xaf #xff)
        (add-color #xff #xd7 #x00)
        (add-color #xff #xd7 #x5f)
        (add-color #xff #xd7 #x87)
        (add-color #xff #xd7 #xaf)
        (add-color #xff #xd7 #xd7)
        (add-color #xff #xd7 #xff)
        (add-color #xff #xff #x00)
        (add-color #xff #xff #x5f)
        (add-color #xff #xff #x87)
        (add-color #xff #xff #xaf)
        (add-color #xff #xff #xd7)
        (add-color #xff #xff #xff)
        (add-color #x08 #x08 #x08)
        (add-color #x12 #x12 #x12)
        (add-color #x1c #x1c #x1c)
        (add-color #x26 #x26 #x26)
        (add-color #x30 #x30 #x30)
        (add-color #x3a #x3a #x3a)
        (add-color #x44 #x44 #x44)
        (add-color #x4e #x4e #x4e)
        (add-color #x58 #x58 #x58)
        (add-color #x62 #x62 #x62)
        (add-color #x6c #x6c #x6c)
        (add-color #x76 #x76 #x76)
        (add-color #x80 #x80 #x80)
        (add-color #x8a #x8a #x8a)
        (add-color #x94 #x94 #x94)
        (add-color #x9e #x9e #x9e)
        (add-color #xa8 #xa8 #xa8)
        (add-color #xb2 #xb2 #xb2)
        (add-color #xbc #xbc #xbc)
        (add-color #xc6 #xc6 #xc6)
        (add-color #xd0 #xd0 #xd0)
        (add-color #xda #xda #xda)
        (add-color #xe4 #xe4 #xe4)
        (add-color #xee #xee #xee))
      (when (= n 88)
        ;; Every 88-color terminal entry shares the rxvt-unicode /
        ;; xterm-88color layout (see the urxvt manual, "COLOURS AND
        ;; GRAPHICS", and urxvt's init.C): a 4x4x4 cube with levels
        ;; 0, 139, 205, 255 at slots 16-79 (index = (r*4+g)*4+b+16,
        ;; blue fastest) plus 8 gray steps at 80-87. Unlike other
        ;; 17-255-color entries these grid values are known, so they
        ;; are populated and take part in exact matching and
        ;; quantization.
        (loop :for r :in '(#x00 #x8b #xcd #xff)
              :do (loop :for g :in '(#x00 #x8b #xcd #xff)
                        :do (loop :for b :in '(#x00 #x8b #xcd #xff)
                                  :do (add-color r g b))))
        (loop :for v :in '(#x2e #x5c #x73 #x8b #xa2 #xb9 #xd0 #xe7)
              :do (add-color v v v))))
    ;; Everything past COUNTER stayed raw (fixnum 0): terminals whose
    ;; colors# falls between 17 and 255 without a known grid layout get
    ;; only the scheme and bright slots, and %exact-slot / quantization
    ;; must never look further (see the FILLED slot of the palette
    ;; struct).
    (setf (palette-filled *palette*) counter)))

;; Slots 0-15 are the terminal's own scheme, and their recorded values
;; are only the classic-xterm approximations: color_content cannot query
;; the terminal's palette (it returns ncurses' built-in defaults, e.g.
;; roughly (173 0 0) for slot 1 on a terminal with a custom red), and the
;; real colors are unknowable without an OSC 4 query round-trip. On
;; palettes with a known grid (the 240-slot 256-grid, and the 72-slot
;; 88-grid of rxvt-unicode / xterm-88color) those slots are therefore
;; excluded from matching: a wrong recorded value could win %exact-slot
;; or the Lab nearest-match and make lem use a slot whose rendered color
;; is not the recorded one. Palettes without a known grid (8/16 colors,
;; and 17-255-color entries whose grid values are unknowable from
;; ncurses — see the FILLED slot of the palette struct) have nothing
;; else to match against, so they keep matching 0-15 despite the
;; approximation.
(defun %match-base (palette)
  (if (>= (palette-filled palette) 88)
      16
      0))

;; Perceptual color distance for 256-color quantization.
;; sRGB -> CIELAB (D65 reference white) and CIE76 distance. This replaces
;; the earlier HSV distance, which distorted badly around grays and treated
;; hue and value errors identically.
;;
;; Type declarations + (speed 3) let SBCL compile all the arithmetic below
;; to unboxed double-float operations (no per-step generic dispatch or
;; consing). Color struct fields are untyped, so rgb-to-lab coerces at the
;; entry point; everything downstream is declared.

(declaim (ftype (function ((unsigned-byte 8)) (double-float 0.0d0 1.0d0))
                srgb-channel-to-linear))
(defun srgb-channel-to-linear (v)
  (declare (optimize (speed 3) (safety 2))
           (type (unsigned-byte 8) v))
  (let ((v (/ (float v 1.0d0) 255.0d0)))
    (declare (type (double-float 0.0d0 1.0d0) v))
    (if (<= v 0.04045d0)
        (/ v 12.92d0)
        (expt (/ (+ v 0.055d0) 1.055d0) 2.4d0))))


;; Named Lab constants.
(alexandria:define-constant +lab-epsilon+   (float (expt 6/29 3) 1.0d0)
  :test #'=)
(alexandria:define-constant +lab-slope+     (float (* (expt 29/6 2) 1/3) 1.0d0)
  :test #'=)
(alexandria:define-constant +lab-intercept+ (float 4/29 1.0d0)
  :test #'=)

(declaim (ftype (function (double-float double-float double-float)
                          (values double-float double-float double-float))
                xyz-to-lab))
(defun xyz-to-lab (x y z)
  (declare (optimize (speed 3) (safety 2))
           (type double-float x y z))
  ;; Reference white D65: Xn=0.95047, Yn=1.0, Zn=1.08883
  (flet ((f (tt)
           (declare (type double-float tt))
           (if (> tt +lab-epsilon+)
               (expt tt (/ 1.0d0 3.0d0))
               (+ (* +lab-slope+ tt) +lab-intercept+))))
    (declare (inline f))
    (let* ((xn (/ x 0.95047d0))
           (yn (/ y 1.0d0))
           (zn (/ z 1.08883d0))
           (fx (f xn))
           (fy (f yn))
           (fz (f zn)))
      (declare (type double-float xn yn zn fx fy fz))
      (values (+ (* 116.0d0 fy) -16.0d0)
              (* 500.0d0 (- fx fy))
              (* 200.0d0 (- fy fz))))))

(declaim (ftype (function (lem:color)
                          (values double-float double-float double-float))
                rgb-to-lab))
(defun rgb-to-lab (color)
  (declare (optimize (speed 3) (safety 2)))
  ;; Color struct fields are untyped in lem-core; the protocol guarantees
  ;; 0..255 components, asserted here once instead of trusting them.
  (let* ((r (srgb-channel-to-linear (the (unsigned-byte 8) (lem:color-red color))))
         (g (srgb-channel-to-linear (the (unsigned-byte 8) (lem:color-green color))))
         (b (srgb-channel-to-linear (the (unsigned-byte 8) (lem:color-blue color))))
         ;; sRGB -> XYZ (D65), scaled so Y is in 0..1
         (x (+ (* 0.4124564d0 r) (* 0.3575761d0 g) (* 0.1804375d0 b)))
         (y (+ (* 0.2126729d0 r) (* 0.7151522d0 g) (* 0.0721750d0 b)))
         (z (+ (* 0.0193339d0 r) (* 0.1191920d0 g) (* 0.9503041d0 b))))
    (declare (type double-float r g b x y z))
    (xyz-to-lab x y z)))

(declaim (ftype (function (lem:color lem:color) double-float)
                rgb-lab-distance))
(defun rgb-lab-distance (color-1 color-2)
  "CIE76: Euclidean distance in CIELAB space."
  (declare (optimize (speed 3) (safety 2)))
  (multiple-value-bind (l1 a1 b1) (rgb-to-lab color-1)
    (multiple-value-bind (l2 a2 b2) (rgb-to-lab color-2)
      (declare (type double-float l1 a1 b1 l2 a2 b2))
      (let ((dl (- l1 l2))
            (da (- a1 a2))
            (db (- b1 b2)))
        (declare (type double-float dl da db))
        (+ (* dl dl) (* da da) (* db db))))))

;; See the direct-color notes at the *direct-color* declaration (top of
;; file) for why packed RGB goes through init_extended_pair below.
(cffi:defcfun ("tigetflag" %tigetflag) :int (cap :string))
(cffi:defcfun ("init_extended_pair" %init-extended-pair)
    :int (pair :int) (f :int) (b :int))

(declaim (inline rgb-to-packed))
(defun rgb-to-packed (color)
  (logior (ash (lem:color-red color) 16)
          (ash (lem:color-green color) 8)
          (lem:color-blue color)))

;; OSC-4 exact-color redefinition. On 256-color terminals the 6x6x6 grid
;; has no entry for most theme colors (e.g. catppuccin's #24273a quantizes
;; to a flat gray). When the terminal can redefine its palette (OSC 4,
;; which init_color emits), allocate grid slots (16+) on demand and set
;; them to the exact RGB instead of quantizing. Colors 0-15 are never
;; touched: they are the terminal's own scheme.

;; Next palette slot to hand out lives in the palette struct itself (see
;; the palette declaration at the top of the file) and is threaded through
;; the allocation functions explicitly.

;; Return the index of the palette slot holding COLOR exactly, or nil.
;; Matching starts at %match-base: slots 0-15 hold only approximations.
;; The scan stops at FILLED: slots past it have no recorded RGB (their
;; grid values are unknowable from ncurses), and their array entries are
;; not lem:color objects.
(defun %exact-slot (palette color)
  ;; Bound and colors vector are hoisted: loop re-evaluates the limit
  ;; form every iteration, and nothing here mutates the palette.
  (let* ((colors (palette-colors palette))
         (bound (min (palette-filled palette) (length colors))))
    (loop :for i :from (%match-base palette)
          :below bound
          :for c = (aref colors i)
          :when (and (= (lem:color-red c) (lem:color-red color))
                     (= (lem:color-green c) (lem:color-green color))
                     (= (lem:color-blue c) (lem:color-blue color)))
            :return i)))

;; Mark a palette slot as handed out. Slots 0-15 are the terminal's own
;; scheme and are never redefined, so they need no tracking.
(defun %mark-slot-used (palette slot)
  (when (>= slot 16)
    (setf (aref (palette-used-slots palette) slot) 1)))

(defun %allocate-exact-color (palette color)
  "Redefine the next free grid slot (16+) in PALETTE to COLOR's exact RGB
via init_color (OSC 4) and return the slot index, or nil when no slot is
available, init_color fails, or the readback mismatched. Slots already
handed out to another color (by exact match, allocation, or quantization
— see USED-SLOTS) are skipped: redefining one would silently change the
color of every cell that resolved to it. A failed init_color call (ERR:
no OSC 4 emitted, ncurses' color table untouched) does not burn the
slot: the caller falls back to nearest-match quantization."
  (let ((colors (palette-colors palette)))
    (when (%allocate-exact-color-preconditions-p palette)
      (let* ((used (palette-used-slots palette))
             (slot (loop :for i :from (palette-next-redefine-slot palette)
                         :below (length colors)
                         :when (zerop (aref used i))
                           :return i)))
        (when slot
          ;; ceiling, not round: ncurses truncates the 0..1000
          ;; value back to 8-bit (val*255/1000) when it emits
          ;; OSC 4, so rounding down loses one bit of precision
          ;; (e.g. #24273a went out as rgb:23/27/39). Ceiling
          ;; makes the round-trip exact for every 8-bit value.
          (let* ((want-r (ceiling (* (lem:color-red color) 1000/255)))
                 (want-g (ceiling (* (lem:color-green color) 1000/255)))
                 (want-b (ceiling (* (lem:color-blue color) 1000/255)))
                 ;; init-color emits OSC 4 and returns OK (0) or ERR (-1).
                 (ok (zerop (charms/ll:init-color slot want-r want-g want-b))))
            (when ok
              ;; ncurses accepts the call optimistically (OK does not prove the
              ;; terminal honored OSC 4), but the readback still guards against
              ;; curses-level failures such as channel clamping in :short. A
              ;; mismatch still CONSUMES the slot: init_color already emitted
              ;; OSC 4 with the value ncurses stored, so the slot's
              ;; terminal-side meaning changed even though it is not exact.
              ;; Mark it used and advance next-redefine-slot past it —
              ;; %reset-redefined-colors (which resets 16 through
              ;; next-redefine-slot - 1) then restores it on exit, and later
              ;; colors never retry a deterministically-failing slot. Record
              ;; what the slot actually holds (ncurses' stored value) so
              ;; exact matching and quantization see the truth; the caller
              ;; gets nil and quantization recovers the color, typically
              ;; resolving right back to this slot.
              (cffi:with-foreign-objects ((r :short) (g :short) (b :short))
                (let ((read-back (zerop (charms/ll:color-content slot r g b))))
                  (if (and read-back
                           (= want-r (cffi:mem-ref r :short))
                           (= want-g (cffi:mem-ref g :short))
                           (= want-b (cffi:mem-ref b :short)))
                      (progn
                        (setf (aref colors slot) color)
                        (setf (aref used slot) 1)
                        (setf (palette-next-redefine-slot palette) (1+ slot))
                        slot)
                      (progn
                        (when read-back
                          (setf (aref colors slot)
                                (lem:make-color
                                 (round (* (cffi:mem-ref r :short) 255/1000))
                                 (round (* (cffi:mem-ref g :short) 255/1000))
                                 (round (* (cffi:mem-ref b :short) 255/1000)))))
                        (setf (aref used slot) 1)
                        (setf (palette-next-redefine-slot palette) (1+ slot))
                        nil)))))))))))

(defun %reset-redefined-colors ()
  "Restore the palette slots lem redefined via OSC 4 (slots 16 and up)
to their terminal defaults with OSC 104 (\"reset palette entry\"). The
original values are not recoverable in-process (ncurses' color table now
holds lem's values), so the terminal itself is asked to restore them.
Only redefined slots are reset: slots 0-15 (the terminal's scheme) are
never touched, and resetting everything via a bare OSC 104 would. Slots
are printed directly rather than through ncurses: addstr would just
buffer them into the screen state."
  #+win32 nil ; PDCurses: no OSC 4, nothing to restore over a real terminal
  #-win32
  (when (and *palette*
             (not *direct-color*))
    (let ((slots (loop :for i :from 16 :below (palette-next-redefine-slot *palette*)
                       :collect i)))
      (when slots
        ;; Escape sequences are printed directly rather than through
        ;; ncurses: addstr would just buffer them into the screen state.
        (let ((seq (format nil "~C]104;~{~D~^;~}~C" #\Esc slots #\Bel)))
          (cond (*term-io*
                 (%fputs seq *term-io*)
                 (%fflush *term-io*))
                (t
                 (princ seq))))))))

(cffi:defcfun ("fputs" %fputs) :int (s :string) (fp :pointer))
(cffi:defcfun ("fflush" %fflush) :int (fp :pointer))

;; Guards of %allocate-exact-color: palettes that can never allocate
;; (8/16-color terminals, redefinition disabled) must not even scan.
(defun %allocate-exact-color-preconditions-p (palette)
  (and *enable-palette-redefinition*
       *can-change-color*
       (>= (length (palette-colors palette)) 256)))

;; When the allocator runs dry (after many theme loads, ~20 slots per
;; theme against 240 grid slots), get-color-rgb degrades to Lab
;; quantization instead of resetting the palette. A reset from inside a
;; color lookup is not safe: it hands terminal slots back (OSC 104) and
;; rebuilds the palette while earlier lookups of the same frame already
;; hold slot numbers — the pending pair then binds a stale slot, and the
;; cache invalidations (pair table, attribute cache, window screens)
;; land mid-redisplay, forcing a full redraw and repeating for every
;; further miss. Slots are handed out monotonically for the whole
;; session (see update-background-color), so every slot number ever
;; returned stays meaningful; the fallback only loses exactness for
;; colors that no longer fit, and the terminal-side palette is restored
;; on exit via %reset-redefined-colors (term-finalize).

(defun get-color-rgb (color-1)
  "Resolve COLOR-1 (a lem:color) to a terminal color number: packed
24-bit RGB in direct-color mode; otherwise an exact palette slot if one
exists, a freshly redefined slot (OSC 4) if possible, and nearest-match
quantization (CIE76 in CIELAB) as a last resort. When the OSC-4
allocator has run dry (no free slot left), this and all future colors
degrade to quantization rather than resetting the palette: a reset
invalidates slot numbers that earlier lookups of the same redisplay
already hold (see the comment above
%allocate-exact-color-preconditions-p)."
  (when *direct-color*
    ;; Direct color: the color number is the exact packed RGB.
    (return-from get-color-rgb (rgb-to-packed color-1)))
  ;; Before the first term-init, *palette* does not exist yet (early
  ;; attribute resolution can reach here — see background-color, which
  ;; guards the same case). There is nothing to match or allocate
  ;; against; degrade to color number 0, the terminal default, like
  ;; every other pre-init answer.
  (when (null *palette*)
    (return-from get-color-rgb 0))
  ;; Every slot this function hands out is marked used (see
  ;; %mark-slot-used): exact grid matches and quantized targets too, so
  ;; that a later OSC 4 allocation can never redefine a slot other
  ;; colors already resolve to.
  (or (let ((slot (%exact-slot *palette* color-1)))
        (when slot (%mark-slot-used *palette* slot))
        slot)
      (let ((slot (%allocate-exact-color *palette* color-1)))
        (when slot (%mark-slot-used *palette* slot))
        slot)
      ;; fallback: nearest palette entry by perceptual distance
      ;; (from %match-base up: slots 0-15 hold only approximations;
      ;; the scan stops at FILLED — entries past it have no recorded
      ;; RGB and are not lem:color objects)
      (let* ((colors (palette-colors *palette*))
             (bound (min (palette-filled *palette*) (length colors)))
             (base (%match-base *palette*))
             (mini most-positive-double-float)
             (best-number 0))
        (declare (type double-float mini))
        (loop :for i :from base
              :below bound
              :for color-2 = (aref colors i)
              :do (let ((dist (rgb-lab-distance
                               color-1
                               color-2)))
                    (when (< dist mini)
                      (setf mini dist
                            best-number i))))
        (%mark-slot-used *palette* best-number)
        best-number)))

(defun get-color-1 (string)
  "Memoized version of parse-color + get-color-rgb for a color NAME
string. The cache key is (string . packed-rgb), not the string alone:
palette slots are handed out monotonically (update-foreground-color /
update-background-color no longer reset the allocator), so an entry
stays valid as long as the name resolves to the same RGB — but two
themes may reuse one name (say red) for different values, and the
RGB component makes such a switch a cache miss instead of a stale hit.
The RGB is packed into an integer rather than keyed on the lem:color
struct: EQUAL on structures is identity, and every theme load re-parses
its colors into fresh structs (clear-parse-color-cache), so struct keys
from earlier themes could never be hit again and the table would grow
across theme switches. With the packed RGB, an entry survives theme
loads as long as the name's RGB is unchanged, and the table only grows
with distinct (name, RGB) pairs actually seen."
  (let ((color (lem:parse-color string)))
    (when color
      (let ((key (cons string (rgb-to-packed color))))
        (multiple-value-bind (cached foundp) (gethash key *color-index-cache*)
          (if foundp
              cached
              (let ((index (get-color-rgb color)))
                (setf (gethash key *color-index-cache*) index))))))))

(defun get-color (string)
  (let ((color (get-color-1 string)))
    (if color
        (values color t)
        (values 0 nil))))

;; The pair number travels through %wattr-set's :short argument (see
;; attribute.lisp), so it must stay within signed-short range. ncurses
;; caps COLOR_PAIRS at 32767 for its own short-based pairs, but that
;; cap is not a contract across builds — min it into the allocation
;; guard so the counter can never outgrow what the FFI can carry.
(alexandria:define-constant +max-pair-number+ 32767 :test #'=)

(defun init-pair (pair-color)
  ;; Store the raw pair number, not COLOR_PAIR(n): the A_COLOR field of
  ;; OR'd attribute bits is only 8 bits wide, so a pair number above 255
  ;; (common in direct-color mode, where every distinct RGB combination
  ;; gets a pair) would spill into other attribute bits. Callers apply
  ;; the pair through wattr_set (%wattr-set), whose pair argument is a
  ;; short taking the full range.
  ;; The allocation call is checked: OK does not fail in practice, but
  ;; caching the number after an ERR would leave the pair table pointing
  ;; at a pair ncurses never allocated, and every cell using it would
  ;; render undefined colors. On ERR the counter is not burned (the
  ;; allocation failed) and pair 0 (the default) is cached for this
  ;; combination, matching the exhausted-pairs fallback below.
  ;; The allocated number is RETURNED (not just cached): get-color-pair
  ;; passes this value through to attribute resolution, so a nil here
  ;; would flow into wattr_set's short argument as a type error.
  ;; The bound is EXCLUSIVE on both constraints: ncurses requires
  ;; pair < COLOR_PAIRS, and %wattr-set's short cannot carry a pair
  ;; number above +max-pair-number+ — so the guard tests the counter
  ;; against the last allocatable pair number, not the pair count.
  ;; A guard at the count itself would let the counter reach
  ;; COLOR_PAIRS and burn one allocation attempt on a pair ncurses
  ;; always rejects (the ERR rollback below would recover, but the
  ;; attempt is pure waste).
  (if (< *pair-counter* (min (1- *color-pairs*) +max-pair-number+))
      (let* ((ok (progn
                   (incf *pair-counter*)
                   (if *direct-color*
                       (zerop (%init-extended-pair *pair-counter*
                                                   (car pair-color)
                                                   (cdr pair-color)))
                       (zerop (charms/ll:init-pair *pair-counter*
                                                   (car pair-color)
                                                   (cdr pair-color))))))
             (stored (if ok *pair-counter* 0)))
        (setf (gethash pair-color *color-pair-table*) stored)
        (unless ok
          (decf *pair-counter*))
        stored)
      ;; Bound exhausted: cache the default pair without touching the
      ;; counter (it must stay at the cap; a rollback here would let the
      ;; next call re-allocate an already-assigned pair number).
      (setf (gethash pair-color *color-pair-table*) 0)))

(defun get-color-pair (fg-color-name bg-color-name)
  (multiple-value-bind (default-fg default-bg)
      ;; Direct color: the default pair cannot carry packed RGB (see
      ;; set-default-color), so the theme's own foreground/background are
      ;; substituted for nil names; -1 keeps the terminal default where
      ;; the theme sets none. Otherwise the curses default pair is read
      ;; back as before.
      (if *direct-color*
          (values (if *direct-foreground*
                      (get-color-rgb *direct-foreground*)
                      -1)
                  (if *direct-background*
                      (get-color-rgb *direct-background*)
                      -1))
          (get-default-colors))
    (let* ((fg-color (if (null fg-color-name)
                         default-fg
                         (get-color fg-color-name)))
           (bg-color (if (null bg-color-name)
                         default-bg
                         (get-color bg-color-name)))
           (pair-color (cons fg-color bg-color)))
      ;; Bounds handling (exhaustion, ERR) lives entirely in init-pair,
      ;; which caches the default pair for this combination on both —
      ;; so post-exhaustion lookups stay cache hits instead of
      ;; re-parsing colors for every rendered cell.
      (cond ((gethash pair-color *color-pair-table*))
            (t (init-pair pair-color))))))

(defun get-default-colors ()
  (cffi:with-foreign-pointer (f (cffi:foreign-type-size '(:pointer :short)))
    (cffi:with-foreign-pointer (b (cffi:foreign-type-size '(:pointer :short)))
      (charms/ll:pair-content 0 f b)
      (values (cffi:mem-ref f :short)
              (cffi:mem-ref b :short)))))

;; Cleared-region background (direct-color mode): wclrtoeol/wclrtobot
;; fill with the WINDOW's curses background, and pair 0 cannot serve as
;; that background here — the default pair stays -1/-1 because
;; assume_default_colors cannot carry packed RGB. Instead each window's
;; background is set to the theme's own default pair via
;; set-window-background (the wide-char API; the chtype path cannot
;; carry pairs above 255). When the theme sets no background the fill
;; is pair 0: untouched regions keep the terminal default, which is
;; what keeps background-transparent terminals transparent. Non-direct
;; mode needs none of this — pair 0 already IS the theme pair there.
;; Views pick changes up through the generation counter because the
;; frontend keeps no registry of live windows.
(defvar *window-background-generation* 0)
(defvar *window-background-pair* 0)

;; Background fills: wclrtoeol/wclrtobot paint cleared cells with the
;; WINDOW's background (wbkgdset's value), whose chtype color field is
;; only 8 bits wide — the same limitation as OR'd attribute bits, so a
;; pair above 255 (common in direct-color mode) cannot be transported
;; that way. The wide-char API (setcchar + wbkgrndset) carries the pair
;; separately in cchar_t's ext_color field, a full int on builds with
;; NCURSES_EXT_COLORS (spike-verified on ncurses 6.6: pairs 300/500
;; round-trip through wclrtoeol and wclrtobot fills on xterm-256color
;; and xterm-direct, while the chtype path truncated 500 to 244).
;; cl-charms binds neither function, hence these bindings.
(cffi:defcfun ("setcchar" %setcchar)
    :int
  "Fill the cchar_t at WCVAL from a NUL-terminated wide string, ATTRS
and a color PAIR. PAIR travels as a short — bounded by
+max-pair-number+ like %wattr-set's. Returns OK (0) or ERR (-1)."
  (wcval :pointer)
  (wchstring :pointer)
  (attrs charms/ll::attr_t)
  (pair :short)
  (opts :pointer))

(cffi:defcfun ("wbkgrndset" %wbkgrndset)
    :void
  "Make the cchar_t at WCH the background of WIN: subsequent
wclrtoeol/wclrtobot fills use it. Does not alter already rendered
cells."
  (win :pointer)
  (wch :pointer))

(defun set-window-background (win attrs pair)
  "Set WIN's background cell to a blank with ATTRS and PAIR: the
colors later wclrtoeol/wclrtobot fills paint. Affects future fills
only, not already rendered cells. Keep ATTRS at 0 — a background
cell only shows its color."
  (cffi:with-foreign-objects ((cell :char 64)    ; >= sizeof(cchar_t): the struct
                                 ; layout varies across ncurses releases and
                                 ; setcchar fills it opaquely through the
                                 ; pointer, so an over-allocated buffer is
                                 ; used instead of a CFFI struct definition
                              (chars :uint32 2)) ; wchar_t is 32-bit on Linux
    (setf (cffi:mem-aref chars :uint32 0) 32     ; #\Space
          (cffi:mem-aref chars :uint32 1) 0)     ; NUL terminator
    ;; The only %setcchar failure mode is bad arguments (constant here:
    ;; attrs = a_normal, pair <= +max-pair-number+), so the return
    ;; value is checked only to keep ERR from reaching wbkgrndset.
    (when (zerop (%setcchar cell chars attrs pair (cffi:null-pointer)))
      (%wbkgrndset win cell))))

(defun %recompute-window-background ()
  (setf *window-background-pair*
        (cond ((and *direct-color* *direct-background*)
               ;; A bg-only pair (-1 = terminal default fg): the fill
               ;; cell is a blank, so its foreground is never visible,
               ;; and keying on the background alone means fg-only
               ;; theme changes reuse the pair instead of burning a
               ;; new one per foreground variation. init-pair caches
               ;; it under (-1 . bg) — a key text pairs can only
               ;; produce when the theme names no foreground, in which
               ;; case fill and text share the allocation.
               (init-pair (cons -1 (get-color-rgb *direct-background*))))
              (t
               ;; 0 = the terminal default: keeps untouched regions
               ;; transparent on direct-color terminals with background
               ;; opacity, and is a no-op in non-direct mode (pair 0
               ;; already carries the theme colors there).
               0)))
  (incf *window-background-generation*)
  ;; stdscr has no lazy sync point of its own (view windows pick the
  ;; generation up at their next fill), so its background is applied
  ;; here directly. Applied in every mode: in non-direct mode the pair
  ;; is 0, which is what newwin initialized the backgrounds to anyway.
  (set-window-background
   charms/ll:*stdscr* charms/ll:a_normal *window-background-pair*))

(defun window-background ()
  "Two values describing how cleared regions (wclrtoeol/wclrtobot
fills) must be painted: a GENERATION counter bumped whenever the
correct fill changes, and the PAIR to fill with — the theme
background pair in direct-color mode, 0 (terminal default in direct
mode, theme colors in non-direct mode) otherwise."
  (values *window-background-generation*
          *window-background-pair*))

(defun set-default-color (foreground background)
  (if *direct-color*
      ;; assume_default_colors takes shorts; packed RGB values do not fit.
      ;; Keep the default pair at -1/-1 (terminal default): theme colors
      ;; are applied per cell through pairs, and *direct-background* /
      ;; *direct-foreground* record the theme colors for background-color
      ;; and for get-color-pair's nil-name substitution.
      (progn
        (setf *direct-background* (and background (lem:parse-color background)))
        (setf *direct-foreground* (and foreground (lem:parse-color foreground)))
        (charms/ll:assume-default-colors -1 -1))
      (let ((fg-color (if foreground (get-color foreground) -1))
            (bg-color (if background (get-color background) -1)))
        (charms/ll:assume-default-colors fg-color
                                         bg-color)))
  (%recompute-window-background))

(defun update-foreground-color (name)
  "Set the default foreground to NAME. In direct-color mode the default
pair stays -1/-1 (assume_default_colors cannot carry packed RGB); NAME
is recorded in *direct-foreground* and substituted by get-color-pair
when a cell names no explicit foreground."
  (multiple-value-bind (fg found) (get-color name)
    (cond ((and found *direct-color*)
           ;; get-color returns the terminal index (packed RGB in direct
           ;; mode), not a lem:color — re-parse for *direct-foreground*,
           ;; which get-color-pair feeds back into get-color-rgb.
           (setf *direct-foreground* (lem:parse-color name))
           (%recompute-window-background)
           t)
          (found
           (let ((bg (nth-value 1 (get-default-colors))))
             (charms/ll:assume-default-colors fg bg)
             (%recompute-window-background)
             t))
          (t
           (error "Undefined color: ~A" name)))))

(defun update-background-color (name)
  "No palette reset here: update-foreground-color and
update-background-color run back-to-back when a theme loads
(apply-theme -> set-foreground -> set-background), and a reset
between them rewinds the allocator to slot 16, making the
background allocation clobber the slot the foreground just
claimed and leaving the default pair pointing at the background
color (dark-on-dark text). Slots are handed out monotonically
instead; the terminal-side palette is restored on exit via
%reset-redefined-colors."
  (multiple-value-bind (bg found) (get-color name)
    (cond ((and found *direct-color*)
           (setf *direct-background* (lem:parse-color name))
           (%recompute-window-background)
           t)
          (found
           (let ((fg (nth-value 0 (get-default-colors))))
             (charms/ll:assume-default-colors fg bg)
             (%recompute-window-background)
             t))
          (t
           (error "Undefined color: ~A" name)))))

(defun background-color ()
  "The theme background as a lem:color. In direct-color mode this is
the tracked *direct-background* (defaults to black when the theme sets
none); otherwise it is read back from the curses default pair. Called
before term-init has built the palette (e.g. during early attribute
resolution), it returns black: the terminal default background."
  (if *direct-color*
      (or *direct-background* (lem:make-color 0 0 0))
      (let ((b (nth-value 1 (get-default-colors))))
        (if (or (= b -1)
                (null *palette*)
                (>= b (length (palette-colors *palette*))))
            (lem:make-color 0 0 0)
            (aref (palette-colors *palette*) b)))))

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
  (cl-setlocale:set-all-to-native)
  (if *tty-name*
      (term-init-tty *tty-name*)
      (charms/ll:initscr))
  (when (zerop (charms/ll:has-colors))
    (charms/ll:endwin)
    (write-line "Please execute TERM=xterm-256color and try again.")
    (return-from term-init nil))
  (charms/ll:start-color)
  ;; enable default color code (-1)
  #+win32(charms/ll:use-default-colors)
  ;; Direct-color mode: terminfo "RGB" boolean present (tigetflag returns
  ;; 1 for present, 0 for cancelled, -1 for absent). tigetflag is an
  ;; ncurses/terminfo API, absent in PDCurses (win32).
  #+win32(setf *direct-color* nil)
  #-win32(setf *direct-color* (= 1 (%tigetflag "RGB")))
  ;; A direct<->non-direct switch invalidates every color pair number:
  ;; pairs allocated in the other mode hold packed RGB or palette slots
  ;; that mean something else here, and long-lived attribute instances
  ;; (overlays, prompt text) may still reference the old numbers. Drop
  ;; the pair table and counter so re-allocation starts clean; plain
  ;; re-inits within the same mode keep pair numbers stable. In direct
  ;; mode the counter is only reset here: get-color-pair caches per
  ;; (fg . bg), so allocation is bounded by the distinct color combos of
  ;; all themes seen, not by theme loads.
  (unless (eq *last-direct-color* *direct-color*)
    (clrhash *color-pair-table*)
    (setf *pair-counter* 0)
    ;; The counter reset reuses pair numbers from 1 for different
    ;; fg/bg combinations. Anonymous attribute instances (overlays,
    ;; make-attribute results) keep cached (attrs . pair) values that
    ;; clear-all-attribute-cache cannot reach; bump the generation so
    ;; they miss and recompute instead of rendering another
    ;; combination's colors (search highlights and similar).
    (lem:invalidate-all-attribute-caches))
  (setf *last-direct-color* *direct-color*)
  (init-colors charms/ll:*colors*)
  (set-default-color nil nil)
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (charms/ll:raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (setf charms/ll::*escdelay* 0)
  ;; (charms/ll:curs-set 0)
  ;; for mouse
  (when (= *mouse-mode* 1)
    (enable-mouse))
  t)

(defun term-set-tty (tty-name)
  (setf *tty-name* tty-name))

(defun term-finalize ()
  (%reset-redefined-colors)
  (when *term-io*
    (fclose *term-io*)
    (setf *term-io* nil))
  (charms/ll:endwin)
  (charms/ll:delscreen charms/ll:*stdscr*))

(defun update-cursor-shape (cursor-type)
  (check-type cursor-type lem:cursor-type)
  (uiop:run-program `("printf"
                      ,(format nil "~C[~D q"
                               #\Esc
                               (case cursor-type
                                 (:box 2)
                                 (:bar 5)
                                 (:underline 4)
                                 (otherwise 2))))
                    :output :interactive
                    :ignore-error-status t))

(defun get-display-width ()
  (max 5 charms/ll:*cols*))

(defun get-display-height ()
  (max 3 charms/ll:*lines*))
