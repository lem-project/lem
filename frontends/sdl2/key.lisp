(defpackage :lem-sdl2/key
  (:use :cl)
  (:export :convert-to-sym
           :make-modifier
           :update-modifier
           :keysym-to-key
           :make-key-with-modifier))
(in-package :lem-sdl2/key)

(defstruct (keyinfo (:type list))
  keycode
  sym
  text-input-p)

(defparameter *code-name-table*
  `((,sdl2-ffi:+sdlk-backspace+ "Backspace" nil)
    (,sdl2-ffi:+sdlk-tab+ "Tab" nil)
    (,sdl2-ffi:+sdlk-return+ "Return" nil)
    (,sdl2-ffi:+sdlk-insert+ "Insert" nil)
    (,sdl2-ffi:+sdlk-delete+ "Delete" nil)
    (,sdl2-ffi:+sdlk-space+ "Space" t)
    (,sdl2-ffi:+sdlk-home+ "Home" nil)
    (,sdl2-ffi:+sdlk-end+ "End" nil)
    (,sdl2-ffi:+sdlk-pageup+ "PageUp" nil)
    (,sdl2-ffi:+sdlk-pagedown+ "PageDown" nil)
    (,sdl2-ffi:+sdlk-escape+ "Escape" nil)
    (,sdl2-ffi:+sdlk-left+ "Left" nil)
    (,sdl2-ffi:+sdlk-right+ "Right" nil)
    (,sdl2-ffi:+sdlk-up+ "Up" nil)
    (,sdl2-ffi:+sdlk-down+ "Down" nil)
    (,sdl2-ffi:+sdlk-f1+ "F1" nil)
    (,sdl2-ffi:+sdlk-f2+ "F2" nil)
    (,sdl2-ffi:+sdlk-f3+ "F3" nil)
    (,sdl2-ffi:+sdlk-f4+ "F4" nil)
    (,sdl2-ffi:+sdlk-f5+ "F5" nil)
    (,sdl2-ffi:+sdlk-f6+ "F6" nil)
    (,sdl2-ffi:+sdlk-f7+ "F7" nil)
    (,sdl2-ffi:+sdlk-f8+ "F8" nil)
    (,sdl2-ffi:+sdlk-f9+ "F9" nil)
    (,sdl2-ffi:+sdlk-f10+ "F10" nil)
    (,sdl2-ffi:+sdlk-f11+ "F11" nil)
    (,sdl2-ffi:+sdlk-f12+ "F12" nil)))

(defun convert-to-sym (code)
  (let ((keyinfo (assoc code *code-name-table*)))
    (if keyinfo
        (values (keyinfo-sym keyinfo) (keyinfo-text-input-p keyinfo))
        (when (<= code #x110000)
          (values (string (code-char code))
                  t)))))

(defun make-key* (&key ctrl meta shift sym)
  (when (and ctrl (equal sym "i"))
    (setf ctrl nil
          sym "Tab"))
  (or (and (= 1 (length sym))
           shift
           (multiple-value-bind (char shift-p)
               (lem-sdl2/layout:shift-char (char sym 0))
             (and shift-p
                  (lem:make-key :ctrl ctrl
                                :meta meta
                                :shift nil
                                :sym (string char)))))
      (lem:make-key :ctrl ctrl
                    :meta meta
                    :shift shift
                    :sym sym)))

(defstruct modifier
  shift
  ctrl
  meta)

(defun get-modifier (keysym)
  (let* ((mod (sdl2:mod-value keysym))
         (shift (= 1 (logand 1 mod)))
         (ctrl (= 64 (logand 64 mod)))
         (meta (= 256 (logand 256 mod))))
    (make-modifier :shift shift :ctrl ctrl :meta meta)))

(defun update-modifier (modifier keysym)
  (let ((modifier2 (get-modifier keysym)))
    (setf (modifier-shift modifier) (modifier-shift modifier2))
    (setf (modifier-ctrl modifier) (modifier-ctrl modifier2))
    (setf (modifier-meta modifier) (modifier-meta modifier2))))

(defun contain-control-p (keysym)
  (let* ((mod (sdl2:mod-value keysym))
         (ctrl (= 64 (logand 64 mod))))
    (or ctrl)))

(defun modifier-pressed-p (modifier)
  (or (modifier-ctrl modifier)
      (modifier-meta modifier)))

(defun keysym-to-key (keysym)
  (let ((code (sdl2:sym-value keysym))
        (modifier (get-modifier keysym)))
    (multiple-value-bind (sym text-input-p) (convert-to-sym code)
      (when sym
        (when (or (not text-input-p)
                  (modifier-pressed-p modifier)
                  (< 256 code))
          (make-key* :shift (modifier-shift modifier)
                     :ctrl (modifier-ctrl modifier)
                     :meta (modifier-meta modifier)
                     :sym sym))))))

(defun make-key-with-modifier (modifier sym &key (without-shift t))
  (make-key* :ctrl (modifier-ctrl modifier)
             :meta (modifier-meta modifier)
             :shift (if without-shift nil (modifier-shift modifier))
             :sym sym))
