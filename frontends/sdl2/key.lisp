(defpackage :lem-sdl2/key
  (:use :cl)
  (:export :convert-to-sym
           :make-modifier
           :update-modifier
           :keysym-to-key
           :make-key-with-modifier))
(in-package :lem-sdl2/key)

(defparameter *code-name-table*
  `((,sdl2-ffi:+sdlk-backspace+ . "Backspace")
    (,sdl2-ffi:+sdlk-tab+ . "Tab")
    (,sdl2-ffi:+sdlk-return+ . "Return")
    (,sdl2-ffi:+sdlk-delete+ . "Delete")
    (,sdl2-ffi:+sdlk-space+ . "Space")
    (,sdl2-ffi:+sdlk-home+ . "Home")
    (,sdl2-ffi:+sdlk-end+ . "End")
    (,sdl2-ffi:+sdlk-pageup+ . "PageUp")
    (,sdl2-ffi:+sdlk-pagedown+ . "PageDown")
    (,sdl2-ffi:+sdlk-escape+ . "Escape")
    (,sdl2-ffi:+sdlk-f1+ . "F1")
    (,sdl2-ffi:+sdlk-f2+ . "F2")
    (,sdl2-ffi:+sdlk-f3+ . "F3")
    (,sdl2-ffi:+sdlk-f4+ . "F4")
    (,sdl2-ffi:+sdlk-f5+ . "F5")
    (,sdl2-ffi:+sdlk-f6+ . "F6")
    (,sdl2-ffi:+sdlk-f7+ . "F7")
    (,sdl2-ffi:+sdlk-f8+ . "F8")
    (,sdl2-ffi:+sdlk-f9+ . "F9")
    (,sdl2-ffi:+sdlk-f10+ . "F10")
    (,sdl2-ffi:+sdlk-f11+ . "F11")
    (,sdl2-ffi:+sdlk-f12+ . "F12")))

(defun convert-to-sym (code-or-char)
  (let ((code (etypecase code-or-char
                (integer code-or-char)
                (character (char-code code-or-char)))))
    (let ((sym (alexandria:assoc-value *code-name-table* code)))
      (if sym
          (values sym t)
          (when (<= code #x110000)
            (values (string (code-char code))
                    nil))))))


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

(defun keysym-to-key (keysym)
  (let* ((code (sdl2:sym-value keysym))
         (modifier (get-modifier keysym)))
    (multiple-value-bind (sym converted) (convert-to-sym code)
      (when (and sym (or converted (modifier-ctrl modifier)))
        (lem:make-key :shift (modifier-shift modifier)
                      :ctrl (modifier-ctrl modifier)
                      :meta (modifier-meta modifier)
                      :sym sym)))))

(defun make-key-with-modifier (modifier sym &key (without-shift t))
  (lem:make-key :ctrl (modifier-ctrl modifier)
                :meta (modifier-meta modifier)
                :shift (if without-shift nil (modifier-shift modifier))
                :sym sym))
