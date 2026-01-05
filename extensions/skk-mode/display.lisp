(defpackage :lem-skk-mode/display
  (:use :cl :lem)
  (:import-from :lem-skk-mode/state
                :skk-state
                :skk-input-mode
                :skk-preedit
                :skk-henkan-mode-p
                :skk-henkan-key
                :skk-candidates
                :skk-candidate-index
                :get-skk-state)
  (:export :update-skk-display
           :clear-skk-display
           :skk-modeline-string
           :show-skk-info
           :update-skk-cursor
           :restore-default-cursor
           :save-default-cursor-color))
(in-package :lem-skk-mode/display)

;;; Cursor colors for different modes
(defvar *skk-cursor-color-hiragana* "green"
  "Cursor color for hiragana mode.")

(defvar *skk-cursor-color-katakana* "cyan"
  "Cursor color for katakana mode.")

(defvar *skk-cursor-color-direct* nil
  "Cursor color for direct (latin) mode. NIL means default.")

(defvar *skk-cursor-color-henkan* "yellow"
  "Cursor color during henkan (conversion) mode.")

(defvar *default-cursor-color* nil
  "Saved default cursor color to restore when SKK is disabled.")

(defun mode-indicator (mode)
  "Return mode indicator string for MODE."
  (case mode
    (:hiragana "あ")
    (:katakana "ア")
    (:direct "--")
    (t "??")))

(defun skk-modeline-string ()
  "Return SKK status string for modeline display."
  (let ((state (get-skk-state)))
    (if state
        (format nil "[SKK:~A]" (mode-indicator (skk-input-mode state)))
        "")))

(defun build-preedit-display (state)
  "Build the preedit display string."
  (let ((henkan-mode-p (skk-henkan-mode-p state))
        (henkan-key (skk-henkan-key state))
        (preedit (skk-preedit state))
        (candidates (skk-candidates state)))
    (cond
      ;; Showing candidates
      (candidates
       (let ((index (skk-candidate-index state))
             (total (length candidates)))
         (format nil "▼~A [~D/~D]"
                 (nth index candidates)
                 (1+ index)
                 total)))
      ;; In henkan mode (before conversion) - only show if there's content
      ((and henkan-mode-p
            (or (plusp (length henkan-key))
                (plusp (length preedit))))
       (format nil "▽~A~A" henkan-key preedit))
      ;; Normal preedit
      ((plusp (length preedit))
       preedit)
      (t
       nil))))

(defun update-skk-display ()
  "Update SKK display in echo area."
  (let* ((state (get-skk-state))
         (display (build-preedit-display state)))
    (if display
        (message "~A" display)
        (message nil))))

(defun clear-skk-display ()
  "Clear SKK display from echo area."
  (message nil))

(defun show-skk-info ()
  "Show detailed SKK state information (for debugging)."
  (let ((state (get-skk-state)))
    (message "SKK: mode=~A henkan=~A key=~S preedit=~S cands=~D"
             (skk-input-mode state)
             (skk-henkan-mode-p state)
             (skk-henkan-key state)
             (skk-preedit state)
             (length (skk-candidates state)))))

;;; Cursor management

(defun save-default-cursor-color ()
  "Save the current cursor color as default."
  (unless *default-cursor-color*
    (let ((attr (lem-core::ensure-attribute 'cursor nil)))
      (when attr
        (setf *default-cursor-color*
              (or (attribute-background attr) "white"))))))

(defun update-skk-cursor ()
  "Update cursor shape and color based on current SKK state."
  (let* ((state (get-skk-state))
         (mode (skk-input-mode state))
         (henkan-p (skk-henkan-mode-p state))
         (candidates-p (skk-candidates state)))
    (cond
      ;; During candidate selection - underline cursor, yellow
      (candidates-p
       (set-attribute 'cursor :background *skk-cursor-color-henkan*)
       (lem-if:update-cursor-shape (lem:implementation) :underline))
      ;; In henkan mode (before conversion) - underline cursor, yellow
      (henkan-p
       (set-attribute 'cursor :background *skk-cursor-color-henkan*)
       (lem-if:update-cursor-shape (lem:implementation) :underline))
      ;; Direct/latin mode - bar cursor, default color
      ((eq mode :direct)
       (set-attribute 'cursor :background (or *skk-cursor-color-direct*
                                              *default-cursor-color*
                                              "white"))
       (lem-if:update-cursor-shape (lem:implementation) :bar))
      ;; Hiragana mode - box cursor, green
      ((eq mode :hiragana)
       (set-attribute 'cursor :background *skk-cursor-color-hiragana*)
       (lem-if:update-cursor-shape (lem:implementation) :box))
      ;; Katakana mode - box cursor, cyan
      ((eq mode :katakana)
       (set-attribute 'cursor :background *skk-cursor-color-katakana*)
       (lem-if:update-cursor-shape (lem:implementation) :box))
      ;; Default
      (t
       (set-attribute 'cursor :background (or *default-cursor-color* "white"))
       (lem-if:update-cursor-shape (lem:implementation) :box)))))

(defun restore-default-cursor ()
  "Restore cursor to default state."
  (set-attribute 'cursor :background (or *default-cursor-color* "white"))
  (lem-if:update-cursor-shape (lem:implementation) :box))
