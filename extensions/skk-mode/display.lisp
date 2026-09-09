(defpackage :lem-skk-mode/display
  (:use :cl :lem)
  (:import-from :lem-skk-mode/state
                :skk-state
                :skk-input-mode
                :skk-preedit
                :skk-henkan-mode-p
                :skk-henkan-key
                :skk-henkan-start
                :skk-okurigana-consonant
                :skk-okurigana-kana
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

;;; Preedit display configuration
(defparameter *skk-preedit-display-style* :floating-window
  "Style for SKK preedit display.
:floating-window - Display at cursor position using floating-window (default)
:echo-area - Display in echo area (legacy behavior)")

(defparameter *skk-preedit-background-color* nil
  "Background color for floating-window preedit display.
NIL means use transparent/default buffer background.")

(defparameter *skk-preedit-offset-y* 0
  "Vertical offset for preedit window from cursor position.
Negative values move the window up, positive values move it down.")

;;; Preedit window state
(defvar *skk-preedit-window* nil
  "Current SKK preedit floating window, or NIL.")

(defvar *skk-preedit-buffer* nil
  "Buffer used for SKK preedit display.")

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
  "Build the preedit display string.
Shows okurigana with * marker, e.g., '▽か*く' for 書く."
  (let ((henkan-mode-p (skk-henkan-mode-p state))
        (henkan-key (skk-henkan-key state))
        (preedit (skk-preedit state))
        (candidates (skk-candidates state))
        (okurigana-consonant (skk-okurigana-consonant state))
        (okurigana-kana (skk-okurigana-kana state)))
    (cond
      ;; Showing candidates - include okurigana if present
      (candidates
       (let* ((index (skk-candidate-index state))
              (total (length candidates))
              (candidate (nth index candidates))
              (display-text (if (plusp (length okurigana-kana))
                                (concatenate 'string candidate okurigana-kana)
                                candidate)))
         (format nil "▼~A [~D/~D]"
                 display-text
                 (1+ index)
                 total)))
      ;; In okurigana mode - show with * marker
      ((and henkan-mode-p okurigana-consonant)
       (format nil "▽~A*~A~A" henkan-key okurigana-kana preedit))
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

;;; Preedit floating window management

(defun ensure-preedit-buffer ()
  "Get or create the preedit display buffer."
  (or *skk-preedit-buffer*
      (let ((buffer (make-buffer "*SKK Preedit*" :temporary t :enable-undo-p nil)))
        (setf (variable-value 'line-wrap :buffer buffer) nil)
        (setf *skk-preedit-buffer* buffer))))

(defun delete-preedit-window ()
  "Delete the current preedit floating window if it exists."
  (delete-popup-message *skk-preedit-window*)
  (setf *skk-preedit-window* nil))

(defun create-preedit-window (text)
  "Create a new floating window displaying TEXT at the henkan-start position."
  (let* ((state (get-skk-state))
         (henkan-start (skk-henkan-start state))
         (buffer (ensure-preedit-buffer))
         ;; Calculate offset-x to position at henkan-start instead of cursor
         (offset-x (if henkan-start
                       (- (point-column henkan-start) (point-column (current-point)))
                       0)))
    ;; Update buffer content
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    ;; Create floating window using public API
    (display-popup-message buffer
                           :timeout nil
                           :style (list :gravity :follow-cursor
                                        :use-border nil
                                        :offset-x offset-x
                                        :offset-y *skk-preedit-offset-y*
                                        :background-color *skk-preedit-background-color*
                                        :cursor-invisible t))))

(defun update-preedit-window (text)
  "Update the preedit window with new TEXT, creating if needed."
  (delete-preedit-window)
  (when (and text (plusp (length text)))
    (setf *skk-preedit-window* (create-preedit-window text))))

(defun update-skk-display ()
  "Update SKK display based on current display style."
  (let* ((state (get-skk-state))
         (display (build-preedit-display state)))
    (case *skk-preedit-display-style*
      (:floating-window
       (update-preedit-window display))
      (:echo-area
       (if display
           (message "~A" display)
           (message nil)))
      (t
       ;; Default to floating-window
       (update-preedit-window display)))))

(defun clear-skk-display ()
  "Clear SKK display."
  (case *skk-preedit-display-style*
    (:floating-window
     (delete-preedit-window))
    (:echo-area
     (message nil))
    (t
     (delete-preedit-window)
     (message nil))))

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
