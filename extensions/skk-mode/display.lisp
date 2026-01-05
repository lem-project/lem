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
           :show-skk-info))
(in-package :lem-skk-mode/display)

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
      ;; In henkan mode (before conversion)
      (henkan-mode-p
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
    (when display
      (message "~A" display))))

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
