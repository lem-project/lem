(defpackage :lem-skk-mode
  (:use :cl :lem)
  (:import-from :lem-skk-mode/state
                :get-skk-state
                :reset-skk-state
                :skk-input-mode
                :skk-preedit
                :skk-henkan-mode-p
                :skk-candidates)
  (:import-from :lem-skk-mode/conversion
                :process-skk-char
                :start-henkan
                :next-candidate
                :prev-candidate
                :commit-current
                :abort-henkan
                :toggle-kana-mode
                :set-input-mode
                :flush-preedit)
  (:import-from :lem-skk-mode/display
                :update-skk-display
                :clear-skk-display
                :skk-modeline-string
                :update-skk-cursor
                :restore-default-cursor
                :save-default-cursor-color)
  (:import-from :lem-skk-mode/dictionary
                :load-skk-dictionary)
  (:export :skk-mode
           :*skk-mode-keymap*))
(in-package :lem-skk-mode)

;;; Keymap

(defvar *skk-mode-keymap*
  (make-keymap :name '*skk-mode-keymap*)
  "Keymap for SKK mode. Binds printable characters for Japanese input.")

;; Bind all lowercase letters to skk-self-insert
(loop :for c :from (char-code #\a) :to (char-code #\z)
      :for char := (string (code-char c))
      :do (define-key *skk-mode-keymap* char 'skk-self-insert))

;; Bind all uppercase letters to skk-self-insert
(loop :for c :from (char-code #\A) :to (char-code #\Z)
      :for char := (string (code-char c))
      :do (define-key *skk-mode-keymap* char 'skk-self-insert))

;; Bind digits
(loop :for c :from (char-code #\0) :to (char-code #\9)
      :for char := (string (code-char c))
      :do (define-key *skk-mode-keymap* char 'skk-self-insert))

;; Bind punctuation that SKK uses
(dolist (char '("-" "." "," "[" "]" "/" "'" "\"" ";" ":"))
  (define-key *skk-mode-keymap* char 'skk-self-insert))

;; Define specific SKK key bindings (these override the above)
(define-keys *skk-mode-keymap*
  ("q"       'skk-toggle-kana)
  ("l"       'skk-latin-mode)
  ("L"       'skk-direct-mode)
  ("C-j"     'skk-kakutei)
  ("C-g"     'skk-cancel)
  ("Space"   'skk-henkan-or-space)
  ("x"       'skk-prev-candidate)
  ("Return"  'skk-kakutei-and-newline)
  ("Backspace" 'skk-delete-backward))

;;; Minor Mode Definition

(define-minor-mode skk-mode
    (:name "SKK"
     :description "SKK Japanese input method"
     :keymap *skk-mode-keymap*
     :global t
     :enable-hook 'skk-enable
     :disable-hook 'skk-disable))

(defun skk-enable ()
  "Called when SKK mode is enabled."
  (load-skk-dictionary)
  (save-default-cursor-color)
  (reset-skk-state)
  (update-skk-display)
  (update-skk-cursor)
  (message "SKK: hiragana mode"))

(defun skk-disable ()
  "Called when SKK mode is disabled."
  (clear-skk-display)
  (reset-skk-state)
  (restore-default-cursor))

;;; Commands

(define-command skk-self-insert () ()
  "Handle character input in SKK mode."
  (let* ((keys (last-read-key-sequence))
         (char (insertion-key-p keys)))
    (when char
      (let ((handled (process-skk-char char)))
        (update-skk-display)
        (update-skk-cursor)
        (unless handled
          ;; Not handled by SKK - insert normally
          (insert-character (current-point) char))))))

(define-command skk-toggle-kana () ()
  "Toggle between hiragana and katakana mode."
  (let ((state (get-skk-state)))
    (cond
      ;; In direct mode - insert 'q' normally
      ((eq (skk-input-mode state) :direct)
       (insert-character (current-point) #\q))
      (t
       ;; If in henkan mode with preedit, convert it first
       (when (and (skk-henkan-mode-p state)
                  (plusp (length (skk-preedit state))))
         (flush-preedit state (skk-input-mode state)))
       (toggle-kana-mode state)
       (update-skk-display)
       (update-skk-cursor)
       (message "SKK: ~A mode" (case (skk-input-mode state)
                                 (:hiragana "hiragana")
                                 (:katakana "katakana")
                                 (t "?")))))))

(define-command skk-latin-mode () ()
  "Switch to latin (direct ASCII) input mode."
  (let ((state (get-skk-state)))
    (cond
      ;; Already in direct mode - insert 'l' normally
      ((eq (skk-input-mode state) :direct)
       (insert-character (current-point) #\l))
      (t
       (commit-current state)
       (set-input-mode state :direct)
       (update-skk-display)
       (update-skk-cursor)
       (message "SKK: latin mode")))))

(define-command skk-direct-mode () ()
  "Switch to direct mode (disable SKK input)."
  (let ((state (get-skk-state)))
    (cond
      ;; Already in direct mode - insert 'L' normally
      ((eq (skk-input-mode state) :direct)
       (insert-character (current-point) #\L))
      (t
       (commit-current state)
       (set-input-mode state :direct)
       (update-skk-display)
       (update-skk-cursor)
       (message "SKK: direct mode")))))

(define-command skk-kakutei () ()
  "Commit the current conversion or input."
  (let ((state (get-skk-state)))
    (commit-current state)
    ;; Return to hiragana mode if was in direct mode from 'l'
    (when (eq (skk-input-mode state) :direct)
      (set-input-mode state :hiragana))
    (clear-skk-display)
    (update-skk-cursor)
    (message "SKK: ~A mode" (case (skk-input-mode state)
                              (:hiragana "hiragana")
                              (:katakana "katakana")
                              (:direct "direct")
                              (t "?")))))

(define-command skk-cancel () ()
  "Cancel the current conversion."
  (let ((state (get-skk-state)))
    (if (or (skk-henkan-mode-p state) (skk-candidates state))
        (progn
          (abort-henkan state)
          (clear-skk-display)
          (update-skk-cursor)
          (message "SKK: cancelled"))
        ;; Not in henkan mode, pass through to normal C-g
        (keyboard-quit))))

(define-command skk-henkan-or-space () ()
  "Start conversion or cycle candidates, or insert space."
  (let ((state (get-skk-state)))
    (cond
      ;; Already showing candidates - next candidate
      ((skk-candidates state)
       (next-candidate state)
       (update-skk-display)
       (update-skk-cursor))
      ;; In henkan mode - start conversion
      ((skk-henkan-mode-p state)
       (start-henkan state (skk-input-mode state))
       (update-skk-display)
       (update-skk-cursor))
      ;; Direct mode - insert space
      ((eq (skk-input-mode state) :direct)
       (insert-character (current-point) #\Space))
      ;; Normal kana mode - insert full-width space
      (t
       (insert-string (current-point) "　")))))

(define-command skk-prev-candidate () ()
  "Show the previous conversion candidate."
  (let ((state (get-skk-state)))
    (cond
      ;; Has candidates - show previous
      ((skk-candidates state)
       (prev-candidate state)
       (update-skk-display)
       (update-skk-cursor))
      ;; In direct mode - insert 'x' normally
      ((eq (skk-input-mode state) :direct)
       (insert-character (current-point) #\x))
      ;; In kana mode - process as romaji
      (t
       (let ((handled (process-skk-char #\x)))
         (update-skk-display)
         (update-skk-cursor)
         (unless handled
           (insert-character (current-point) #\x)))))))

(define-command skk-kakutei-and-newline () ()
  "Commit the current conversion and insert a newline."
  (let ((state (get-skk-state)))
    (if (or (skk-henkan-mode-p state) (skk-candidates state))
        (progn
          (commit-current state)
          (clear-skk-display)
          (update-skk-cursor))
        (newline))))

(define-command skk-delete-backward () ()
  "Delete backward in SKK mode."
  (let ((state (get-skk-state)))
    (cond
      ;; Has preedit - remove last char from preedit
      ((plusp (length (skk-preedit state)))
       (setf (skk-preedit state)
             (subseq (skk-preedit state) 0 (1- (length (skk-preedit state)))))
       (update-skk-display)
       (update-skk-cursor))
      ;; In henkan mode with henkan-key
      ((and (skk-henkan-mode-p state)
            (plusp (length (lem-skk-mode/state:skk-henkan-key state))))
       ;; Remove last char from henkan-key
       (let ((key (lem-skk-mode/state:skk-henkan-key state)))
         (setf (lem-skk-mode/state:skk-henkan-key state)
               (subseq key 0 (1- (length key)))))
       (update-skk-display)
       (update-skk-cursor))
      ;; Has candidates - cancel conversion
      ((skk-candidates state)
       (abort-henkan state)
       (clear-skk-display)
       (update-skk-cursor))
      ;; Otherwise, normal backspace
      (t
       (delete-previous-char)))))
