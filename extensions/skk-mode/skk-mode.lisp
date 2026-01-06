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
           :*skk-mode-keymap*
           :skk-toggle-debug
           :skk-debug-state
           :skk-test-dekiru))
(in-package :lem-skk-mode)

;;; Debug logging
(defvar *skk-debug-log* nil
  "When non-nil, log SKK operations to *SKK Debug Log* buffer.")

(defun skk-debug (format-string &rest args)
  "Log debug message to *SKK Debug Log* buffer if debugging is enabled."
  (when *skk-debug-log*
    (let ((buffer (make-buffer "*SKK Debug Log*")))
      (with-open-stream (stream (make-buffer-output-stream
                                 (buffer-end-point buffer)))
        (apply #'format stream format-string args)
        (terpri stream)))))

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
  (skk-debug "skk-enable: dictionary-loaded=~A test-lookup=~S"
             (lem-skk-mode/dictionary:dictionary-loaded-p)
             (lem-skk-mode/dictionary:lookup-candidates "できr"))
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
    (skk-debug "self-insert: char=~S keys=~S" char keys)
    (when char
      (let* ((state (get-skk-state))
             (handled (process-skk-char char)))
        (skk-debug "  after-process: handled=~A henkan-mode=~A henkan-key=~S okuri-c=~S okuri-k=~S preedit=~S"
                   handled
                   (skk-henkan-mode-p state)
                   (lem-skk-mode/state:skk-henkan-key state)
                   (lem-skk-mode/state:skk-okurigana-consonant state)
                   (lem-skk-mode/state:skk-okurigana-kana state)
                   (skk-preedit state))
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
  "Commit the current conversion and insert a newline.
If in henkan mode without candidates, first try to convert, then commit."
  (let ((state (get-skk-state)))
    (skk-debug "Return: henkan-mode=~A candidates=~S henkan-key=~S okuri-c=~S okuri-k=~S"
               (skk-henkan-mode-p state)
               (skk-candidates state)
               (lem-skk-mode/state:skk-henkan-key state)
               (lem-skk-mode/state:skk-okurigana-consonant state)
               (lem-skk-mode/state:skk-okurigana-kana state))
    (cond
      ;; Already showing candidates - just commit
      ((skk-candidates state)
       (skk-debug "  Return: committing existing candidates")
       (commit-current state)
       (clear-skk-display)
       (update-skk-cursor))
      ;; In henkan mode but no candidates yet - try to convert first
      ((skk-henkan-mode-p state)
       (skk-debug "  Return: in henkan mode, calling start-henkan")
       (start-henkan state (skk-input-mode state))
       (skk-debug "  Return: after start-henkan, candidates=~S" (skk-candidates state))
       ;; If we got candidates, commit them; otherwise reading was already committed
       (when (skk-candidates state)
         (commit-current state))
       (clear-skk-display)
       (update-skk-cursor))
      ;; Normal mode - just newline
      (t
       (skk-debug "  Return: normal mode, inserting newline")
       (newline)))))

(define-command skk-toggle-debug () ()
  "Toggle SKK debug logging."
  (setf *skk-debug-log* (not *skk-debug-log*))
  (if *skk-debug-log*
      (progn
        (make-buffer "*SKK Debug Log*")
        (message "SKK debug logging enabled. Check *SKK Debug Log* buffer."))
      (message "SKK debug logging disabled.")))

(define-command skk-debug-state () ()
  "Show current SKK state for debugging."
  (let ((state (get-skk-state)))
    (with-pop-up-typeout-window (stream (make-buffer "*SKK Debug*") :erase t)
      (format stream "=== SKK Debug State ===~%")
      (format stream "input-mode: ~A~%" (skk-input-mode state))
      (format stream "henkan-mode-p: ~A~%" (skk-henkan-mode-p state))
      (format stream "henkan-start: ~A~%" (lem-skk-mode/state:skk-henkan-start state))
      (format stream "henkan-key: ~S~%" (lem-skk-mode/state:skk-henkan-key state))
      (format stream "preedit: ~S~%" (skk-preedit state))
      (format stream "okurigana-consonant: ~S~%" (lem-skk-mode/state:skk-okurigana-consonant state))
      (format stream "okurigana-kana: ~S~%" (lem-skk-mode/state:skk-okurigana-kana state))
      (format stream "candidates: ~S~%" (skk-candidates state))
      (format stream "candidate-index: ~D~%" (lem-skk-mode/state:skk-candidate-index state))
      (format stream "~%=== Dictionary Test ===~%")
      (format stream "dictionary-loaded-p: ~A~%" (lem-skk-mode/dictionary:dictionary-loaded-p))
      (format stream "lookup 'できr': ~S~%" (lem-skk-mode/dictionary:lookup-candidates "できr"))
      (format stream "lookup 'かk': ~S~%" (lem-skk-mode/dictionary:lookup-candidates "かk")))))

(define-command skk-test-dekiru () ()
  "Test 'DekiRu' conversion. Run in a buffer where you want the result."
  ;; Save current buffer
  (let ((original-buffer (current-buffer)))
    ;; Create temporary test buffer
    (let ((test-buf (make-buffer "*skk-test-buf*" :temporary t)))
      (switch-to-buffer test-buf)
      (erase-buffer test-buf)

      ;; Ensure SKK mode is enabled
      (unless (skk-mode)
        (skk-mode t))
      (let ((state (get-skk-state)))
        ;; Reset state
        (reset-skk-state)

        ;; Process DekiRu character by character
        (process-skk-char #\D)
        (process-skk-char #\e)
        (process-skk-char #\k)
        (process-skk-char #\i)
        (process-skk-char #\R)
        (process-skk-char #\u)

        ;; Show state in pop-up window
        (with-pop-up-typeout-window (stream (make-buffer "*SKK Test Result*") :erase t)
          (format stream "=== SKK DekiRu Test ===~%~%")
          (format stream "State after typing 'DekiRu':~%")
          (format stream "  henkan-mode-p: ~A~%" (skk-henkan-mode-p state))
          (format stream "  henkan-key: ~S~%" (lem-skk-mode/state:skk-henkan-key state))
          (format stream "  okurigana-consonant: ~S~%" (lem-skk-mode/state:skk-okurigana-consonant state))
          (format stream "  okurigana-kana: ~S~%" (lem-skk-mode/state:skk-okurigana-kana state))
          (format stream "  preedit: ~S~%" (skk-preedit state))
          (format stream "~%")

          ;; Build lookup key
          (let* ((lookup-key (concatenate 'string
                                          (lem-skk-mode/state:skk-henkan-key state)
                                          (or (lem-skk-mode/state:skk-okurigana-consonant state) "")))
                 (candidates (lem-skk-mode/dictionary:lookup-candidates lookup-key)))
            (format stream "Dictionary lookup:~%")
            (format stream "  lookup-key: ~S~%" lookup-key)
            (format stream "  candidates: ~S~%" candidates)
            (format stream "~%")

            ;; Now call start-henkan (like Return)
            (format stream "Calling start-henkan (simulating Return):~%")
            (start-henkan state (skk-input-mode state))
            (format stream "  candidates in state: ~S~%" (skk-candidates state))
            (format stream "~%")

            ;; Show buffer content
            (format stream "Buffer content after conversion:~%")
            (format stream "  ~S~%" (buffer-text test-buf))
            (format stream "~%")

            (if (skk-candidates state)
                (format stream "SUCCESS: Conversion completed!~%")
                (format stream "NOTE: No candidates found, reading was inserted as-is.~%"))))

        ;; Cleanup
        (reset-skk-state)
        (kill-buffer test-buf)
        (switch-to-buffer original-buffer)))))

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
