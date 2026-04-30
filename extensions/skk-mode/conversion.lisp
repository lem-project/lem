(defpackage :lem-skk-mode/conversion
  (:use :cl :lem)
  (:import-from :lem-skk-mode/state
                :skk-state
                :skk-input-mode
                :skk-preedit
                :skk-henkan-mode-p
                :skk-henkan-start
                :skk-henkan-key
                :skk-okurigana-consonant
                :skk-okurigana-kana
                :skk-candidates
                :skk-candidate-index
                :get-skk-state
                :clear-skk-state
                :reset-skk-state)
  (:import-from :lem-skk-mode/romaji
                :romaji-to-hiragana
                :romaji-to-katakana
                :hiragana-to-katakana)
  (:import-from :lem-skk-mode/dictionary
                :lookup-candidates
                :load-skk-dictionary)
  (:export :process-skk-char
           :start-henkan
           :next-candidate
           :prev-candidate
           :commit-current
           :abort-henkan
           :toggle-kana-mode
           :set-input-mode
           :flush-preedit))
(in-package :lem-skk-mode/conversion)

(defun convert-romaji (romaji mode)
  "Convert ROMAJI to kana based on MODE (:hiragana or :katakana).
Returns two values: converted kana and remaining romaji."
  (if (eq mode :katakana)
      (romaji-to-katakana romaji)
      (romaji-to-hiragana romaji)))

(defun process-skk-char (char)
  "Process a single character input for SKK.
Returns :handled if the input was consumed, NIL otherwise."
  (let* ((state (get-skk-state))
         (mode (skk-input-mode state)))
    (case mode
      (:direct
       ;; Direct input - pass through
       nil)
      ((:hiragana :katakana)
       (process-kana-char state char mode)))))

(defun process-kana-char (state char mode)
  "Process a character in hiragana/katakana mode."
  (cond
    ;; Uppercase letter starts henkan mode (not already in henkan)
    ((and (alpha-char-p char)
          (upper-case-p char)
          (not (skk-henkan-mode-p state)))
     (start-henkan-mode state char)
     :handled)
    ;; Uppercase while showing candidates - commit current and start new henkan
    ((and (alpha-char-p char)
          (upper-case-p char)
          (skk-candidates state))
     ;; Commit current candidate
     (cleanup-henkan-point state)
     (clear-skk-state state)
     ;; Start new henkan mode
     (start-henkan-mode state char)
     :handled)
    ;; Lowercase while showing candidates - commit current and continue as normal input
    ((and (alpha-char-p char)
          (not (upper-case-p char))
          (skk-candidates state))
     ;; Commit current candidate
     (cleanup-henkan-point state)
     (clear-skk-state state)
     ;; Process as normal input (not in henkan mode now)
     (add-to-preedit state char mode)
     :handled)
    ;; Uppercase in henkan mode (no candidates, not in okurigana) - okurigana start
    ((and (alpha-char-p char)
          (upper-case-p char)
          (skk-henkan-mode-p state)
          (not (skk-okurigana-consonant state)))
     (handle-okurigana state char mode)
     :handled)
    ;; Uppercase in okurigana mode - continue okurigana input
    ((and (alpha-char-p char)
          (upper-case-p char)
          (skk-okurigana-consonant state))
     (add-to-preedit state char mode)
     :handled)
    ;; Normal alphabetic input - add to preedit
    ((alpha-char-p char)
     (add-to-preedit state char mode)
     :handled)
    ;; Non-alphabetic in henkan mode or with candidates - commit and pass through
    ((or (skk-henkan-mode-p state) (skk-candidates state))
     (if (skk-candidates state)
         (progn
           (cleanup-henkan-point state)
           (clear-skk-state state))
         (commit-reading state mode))
     nil)
    ;; Non-alphabetic in normal mode - flush preedit and pass through
    (t
     (flush-preedit state mode)
     nil)))

(defun start-henkan-mode (state char)
  "Start henkan mode with the first character."
  (setf (skk-henkan-mode-p state) t
        (skk-henkan-start state) (copy-point (current-point) :left-inserting)
        (skk-preedit state) (string (char-downcase char))
        (skk-henkan-key state) ""
        (skk-candidates state) nil
        (skk-candidate-index state) 0))

(defun handle-okurigana (state char mode)
  "Handle uppercase char as okurigana start in henkan mode.
For example, when typing 'KaKu' for 書く:
- After 'Ka', henkan-key = 'か'
- 'K' triggers this function, setting okurigana-consonant = 'k'"
  (declare (ignore mode))
  ;; First, flush any remaining preedit to henkan-key
  (flush-preedit state (skk-input-mode state))
  ;; Set okurigana consonant and start accumulating
  (let ((consonant (string (char-downcase char))))
    (setf (skk-okurigana-consonant state) consonant
          (skk-preedit state) consonant)))

(defun add-to-preedit (state char mode)
  "Add a character to preedit and convert if possible.
When in okurigana mode (okurigana-consonant is set), accumulates
converted kana to okurigana-kana instead of henkan-key."
  (let ((new-preedit (concatenate 'string
                                  (skk-preedit state)
                                  (string (char-downcase char)))))
    (multiple-value-bind (kana remaining)
        (convert-romaji new-preedit mode)
      (when (plusp (length kana))
        (cond
          ;; In okurigana mode - accumulate to okurigana-kana
          ((skk-okurigana-consonant state)
           (setf (skk-okurigana-kana state)
                 (concatenate 'string (skk-okurigana-kana state) kana)))
          ;; In henkan mode - accumulate to henkan-key
          ((skk-henkan-mode-p state)
           (setf (skk-henkan-key state)
                 (concatenate 'string (skk-henkan-key state) kana)))
          ;; Otherwise, insert directly
          (t
           (insert-string (current-point) kana))))
      (setf (skk-preedit state) remaining))))

(defun flush-preedit (state mode)
  "Flush any remaining preedit to the buffer.
Converts trailing 'n' to 'ん' on flush."
  (let ((preedit (skk-preedit state)))
    (when (plusp (length preedit))
      (multiple-value-bind (kana remaining)
          (convert-romaji preedit mode)
        ;; Convert remaining "n" to "ん"
        (when (equal remaining "n")
          (setf kana (concatenate 'string kana
                                  (if (eq mode :katakana) "ン" "ん"))
                remaining ""))
        (if (skk-henkan-mode-p state)
            (setf (skk-henkan-key state)
                  (concatenate 'string (skk-henkan-key state) kana))
            (when (plusp (length kana))
              (insert-string (current-point) kana)))
        (setf (skk-preedit state) remaining)))))

(defun start-henkan (state mode)
  "Start the conversion process, looking up candidates.
For okurigana entries, builds lookup key as 'reading + consonant' (e.g., 'かk')."
  ;; Flush preedit - goes to okurigana-kana if in okurigana mode, else henkan-key
  (flush-preedit-for-henkan state mode)
  (let* ((reading (skk-henkan-key state))
         (okurigana-consonant (skk-okurigana-consonant state))
         ;; Build lookup key: reading + okurigana consonant (if any)
         (lookup-key (if okurigana-consonant
                         (concatenate 'string reading okurigana-consonant)
                         reading))
         (candidates (lookup-candidates lookup-key)))
    (cond
      (candidates
       ;; Found candidates
       (setf (skk-candidates state) candidates
             (skk-candidate-index state) 0)
       (show-candidate state 0))
      (t
       ;; No candidates - just insert the reading as-is
       (commit-reading state mode)))))

(defun flush-preedit-for-henkan (state mode)
  "Flush preedit before henkan lookup.
If in okurigana mode, flushes to okurigana-kana. Otherwise to henkan-key."
  (let ((preedit (skk-preedit state)))
    (when (plusp (length preedit))
      (multiple-value-bind (kana remaining)
          (convert-romaji preedit mode)
        ;; Convert remaining "n" to "ん"
        (when (equal remaining "n")
          (setf kana (concatenate 'string kana
                                  (if (eq mode :katakana) "ン" "ん"))
                remaining ""))
        (when (plusp (length kana))
          (if (skk-okurigana-consonant state)
              ;; Okurigana mode - accumulate to okurigana-kana
              (setf (skk-okurigana-kana state)
                    (concatenate 'string (skk-okurigana-kana state) kana))
              ;; Normal mode - accumulate to henkan-key
              (setf (skk-henkan-key state)
                    (concatenate 'string (skk-henkan-key state) kana))))
        (setf (skk-preedit state) remaining)))))

(defun show-candidate (state index)
  "Display the candidate at INDEX.
Appends okurigana-kana to the candidate if present."
  (let ((candidates (skk-candidates state))
        (start (skk-henkan-start state)))
    (when (and candidates start (<= 0 index (1- (length candidates))))
      (let* ((candidate (nth index candidates))
             (okurigana (skk-okurigana-kana state))
             (text (if (plusp (length okurigana))
                       (concatenate 'string candidate okurigana)
                       candidate))
             ;; Save the start position as an integer before any operations
             (start-pos (lem:position-at-point start)))
        ;; Delete from henkan-start to current point
        (delete-between-points start (current-point))
        ;; Move current-point back to the saved start position for insertion
        (lem:move-to-position (current-point) start-pos)
        ;; Insert new candidate
        (insert-string (current-point) text)
        ;; IMPORTANT: Restore henkan-start to its original position AFTER insertion
        ;; This ensures it's ready for the next candidate change
        (lem:move-to-position start start-pos)))))

(defun next-candidate (state)
  "Show the next candidate."
  (let ((candidates (skk-candidates state))
        (index (skk-candidate-index state)))
    (when candidates
      (let ((new-index (mod (1+ index) (length candidates))))
        (setf (skk-candidate-index state) new-index)
        (show-candidate state new-index)))))

(defun prev-candidate (state)
  "Show the previous candidate."
  (let ((candidates (skk-candidates state))
        (index (skk-candidate-index state)))
    (when candidates
      (let ((new-index (mod (1- index) (length candidates))))
        (setf (skk-candidate-index state) new-index)
        (show-candidate state new-index)))))

(defun commit-current (state)
  "Commit the current conversion or preedit."
  (cond
    ;; If we have candidates displayed, commit current one
    ((skk-candidates state)
     (cleanup-henkan-point state)
     (clear-skk-state state))
    ;; If in henkan mode but no candidates, commit reading
    ((skk-henkan-mode-p state)
     (commit-reading state (skk-input-mode state)))
    ;; Otherwise, flush preedit and reset
    (t
     (flush-preedit state (skk-input-mode state))
     ;; Clear any remaining unconverted input on explicit commit
     (setf (skk-preedit state) ""))))

(defun commit-reading (state mode)
  "Commit the henkan-key reading as kana, including okurigana if present."
  (let* ((start (skk-henkan-start state))
         (reading (skk-henkan-key state))
         (okurigana (skk-okurigana-kana state))
         (full-reading (if (plusp (length okurigana))
                           (concatenate 'string reading okurigana)
                           reading))
         (text (if (eq mode :katakana)
                   (hiragana-to-katakana full-reading)
                   full-reading)))
    (when start
      (delete-between-points start (current-point))
      (insert-string start text)
      ;; Move cursor to end of inserted text before cleanup
      (move-point (current-point) start)
      (character-offset (current-point) (length text))
      (cleanup-henkan-point state))
    (clear-skk-state state)))

(defun abort-henkan (state)
  "Abort the current conversion, restoring original state."
  (let ((start (skk-henkan-start state)))
    (when start
      (delete-between-points start (current-point))
      (cleanup-henkan-point state)))
  (clear-skk-state state))

(defun cleanup-henkan-point (state)
  "Clean up the henkan start point marker."
  (when (skk-henkan-start state)
    (delete-point (skk-henkan-start state))
    (setf (skk-henkan-start state) nil)))

(defun toggle-kana-mode (state)
  "Toggle between hiragana and katakana mode."
  (let ((mode (skk-input-mode state)))
    (setf (skk-input-mode state)
          (case mode
            (:hiragana :katakana)
            (:katakana :hiragana)
            (t mode)))))

(defun set-input-mode (state mode)
  "Set the input mode to MODE (:hiragana, :katakana, or :direct)."
  (setf (skk-input-mode state) mode))
