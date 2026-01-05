(defpackage :lem-skk-mode/conversion
  (:use :cl :lem)
  (:import-from :lem-skk-mode/state
                :skk-state
                :skk-input-mode
                :skk-preedit
                :skk-henkan-mode-p
                :skk-henkan-start
                :skk-henkan-key
                :skk-okurigana
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
    ;; Uppercase in henkan mode (no candidates) - okurigana start
    ((and (alpha-char-p char)
          (upper-case-p char)
          (skk-henkan-mode-p state))
     (handle-okurigana state char mode)
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
  "Handle uppercase char as okurigana start in henkan mode."
  ;; First, convert current preedit to henkan-key
  (let ((preedit (skk-preedit state)))
    (when (plusp (length preedit))
      (multiple-value-bind (kana remaining)
          (convert-romaji preedit mode)
        (setf (skk-henkan-key state)
              (concatenate 'string (skk-henkan-key state) kana)
              (skk-preedit state) remaining))))
  ;; Set okurigana start
  (setf (skk-okurigana state) (char-downcase char)
        (skk-preedit state) (string (char-downcase char))))

(defun add-to-preedit (state char mode)
  "Add a character to preedit and convert if possible."
  (let ((new-preedit (concatenate 'string
                                  (skk-preedit state)
                                  (string (char-downcase char)))))
    (multiple-value-bind (kana remaining)
        (convert-romaji new-preedit mode)
      (when (plusp (length kana))
        (if (skk-henkan-mode-p state)
            ;; In henkan mode, accumulate in henkan-key
            (setf (skk-henkan-key state)
                  (concatenate 'string (skk-henkan-key state) kana))
            ;; Otherwise, insert directly
            (insert-string (current-point) kana)))
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
  "Start the conversion process, looking up candidates."
  ;; First flush preedit to henkan-key
  (flush-preedit state mode)
  (let* ((reading (skk-henkan-key state))
         (candidates (lookup-candidates reading)))
    (cond
      (candidates
       ;; Found candidates
       (setf (skk-candidates state) candidates
             (skk-candidate-index state) 0)
       (show-candidate state 0))
      (t
       ;; No candidates - just insert the reading as-is
       (commit-reading state mode)))))

(defun show-candidate (state index)
  "Display the candidate at INDEX."
  (let ((candidates (skk-candidates state))
        (start (skk-henkan-start state)))
    (when (and candidates start (<= 0 index (1- (length candidates))))
      ;; Delete from henkan-start to current point
      (delete-between-points start (current-point))
      ;; Insert the candidate
      (let ((candidate (nth index candidates)))
        (insert-string start candidate)
        ;; If there's okurigana, add it
        (when (skk-okurigana state)
          ;; The okurigana should be converted and added
          ;; For now, just add the preedit remainder
          )))))

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
  "Commit the henkan-key reading as kana."
  (let ((start (skk-henkan-start state))
        (reading (skk-henkan-key state)))
    (when start
      (delete-between-points start (current-point))
      (insert-string start
                     (if (eq mode :katakana)
                         (hiragana-to-katakana reading)
                         reading))
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
