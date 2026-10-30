(defpackage :lem-spell-check
  (:use :cl :lem)
  (:export
   :spell-check-mode
   :spell-check-buffer
   :spell-check-word
   :spell-check-next-error
   :spell-check-previous-error
   :spell-check-add-word
   :spell-check-ignore-word
   :spell-check-change-dictionary
   :spell-check-clear
   :spell-check-available-dictionaries
   :spell-check-language
   :spell-check-delay
   :spell-check-min-word-length
   :parse-words))
(in-package :lem-spell-check)

;;; Configuration

(define-editor-variable spell-check-language nil
  "Language code of the Enchant dictionary (e.g. \"en_US\").
NIL detects the language from LC_ALL, LC_MESSAGES or LANG and falls
back to the first available dictionary. Set this buffer-locally to use
a different dictionary per buffer.")

(define-editor-variable spell-check-delay 400
  "Idle delay in milliseconds before rescanning a buffer after an edit.")

(define-editor-variable spell-check-min-word-length 2
  "Minimum length of words that are spell checked.")

(defvar *spell-check-broker* nil
  "The shared Enchant broker. All Enchant calls run on the editor thread.")

(defvar *spell-check-dictionaries* nil
  "Hash table mapping language codes to cached Enchant dictionaries.")

(defvar *spell-check-available-dictionaries* nil
  "Cached list of language codes that Enchant can provide.")

;;; Appearance

(define-attribute spell-check-misspelled-attribute
  (t :underline "red" :underline-style :curly))

;;; Minor mode

(define-minor-mode spell-check-mode
    (:name "Spell"
     :description "Highlight misspelled words using Enchant"
     :keymap *spell-check-mode-keymap*
     :enable-hook 'spell-check-enable
     :disable-hook 'spell-check-disable))

(define-keys *spell-check-mode-keymap*
  ("M-$" 'spell-check-word))

(setf (documentation 'spell-check-mode 'function)
      "Toggle buffer-local spell checking.

While enabled, misspelled words are underlined with a curly red
underline. The buffer is rescanned after edits stop. Use
`spell-check-word' (M-$) to correct the word at point.")

;;; Word parsing

(defun word-internal-char-p (char)
  "Return T if CHAR can appear inside a word."
  (or (char= char #\')
      (char= char #\’)
      (char= char #\-)))

(defun parse-words (line)
  "Return a list of (START END WORD) for the words in LINE.
START is inclusive, END is exclusive, and both are character offsets
from the beginning of LINE. Apostrophes and hyphens are part of a word
only when followed by an alphanumeric character."
  (let ((length (length line))
        (words '()))
    (loop :with i := 0
          :while (< i length)
          :do (let ((char (char line i)))
                (if (alphanumericp char)
                    (let ((start i))
                      (incf i)
                      (loop :while (< i length)
                            :do (let ((char (char line i)))
                                  (cond ((alphanumericp char)
                                         (incf i))
                                        ((and (word-internal-char-p char)
                                              (< (1+ i) length)
                                              (alphanumericp (char line (1+ i))))
                                         (incf i 2))
                                        (t
                                         (return)))))
                      (push (list start i (subseq line start i)) words))
                    (incf i)))
          :finally (return (nreverse words)))))

(defun spell-checkable-word-p (word min-length)
  "Return T if WORD should be looked up in the dictionary."
  (and (>= (length word) min-length)
       (some #'alpha-char-p word)
       (notany #'digit-char-p word)))

;;; Dictionaries

(defun spell-check-broker ()
  "Return an initialized Enchant broker, creating one if necessary."
  (let ((broker *spell-check-broker*))
    (if (and broker (enchant:activep broker))
        broker
        (let ((broker (enchant:broker-init)))
          (unless broker
            (editor-error "Failed to initialize the Enchant spell checker"))
          (setf *spell-check-broker* broker
                *spell-check-dictionaries* (make-hash-table :test #'equal)
                *spell-check-available-dictionaries* nil)
          broker))))

(defun spell-check-available-dictionaries ()
  "Return the sorted list of language codes available to Enchant."
  (or *spell-check-available-dictionaries*
      (setf *spell-check-available-dictionaries*
            (sort (remove-duplicates
                   (mapcar #'first
                           (enchant:broker-list-dicts (spell-check-broker)))
                   :test #'string=)
                  #'string<))))

(defun spell-check-normalize-language (language)
  "Normalize LANGUAGE for use as an Enchant language code.
Strip encoding and modifier suffixes, and return NIL for the C and
POSIX locales."
  (let* ((language (string-trim '(#\space #\tab #\newline) language))
         (dot (position #\. language))
         (at (position #\@ language))
         (end (cond ((and dot at) (min dot at))
                    (dot dot)
                    (at at)
                    (t (length language))))
         (language (subseq language 0 end)))
    (unless (or (zerop (length language))
                (string-equal language "C")
                (string-equal language "POSIX"))
      (substitute #\_ #\- language))))

(defun spell-check-environment-language ()
  "Return a language code derived from the process locale, if any."
  (some (lambda (variable)
          (let ((value (uiop:getenv variable)))
            (and value (spell-check-normalize-language value))))
        '("LC_ALL" "LC_MESSAGES" "LANG")))

(defun spell-check-find-language (broker language)
  "Return the canonical dictionary language matching LANGUAGE, or NIL."
  (and language
       (or (enchant:broker-dict-exists-p broker language)
           (find language (spell-check-available-dictionaries)
                 :test #'string-equal))))

(defun spell-check-auto-language (broker)
  "Return the language to use when `spell-check-language' is unset."
  (let* ((locale (spell-check-environment-language))
         (base (and locale
                    (subseq locale 0 (or (position #\_ locale)
                                         (length locale))))))
    (or (spell-check-find-language broker locale)
        (spell-check-find-language broker base)
        (first (spell-check-available-dictionaries))
        (editor-error "No Enchant dictionaries are available"))))

(defun spell-check-language-for-buffer (&optional (buffer (current-buffer)))
  "Return the Enchant language code to use for BUFFER."
  (let* ((broker (spell-check-broker))
         (specified (variable-value 'spell-check-language :default buffer)))
    (cond
      ((null specified)
       (spell-check-auto-language broker))
      ((not (stringp specified))
       (editor-error "spell-check-language must be a string, got ~S"
                     specified))
      ((zerop (length specified))
       (spell-check-auto-language broker))
      (t
       (let ((language (spell-check-normalize-language specified)))
         (or (spell-check-find-language broker language)
             (editor-error "No Enchant dictionary for language ~S"
                           specified)))))))

(defun spell-check-dictionary-for-language (language)
  "Return a cached Enchant dictionary for LANGUAGE."
  (let* ((broker (spell-check-broker))
         (dictionary (gethash language *spell-check-dictionaries*)))
    (unless (and dictionary (enchant:activep dictionary))
      (handler-case
          (setf (gethash language *spell-check-dictionaries*)
                (enchant:broker-request-dict broker language))
        (enchant:enchant-error (condition)
          (editor-error "Cannot use dictionary ~S: ~A"
                        language (enchant:error-string condition)))))
    (gethash language *spell-check-dictionaries*)))

(defun spell-check-dictionary-for-buffer (buffer)
  "Return the Enchant dictionary used for BUFFER."
  (spell-check-dictionary-for-language
   (spell-check-language-for-buffer buffer)))

(defun spell-check-suggestions (dictionary word &key limit)
  "Return spelling suggestions for WORD, optionally limited to LIMIT."
  (let ((suggestions
          (handler-case
              (enchant:dict-suggest dictionary word)
            (enchant:enchant-error ()
              nil))))
    (if (and limit (< limit (length suggestions)))
        (subseq suggestions 0 limit)
        suggestions)))

;;; Buffer state

(defun spell-check-overlays (buffer)
  "Return the spell-check overlays registered for BUFFER."
  (buffer-value buffer 'spell-check-overlays))

(defun (setf spell-check-overlays) (overlays buffer)
  (setf (buffer-value buffer 'spell-check-overlays) overlays))

(defun spell-check-scanned-tick (buffer)
  "Return the modification tick at which BUFFER was last scanned."
  (buffer-value buffer 'spell-check-scanned-tick))

(defun (setf spell-check-scanned-tick) (tick buffer)
  (setf (buffer-value buffer 'spell-check-scanned-tick) tick))

(defun spell-check-timer (buffer)
  "Return BUFFER's pending rescan timer."
  (buffer-value buffer 'spell-check-timer))

(defun (setf spell-check-timer) (timer buffer)
  (setf (buffer-value buffer 'spell-check-timer) timer))

(defun spell-check-clear-overlays (buffer)
  "Delete every overlay owned by spell checking in BUFFER."
  (dolist (overlay (spell-check-overlays buffer))
    (delete-overlay overlay))
  (setf (spell-check-overlays buffer) nil))

(defun spell-check-live-overlays (buffer)
  "Return BUFFER's spell-check overlays that are still registered."
  (let* ((registered (buffer-overlays buffer))
         (overlays (remove-if-not (lambda (overlay)
                                    (member overlay registered))
                                  (spell-check-overlays buffer))))
    (setf (spell-check-overlays buffer) overlays)
    overlays))

(defun spell-check-overlay-word (overlay)
  "Return the misspelled word recorded in OVERLAY."
  (overlay-get overlay 'word))

(defun spell-check-make-overlay (line-point start end word)
  "Create an overlay for WORD between START and END on LINE-POINT's line."
  (with-point ((start-point line-point :temporary)
               (end-point line-point :temporary))
    (line-offset start-point 0 start)
    (line-offset end-point 0 end)
    (let ((overlay (make-overlay start-point end-point
                                 'spell-check-misspelled-attribute
                                 :end-point-kind :right-inserting)))
      (overlay-put overlay 'word word)
      overlay)))

(defun spell-check-delete-word-overlays (buffer word)
  "Delete all overlays for WORD in BUFFER."
  (let ((remaining '()))
    (dolist (overlay (spell-check-live-overlays buffer))
      (if (string= word (spell-check-overlay-word overlay))
          (delete-overlay overlay)
          (push overlay remaining)))
    (setf (spell-check-overlays buffer) (nreverse remaining))))

(defun spell-check-misspelled-p (dictionary word cache)
  "Return T if WORD is misspelled according to DICTIONARY.
CACHE maps words to results for the current scan."
  (multiple-value-bind (misspelled-p found-p)
      (gethash word cache)
    (if found-p
        misspelled-p
        (setf (gethash word cache)
              (handler-case
                  (null (enchant:dict-check dictionary word))
                (enchant:enchant-error ()
                  nil))))))

;;; Scanning

(defun spell-check-scan-buffer (buffer dictionary)
  "Replace BUFFER's overlays with a fresh scan using DICTIONARY.
Return the number of misspelled words."
  (let* ((configured-min-length
           (variable-value 'spell-check-min-word-length :default buffer))
         (min-length (if (and (integerp configured-min-length)
                              (plusp configured-min-length))
                         configured-min-length
                         2))
         (new-overlays '())
         (count 0)
         (cache (make-hash-table :test #'equal)))
    (handler-case
        (progn
          (with-point ((line-point (buffer-point buffer) :temporary))
            (buffer-start line-point)
            (loop
              (dolist (entry (parse-words (line-string line-point)))
                (destructuring-bind (start end word) entry
                  (when (and (spell-checkable-word-p word min-length)
                             (spell-check-misspelled-p dictionary word cache))
                    (push (spell-check-make-overlay line-point start end word)
                          new-overlays)
                    (incf count))))
              (unless (line-offset line-point 1)
                (return))))
          (spell-check-clear-overlays buffer)
          (setf (spell-check-overlays buffer) (nreverse new-overlays)
                (spell-check-scanned-tick buffer) (buffer-modified-tick buffer))
          count)
      (error (condition)
        (dolist (overlay new-overlays)
          (delete-overlay overlay))
        (editor-error "Spell check failed: ~A" condition)))))

(defun spell-check-cancel-timer (buffer)
  "Cancel BUFFER's pending rescan timer, if any."
  (let ((timer (spell-check-timer buffer)))
    (when timer
      (stop-timer timer)
      (setf (spell-check-timer buffer) nil))))

(defun spell-check-delay-for-buffer (buffer)
  "Return the rescan delay in milliseconds for BUFFER."
  (let ((delay (variable-value 'spell-check-delay :default buffer)))
    (if (and (integerp delay) (plusp delay))
        delay
        400)))

(defun spell-check-rescan-buffer (buffer)
  "Rescan BUFFER now and return the number of misspelled words."
  (spell-check-cancel-timer buffer)
  (spell-check-scan-buffer buffer
                           (spell-check-dictionary-for-buffer buffer)))

(defun spell-check-run-scheduled-scan (buffer)
  "Rescan BUFFER from its idle timer."
  (setf (spell-check-timer buffer) nil)
  (when (and (not (deleted-buffer-p buffer))
             (mode-active-p buffer 'spell-check-mode))
    (handler-case
        (spell-check-rescan-buffer buffer)
      (error (condition)
        (message "Spell check failed: ~A" condition)))))

(defun spell-check-schedule-scan (buffer)
  "Schedule a rescan of BUFFER after the configured idle delay."
  (spell-check-cancel-timer buffer)
  (setf (spell-check-timer buffer)
        (start-timer (make-idle-timer
                      (lambda ()
                        (spell-check-run-scheduled-scan buffer))
                      :name "spell-check")
                     (spell-check-delay-for-buffer buffer)
                     :repeat nil)))

(defun spell-check-ensure-overlays (buffer)
  "Return BUFFER's misspelling overlays, rescanning if they are stale."
  (unless (mode-active-p buffer 'spell-check-mode)
    (with-current-buffer buffer
      (enable-minor-mode 'spell-check-mode)))
  (when (or (spell-check-timer buffer)
            (null (spell-check-scanned-tick buffer))
            (/= (buffer-modified-tick buffer)
                (spell-check-scanned-tick buffer)))
    (spell-check-rescan-buffer buffer))
  (spell-check-live-overlays buffer))

;;; Mode lifecycle

(defun spell-check-after-change (start end old-length)
  "Schedule a rescan after the buffer is modified."
  (declare (ignore end old-length))
  (let ((buffer (point-buffer start)))
    (when (mode-active-p buffer 'spell-check-mode)
      (spell-check-schedule-scan buffer))))

(defun spell-check-on-kill-buffer (buffer)
  "Release spell-check resources before BUFFER is killed."
  (spell-check-cancel-timer buffer)
  (spell-check-clear-overlays buffer))

(defun spell-check-enable ()
  "Set up spell checking for the current buffer."
  (let ((buffer (current-buffer)))
    (handler-case
        (progn
          (spell-check-dictionary-for-buffer buffer)
          (add-hook (variable-value 'after-change-functions :buffer buffer)
                    'spell-check-after-change)
          (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
                    'spell-check-on-kill-buffer)
          (spell-check-rescan-buffer buffer))
      (error (condition)
        (ignore-errors (disable-minor-mode 'spell-check-mode))
        (editor-error "Cannot enable spell checking: ~A" condition)))))

(defun spell-check-disable ()
  "Remove spell-check state from the current buffer."
  (let ((buffer (current-buffer)))
    (remove-hook (variable-value 'after-change-functions :buffer buffer)
                 'spell-check-after-change)
    (remove-hook (variable-value 'kill-buffer-hook :buffer buffer)
                 'spell-check-on-kill-buffer)
    (spell-check-cancel-timer buffer)
    (spell-check-clear-overlays buffer)
    (setf (spell-check-scanned-tick buffer) nil)))

;;; Commands

(define-command spell-check-buffer () ()
  "Check the spelling of the whole current buffer."
  (let ((buffer (current-buffer)))
    (unless (mode-active-p buffer 'spell-check-mode)
      (enable-minor-mode 'spell-check-mode))
    (let ((count (spell-check-rescan-buffer buffer)))
      (message "~D misspelled word~:P" count))))

(defun spell-check-word-at-point (point)
  "Return the word at POINT and its offsets on the current line."
  (let ((charpos (point-charpos point)))
    (loop :for (start end word) :in (parse-words (line-string point))
          :when (<= start charpos end)
          :do (return (values word start end)))))

(defun spell-check-replace-word (dictionary word start end suggestions)
  "Prompt for a replacement for WORD and replace it in the buffer."
  (with-point ((start-point (current-point) :right-inserting)
               (end-point (current-point) :left-inserting))
    (line-offset start-point 0 start)
    (line-offset end-point 0 end)
    (let ((buffer (point-buffer start-point))
          (replacement
            (prompt-for-string
             (format nil "Replace ~S with: " word)
             :initial-value (first suggestions)
             :completion-function (lambda (input)
                                    (completion-strings input suggestions))
             :history-symbol 'spell-check-replacement)))
      (when (and replacement (plusp (length replacement)))
        (delete-between-points start-point end-point)
        (insert-string start-point replacement)
        (ignore-errors
          (enchant:dict-store-replacement dictionary word replacement))
        (when (mode-active-p buffer 'spell-check-mode)
          (spell-check-rescan-buffer buffer))))))

(define-command spell-check-word () ()
  "Check the word at point and offer spelling suggestions."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (word start end)
        (spell-check-word-at-point (current-point))
      (unless word
        (editor-error "No word at point"))
      (let ((dictionary (spell-check-dictionary-for-buffer buffer)))
        (if (enchant:dict-check dictionary word)
            (message "~S is spelled correctly" word)
            (let ((suggestions (spell-check-suggestions dictionary word)))
              (unless suggestions
                (editor-error "No spelling suggestions for ~S" word))
              (spell-check-replace-word dictionary word start end suggestions)))))))

(defun spell-check-report-overlay (buffer overlay)
  "Show the misspelled word and suggestions for OVERLAY."
  (let* ((word (spell-check-overlay-word overlay))
         (suggestions (spell-check-suggestions
                       (spell-check-dictionary-for-buffer buffer)
                       word
                       :limit 5)))
    (if suggestions
        (message "~S: ~{~A~^, ~}" word suggestions)
        (message "~S" word))))

(defun spell-check-goto-error (forward)
  "Move to the next or previous misspelling depending on FORWARD."
  (let* ((buffer (current-buffer))
         (overlays (sort (spell-check-ensure-overlays buffer)
                         #'point<
                         :key #'overlay-start))
         (point (current-point)))
    (if (null overlays)
        (message "No misspelled words")
        (let ((overlay
                (if forward
                    (or (find-if (lambda (overlay)
                                   (point< point (overlay-start overlay)))
                                 overlays)
                        (first overlays))
                    (or (find-if (lambda (overlay)
                                   (point< (overlay-start overlay) point))
                                 overlays
                                 :from-end t)
                        (car (last overlays))))))
          (move-point point (overlay-start overlay))
          (spell-check-report-overlay buffer overlay)))))

(define-command spell-check-next-error () ()
  "Move to the next misspelled word in the current buffer."
  (spell-check-goto-error t))

(define-command spell-check-previous-error () ()
  "Move to the previous misspelled word in the current buffer."
  (spell-check-goto-error nil))

(defun spell-check-add-word-at-point (add-function format-string)
  "Add the word at point using ADD-FUNCTION and report with FORMAT-STRING."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (word start end)
        (spell-check-word-at-point (current-point))
      (declare (ignore start end))
      (unless word
        (editor-error "No word at point"))
      (let ((dictionary (spell-check-dictionary-for-buffer buffer)))
        (handler-case
            (funcall add-function dictionary word)
          (enchant:enchant-error (condition)
            (editor-error "Cannot add ~S: ~A"
                          word (enchant:error-string condition))))
        (spell-check-delete-word-overlays buffer word)
        (message format-string word)))))

(define-command spell-check-add-word () ()
  "Add the word at point to the user's personal dictionary."
  (spell-check-add-word-at-point
   #'enchant:dict-add
   "Added ~S to the personal dictionary"))

(define-command spell-check-ignore-word () ()
  "Ignore the word at point for the rest of this session."
  (spell-check-add-word-at-point
   #'enchant:dict-add-to-session
   "Ignoring ~S for this session"))

(define-command spell-check-change-dictionary () ()
  "Select the Enchant dictionary used for the current buffer."
  (let* ((buffer (current-buffer))
         (available (spell-check-available-dictionaries))
         (current (ignore-errors (spell-check-language-for-buffer buffer)))
         (language (prompt-for-string
                    "Dictionary: "
                    :initial-value current
                    :completion-function (lambda (input)
                                           (completion-strings input available))
                    :history-symbol 'spell-check-dictionary)))
    (when (and language (plusp (length language)))
      (let* ((normalized (spell-check-normalize-language language))
             (canonical (and normalized
                             (spell-check-find-language (spell-check-broker)
                                                        normalized))))
        (unless canonical
          (editor-error "No Enchant dictionary for language ~S" language))
        (spell-check-dictionary-for-language canonical)
        (setf (variable-value 'spell-check-language :buffer buffer) canonical)
        (when (mode-active-p buffer 'spell-check-mode)
          (spell-check-rescan-buffer buffer))
        (message "Using dictionary ~A" canonical)))))

(define-command spell-check-clear () ()
  "Remove all spelling highlights from the current buffer."
  (let ((buffer (current-buffer)))
    (spell-check-cancel-timer buffer)
    (spell-check-clear-overlays buffer)
    (setf (spell-check-scanned-tick buffer) (buffer-modified-tick buffer))
    (message "Cleared spelling highlights")))
