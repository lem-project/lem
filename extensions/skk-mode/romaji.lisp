(defpackage :lem-skk-mode/romaji
  (:use :cl)
  (:export :romaji-to-hiragana
           :romaji-to-katakana
           :hiragana-to-katakana
           :katakana-to-hiragana))
(in-package :lem-skk-mode/romaji)

(defparameter *romaji-hiragana-alist*
  '(;; Basic vowels
    ("a" . "あ") ("i" . "い") ("u" . "う") ("e" . "え") ("o" . "お")
    ;; K row
    ("ka" . "か") ("ki" . "き") ("ku" . "く") ("ke" . "け") ("ko" . "こ")
    ;; S row
    ("sa" . "さ") ("si" . "し") ("su" . "す") ("se" . "せ") ("so" . "そ")
    ("shi" . "し")
    ;; T row
    ("ta" . "た") ("ti" . "ち") ("tu" . "つ") ("te" . "て") ("to" . "と")
    ("chi" . "ち") ("tsu" . "つ")
    ;; N row
    ("na" . "な") ("ni" . "に") ("nu" . "ぬ") ("ne" . "ね") ("no" . "の")
    ;; H row
    ("ha" . "は") ("hi" . "ひ") ("hu" . "ふ") ("he" . "へ") ("ho" . "ほ")
    ("fu" . "ふ")
    ;; M row
    ("ma" . "ま") ("mi" . "み") ("mu" . "む") ("me" . "め") ("mo" . "も")
    ;; Y row
    ("ya" . "や") ("yu" . "ゆ") ("yo" . "よ")
    ;; R row
    ("ra" . "ら") ("ri" . "り") ("ru" . "る") ("re" . "れ") ("ro" . "ろ")
    ;; W row
    ("wa" . "わ") ("wi" . "ゐ") ("we" . "ゑ") ("wo" . "を")
    ;; N
    ("nn" . "ん") ("n'" . "ん")
    ;; G row (voiced)
    ("ga" . "が") ("gi" . "ぎ") ("gu" . "ぐ") ("ge" . "げ") ("go" . "ご")
    ;; Z row (voiced)
    ("za" . "ざ") ("zi" . "じ") ("zu" . "ず") ("ze" . "ぜ") ("zo" . "ぞ")
    ("ji" . "じ")
    ;; D row (voiced)
    ("da" . "だ") ("di" . "ぢ") ("du" . "づ") ("de" . "で") ("do" . "ど")
    ;; B row (voiced)
    ("ba" . "ば") ("bi" . "び") ("bu" . "ぶ") ("be" . "べ") ("bo" . "ぼ")
    ;; P row (semi-voiced)
    ("pa" . "ぱ") ("pi" . "ぴ") ("pu" . "ぷ") ("pe" . "ぺ") ("po" . "ぽ")
    ;; Youon (K)
    ("kya" . "きゃ") ("kyu" . "きゅ") ("kyo" . "きょ")
    ;; Youon (S)
    ("sha" . "しゃ") ("shu" . "しゅ") ("sho" . "しょ")
    ("sya" . "しゃ") ("syu" . "しゅ") ("syo" . "しょ")
    ;; Youon (T)
    ("cha" . "ちゃ") ("chu" . "ちゅ") ("cho" . "ちょ")
    ("tya" . "ちゃ") ("tyu" . "ちゅ") ("tyo" . "ちょ")
    ;; Youon (N)
    ("nya" . "にゃ") ("nyu" . "にゅ") ("nyo" . "にょ")
    ;; Youon (H)
    ("hya" . "ひゃ") ("hyu" . "ひゅ") ("hyo" . "ひょ")
    ;; Youon (M)
    ("mya" . "みゃ") ("myu" . "みゅ") ("myo" . "みょ")
    ;; Youon (R)
    ("rya" . "りゃ") ("ryu" . "りゅ") ("ryo" . "りょ")
    ;; Youon (G)
    ("gya" . "ぎゃ") ("gyu" . "ぎゅ") ("gyo" . "ぎょ")
    ;; Youon (J/Z)
    ("ja" . "じゃ") ("ju" . "じゅ") ("jo" . "じょ")
    ("jya" . "じゃ") ("jyu" . "じゅ") ("jyo" . "じょ")
    ("zya" . "じゃ") ("zyu" . "じゅ") ("zyo" . "じょ")
    ;; Youon (B)
    ("bya" . "びゃ") ("byu" . "びゅ") ("byo" . "びょ")
    ;; Youon (P)
    ("pya" . "ぴゃ") ("pyu" . "ぴゅ") ("pyo" . "ぴょ")
    ;; Small kana (x prefix)
    ("xa" . "ぁ") ("xi" . "ぃ") ("xu" . "ぅ") ("xe" . "ぇ") ("xo" . "ぉ")
    ("xtu" . "っ") ("xtsu" . "っ")
    ("xya" . "ゃ") ("xyu" . "ゅ") ("xyo" . "ょ")
    ("xwa" . "ゎ")
    ;; Small kana (l prefix, alternative)
    ("la" . "ぁ") ("li" . "ぃ") ("lu" . "ぅ") ("le" . "ぇ") ("lo" . "ぉ")
    ("ltu" . "っ") ("ltsu" . "っ")
    ("lya" . "ゃ") ("lyu" . "ゅ") ("lyo" . "ょ")
    ("lwa" . "ゎ")
    ;; Special combinations
    ("fa" . "ふぁ") ("fi" . "ふぃ") ("fe" . "ふぇ") ("fo" . "ふぉ")
    ("ti" . "ち") ("tu" . "つ")
    ("di" . "ぢ") ("du" . "づ")
    ("dha" . "でゃ") ("dhi" . "でぃ") ("dhu" . "でゅ") ("dhe" . "でぇ") ("dho" . "でょ")
    ("tha" . "てゃ") ("thi" . "てぃ") ("thu" . "てゅ") ("the" . "てぇ") ("tho" . "てょ")
    ;; Punctuation
    ("-" . "ー")
    ("." . "。")
    ("," . "、")
    ("[" . "「")
    ("]" . "」")
    ("/" . "・"))
  "Alist mapping romaji to hiragana.")

(defvar *romaji-hiragana-table* nil
  "Hash table for romaji to hiragana conversion.")

(defun ensure-romaji-table ()
  "Ensure the romaji conversion table is initialized."
  (unless *romaji-hiragana-table*
    (setf *romaji-hiragana-table*
          (make-hash-table :test 'equal :size (length *romaji-hiragana-alist*)))
    (dolist (pair *romaji-hiragana-alist*)
      (setf (gethash (car pair) *romaji-hiragana-table*) (cdr pair)))))

(defparameter *sokuon-consonants*
  '(#\k #\s #\t #\p #\c #\g #\z #\d #\b #\j #\f #\h #\m #\r #\w)
  "Consonants that can form sokuon (っ) by doubling.")

(defun sokuon-consonant-p (char)
  "Return non-nil if CHAR can form sokuon by doubling."
  (member (char-downcase char) *sokuon-consonants*))

(defun vowel-p (char)
  "Return non-nil if CHAR is a vowel."
  (member (char-downcase char) '(#\a #\i #\u #\e #\o)))

(defun romaji-to-hiragana (romaji)
  "Convert ROMAJI string to hiragana.
Returns two values: converted string and remaining unconverted romaji."
  (ensure-romaji-table)
  (let ((input (string-downcase romaji))
        (result (make-string-output-stream)))
    (loop :while (plusp (length input))
          :do (let ((matched nil))
                ;; Try longest match first (4 chars down to 1)
                (loop :for len :from (min 4 (length input)) :downto 1
                      :for substr := (subseq input 0 len)
                      :for kana := (gethash substr *romaji-hiragana-table*)
                      :when kana
                        :do (write-string kana result)
                            (setf input (subseq input len)
                                  matched t)
                            (return))
                ;; Check for sokuon (doubled consonant)
                (unless matched
                  (when (and (>= (length input) 2)
                             (sokuon-consonant-p (char input 0))
                             (char= (char input 0) (char input 1)))
                    (write-string "っ" result)
                    (setf input (subseq input 1)
                          matched t)))
                ;; Handle 'n' before non-vowel/non-y (but NOT at end of string)
                (unless matched
                  (when (and (char= (char input 0) #\n)
                             (> (length input) 1)  ; Must have next char
                             (let ((next (char input 1)))
                               (and (not (vowel-p next))
                                    (char/= next #\y)
                                    (char/= next #\n))))  ; "nn" handled by table
                    (write-string "ん" result)
                    (setf input (subseq input 1)
                          matched t)))
                ;; No match - stop processing, return remaining
                (unless matched
                  (return))))
    (values (get-output-stream-string result) input)))

(defun hiragana-to-katakana (hiragana)
  "Convert HIRAGANA string to katakana."
  (map 'string
       (lambda (c)
         (let ((code (char-code c)))
           ;; Hiragana range: U+3041 to U+3096
           ;; Katakana offset: +96 (0x60)
           (if (and (>= code #x3041) (<= code #x3096))
               (code-char (+ code #x60))
               c)))
       hiragana))

(defun katakana-to-hiragana (katakana)
  "Convert KATAKANA string to hiragana."
  (map 'string
       (lambda (c)
         (let ((code (char-code c)))
           ;; Katakana range: U+30A1 to U+30F6
           ;; Hiragana offset: -96 (0x60)
           (if (and (>= code #x30A1) (<= code #x30F6))
               (code-char (- code #x60))
               c)))
       katakana))

(defun romaji-to-katakana (romaji)
  "Convert ROMAJI string to katakana.
Returns two values: converted string and remaining unconverted romaji."
  (multiple-value-bind (hiragana remaining)
      (romaji-to-hiragana romaji)
    (values (hiragana-to-katakana hiragana) remaining)))
