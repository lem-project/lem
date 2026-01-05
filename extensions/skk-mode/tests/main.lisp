(defpackage :lem-skk-mode/tests/main
  (:use :cl :rove)
  (:import-from :lem-skk-mode/romaji
                :romaji-to-hiragana
                :romaji-to-katakana
                :hiragana-to-katakana
                :katakana-to-hiragana)
  (:import-from :lem-skk-mode/dictionary
                :parse-dictionary-line))
(in-package :lem-skk-mode/tests/main)

;;; Romaji Conversion Tests

(deftest test-basic-vowels
  (testing "basic vowel conversion"
    (ok (equal (romaji-to-hiragana "a") "あ"))
    (ok (equal (romaji-to-hiragana "i") "い"))
    (ok (equal (romaji-to-hiragana "u") "う"))
    (ok (equal (romaji-to-hiragana "e") "え"))
    (ok (equal (romaji-to-hiragana "o") "お"))))

(deftest test-basic-consonants
  (testing "basic consonant+vowel conversion"
    (ok (equal (romaji-to-hiragana "ka") "か"))
    (ok (equal (romaji-to-hiragana "ki") "き"))
    (ok (equal (romaji-to-hiragana "ku") "く"))
    (ok (equal (romaji-to-hiragana "ke") "け"))
    (ok (equal (romaji-to-hiragana "ko") "こ"))
    (ok (equal (romaji-to-hiragana "sa") "さ"))
    (ok (equal (romaji-to-hiragana "ta") "た"))
    (ok (equal (romaji-to-hiragana "na") "な"))
    (ok (equal (romaji-to-hiragana "ha") "は"))
    (ok (equal (romaji-to-hiragana "ma") "ま"))
    (ok (equal (romaji-to-hiragana "ya") "や"))
    (ok (equal (romaji-to-hiragana "ra") "ら"))
    (ok (equal (romaji-to-hiragana "wa") "わ"))))

(deftest test-alternative-spellings
  (testing "alternative romaji spellings"
    (ok (equal (romaji-to-hiragana "si") "し"))
    (ok (equal (romaji-to-hiragana "shi") "し"))
    (ok (equal (romaji-to-hiragana "ti") "ち"))
    (ok (equal (romaji-to-hiragana "chi") "ち"))
    (ok (equal (romaji-to-hiragana "tu") "つ"))
    (ok (equal (romaji-to-hiragana "tsu") "つ"))
    (ok (equal (romaji-to-hiragana "hu") "ふ"))
    (ok (equal (romaji-to-hiragana "fu") "ふ"))
    (ok (equal (romaji-to-hiragana "zi") "じ"))
    (ok (equal (romaji-to-hiragana "ji") "じ"))))

(deftest test-voiced-consonants
  (testing "voiced consonants (dakuon)"
    (ok (equal (romaji-to-hiragana "ga") "が"))
    (ok (equal (romaji-to-hiragana "za") "ざ"))
    (ok (equal (romaji-to-hiragana "da") "だ"))
    (ok (equal (romaji-to-hiragana "ba") "ば"))))

(deftest test-semi-voiced
  (testing "semi-voiced consonants (handakuon)"
    (ok (equal (romaji-to-hiragana "pa") "ぱ"))
    (ok (equal (romaji-to-hiragana "pi") "ぴ"))
    (ok (equal (romaji-to-hiragana "pu") "ぷ"))
    (ok (equal (romaji-to-hiragana "pe") "ぺ"))
    (ok (equal (romaji-to-hiragana "po") "ぽ"))))

(deftest test-youon
  (testing "youon (contracted sounds)"
    (ok (equal (romaji-to-hiragana "kya") "きゃ"))
    (ok (equal (romaji-to-hiragana "kyu") "きゅ"))
    (ok (equal (romaji-to-hiragana "kyo") "きょ"))
    (ok (equal (romaji-to-hiragana "sha") "しゃ"))
    (ok (equal (romaji-to-hiragana "shu") "しゅ"))
    (ok (equal (romaji-to-hiragana "sho") "しょ"))
    (ok (equal (romaji-to-hiragana "cha") "ちゃ"))
    (ok (equal (romaji-to-hiragana "chu") "ちゅ"))
    (ok (equal (romaji-to-hiragana "cho") "ちょ"))
    (ok (equal (romaji-to-hiragana "ja") "じゃ"))
    (ok (equal (romaji-to-hiragana "ju") "じゅ"))
    (ok (equal (romaji-to-hiragana "jo") "じょ"))))

(deftest test-sokuon
  (testing "sokuon (double consonants)"
    (ok (equal (romaji-to-hiragana "kka") "っか"))
    (ok (equal (romaji-to-hiragana "tta") "った"))
    (ok (equal (romaji-to-hiragana "ppa") "っぱ"))
    (ok (equal (romaji-to-hiragana "sshi") "っし"))
    (ok (equal (romaji-to-hiragana "cchi") "っち"))))

(deftest test-n-handling
  (testing "handling of 'n'"
    (ok (equal (romaji-to-hiragana "nn") "ん"))
    (ok (equal (romaji-to-hiragana "n'") "ん"))
    (ok (equal (romaji-to-hiragana "nka") "んか"))
    (ok (equal (romaji-to-hiragana "nta") "んた"))
    (ok (equal (romaji-to-hiragana "nna") "んな"))))

(deftest test-small-kana
  (testing "small kana"
    (ok (equal (romaji-to-hiragana "xa") "ぁ"))
    (ok (equal (romaji-to-hiragana "xi") "ぃ"))
    (ok (equal (romaji-to-hiragana "xtu") "っ"))
    (ok (equal (romaji-to-hiragana "la") "ぁ"))
    (ok (equal (romaji-to-hiragana "ltu") "っ"))))

(deftest test-word-conversion
  (testing "full word conversion"
    (ok (equal (romaji-to-hiragana "nihongo") "にほんご"))
    (ok (equal (romaji-to-hiragana "toukyou") "とうきょう"))
    (ok (equal (romaji-to-hiragana "gakkou") "がっこう"))
    (ok (equal (romaji-to-hiragana "shinkansen") "しんかんせん"))))

(deftest test-remaining-input
  (testing "remaining input handling"
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "k")
      (ok (equal converted ""))
      (ok (equal remaining "k")))
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "ky")
      (ok (equal converted ""))
      (ok (equal remaining "ky")))
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "kak")
      (ok (equal converted "か"))
      (ok (equal remaining "k")))))

;;; Katakana Conversion Tests

(deftest test-katakana-conversion
  (testing "katakana conversion"
    (ok (equal (romaji-to-katakana "a") "ア"))
    (ok (equal (romaji-to-katakana "ka") "カ"))
    (ok (equal (romaji-to-katakana "nihongo") "ニホンゴ"))))

(deftest test-hiragana-katakana-conversion
  (testing "hiragana to katakana"
    (ok (equal (hiragana-to-katakana "あいうえお") "アイウエオ"))
    (ok (equal (hiragana-to-katakana "かきくけこ") "カキクケコ"))
    (ok (equal (hiragana-to-katakana "にほんご") "ニホンゴ"))))

(deftest test-katakana-hiragana-conversion
  (testing "katakana to hiragana"
    (ok (equal (katakana-to-hiragana "アイウエオ") "あいうえお"))
    (ok (equal (katakana-to-hiragana "カキクケコ") "かきくけこ"))
    (ok (equal (katakana-to-hiragana "ニホンゴ") "にほんご"))))

;;; Dictionary Tests

(deftest test-dictionary-parse
  (testing "dictionary line parsing"
    ;; Basic entry
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "かんじ /漢字/感じ/幹事/")))
      (ok (equal (car result) "かんじ"))
      (ok (equal (cdr result) '("漢字" "感じ" "幹事"))))
    ;; Entry with annotation
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "かんじ /漢字;Chinese characters/感じ;feeling/")))
      (ok (equal (car result) "かんじ"))
      (ok (equal (cdr result) '("漢字" "感じ"))))
    ;; Comment line
    (ok (null (lem-skk-mode/dictionary::parse-dictionary-line
               ";; This is a comment")))
    ;; Empty line
    (ok (null (lem-skk-mode/dictionary::parse-dictionary-line "")))))
