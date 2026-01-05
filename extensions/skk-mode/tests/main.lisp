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
    ;; "nna" = "nn" + "a" = "ん" + "あ" = "んあ"
    (ok (equal (romaji-to-hiragana "nna") "んあ"))))

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
    ;; Final "n" remains as input (handled by flush-preedit in actual usage)
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "shinkansen")
      (ok (equal converted "しんかんせ"))
      (ok (equal remaining "n")))))

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

;;; Part 3: Comprehensive "n" handling tests

(deftest test-n-edge-cases
  (testing "'n' at various positions"
    ;; n + vowel = にゃにゅにょ etc. (not ん)
    (ok (equal (romaji-to-hiragana "na") "な"))
    (ok (equal (romaji-to-hiragana "ni") "に"))
    (ok (equal (romaji-to-hiragana "nu") "ぬ"))
    (ok (equal (romaji-to-hiragana "ne") "ね"))
    (ok (equal (romaji-to-hiragana "no") "の"))
    ;; n + y = にゃ etc. (not ん)
    (ok (equal (romaji-to-hiragana "nya") "にゃ"))
    (ok (equal (romaji-to-hiragana "nyu") "にゅ"))
    (ok (equal (romaji-to-hiragana "nyo") "にょ"))
    ;; n + consonant = んx
    (ok (equal (romaji-to-hiragana "nka") "んか"))
    (ok (equal (romaji-to-hiragana "nsa") "んさ"))
    (ok (equal (romaji-to-hiragana "nta") "んた"))
    (ok (equal (romaji-to-hiragana "nha") "んは"))
    (ok (equal (romaji-to-hiragana "nma") "んま"))
    (ok (equal (romaji-to-hiragana "nra") "んら"))
    (ok (equal (romaji-to-hiragana "nwa") "んわ"))
    (ok (equal (romaji-to-hiragana "nga") "んが"))
    (ok (equal (romaji-to-hiragana "nza") "んざ"))
    (ok (equal (romaji-to-hiragana "nda") "んだ"))
    (ok (equal (romaji-to-hiragana "nba") "んば"))
    (ok (equal (romaji-to-hiragana "npa") "んぱ"))))

(deftest test-n-explicit-conversion
  (testing "explicit 'n' to 'ん' conversion"
    ;; nn = ん
    (ok (equal (romaji-to-hiragana "nn") "ん"))
    ;; "nna" = "nn" + "a" = "ん" + "あ" = "んあ"
    (ok (equal (romaji-to-hiragana "nna") "んあ"))
    ;; n' = ん
    (ok (equal (romaji-to-hiragana "n'") "ん"))
    (ok (equal (romaji-to-hiragana "n'a") "んあ"))))

(deftest test-n-remaining
  (testing "'n' at end remains as input (to wait for next char)"
    ;; Single n at end should remain
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "n")
      (ok (equal converted ""))
      (ok (equal remaining "n")))
    ;; n after converted kana should remain
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "kan")
      (ok (equal converted "か"))
      (ok (equal remaining "n")))
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "shin")
      (ok (equal converted "し"))
      (ok (equal remaining "n")))))

(deftest test-n-in-words
  (testing "'n' handling in complete words"
    ;; Words with ん before consonant
    ;; nihon - final "n" remains
    (multiple-value-bind (converted remaining)
        (romaji-to-hiragana "nihon")
      (ok (equal converted "にほ"))
      (ok (equal remaining "n")))
    (ok (equal (romaji-to-hiragana "konnnichiha") "こんにちは"))
    (ok (equal (romaji-to-hiragana "sanpo") "さんぽ"))
    (ok (equal (romaji-to-hiragana "ganbaru") "がんばる"))
    (ok (equal (romaji-to-hiragana "tenki") "てんき"))
    ;; Words with ん using nn
    (ok (equal (romaji-to-hiragana "unnmei") "うんめい"))
    ;; onnna = "onn" + "na" = "おん" + "な" = "おんな"
    (ok (equal (romaji-to-hiragana "onnna") "おんな"))
    ;; nwo -> んを
    (ok (equal (romaji-to-hiragana "nwo") "んを"))))

;;; Part 3: Complete Katakana tests

(deftest test-katakana-basic-vowels
  (testing "katakana basic vowels"
    (ok (equal (romaji-to-katakana "a") "ア"))
    (ok (equal (romaji-to-katakana "i") "イ"))
    (ok (equal (romaji-to-katakana "u") "ウ"))
    (ok (equal (romaji-to-katakana "e") "エ"))
    (ok (equal (romaji-to-katakana "o") "オ"))))

(deftest test-katakana-consonants
  (testing "katakana consonant+vowel"
    (ok (equal (romaji-to-katakana "ka") "カ"))
    (ok (equal (romaji-to-katakana "ki") "キ"))
    (ok (equal (romaji-to-katakana "ku") "ク"))
    (ok (equal (romaji-to-katakana "ke") "ケ"))
    (ok (equal (romaji-to-katakana "ko") "コ"))
    (ok (equal (romaji-to-katakana "sa") "サ"))
    (ok (equal (romaji-to-katakana "shi") "シ"))
    (ok (equal (romaji-to-katakana "su") "ス"))
    (ok (equal (romaji-to-katakana "se") "セ"))
    (ok (equal (romaji-to-katakana "so") "ソ"))
    (ok (equal (romaji-to-katakana "ta") "タ"))
    (ok (equal (romaji-to-katakana "chi") "チ"))
    (ok (equal (romaji-to-katakana "tsu") "ツ"))
    (ok (equal (romaji-to-katakana "te") "テ"))
    (ok (equal (romaji-to-katakana "to") "ト"))
    (ok (equal (romaji-to-katakana "na") "ナ"))
    (ok (equal (romaji-to-katakana "ni") "ニ"))
    (ok (equal (romaji-to-katakana "nu") "ヌ"))
    (ok (equal (romaji-to-katakana "ne") "ネ"))
    (ok (equal (romaji-to-katakana "no") "ノ"))
    (ok (equal (romaji-to-katakana "ha") "ハ"))
    (ok (equal (romaji-to-katakana "hi") "ヒ"))
    (ok (equal (romaji-to-katakana "fu") "フ"))
    (ok (equal (romaji-to-katakana "he") "ヘ"))
    (ok (equal (romaji-to-katakana "ho") "ホ"))
    (ok (equal (romaji-to-katakana "ma") "マ"))
    (ok (equal (romaji-to-katakana "mi") "ミ"))
    (ok (equal (romaji-to-katakana "mu") "ム"))
    (ok (equal (romaji-to-katakana "me") "メ"))
    (ok (equal (romaji-to-katakana "mo") "モ"))
    (ok (equal (romaji-to-katakana "ya") "ヤ"))
    (ok (equal (romaji-to-katakana "yu") "ユ"))
    (ok (equal (romaji-to-katakana "yo") "ヨ"))
    (ok (equal (romaji-to-katakana "ra") "ラ"))
    (ok (equal (romaji-to-katakana "ri") "リ"))
    (ok (equal (romaji-to-katakana "ru") "ル"))
    (ok (equal (romaji-to-katakana "re") "レ"))
    (ok (equal (romaji-to-katakana "ro") "ロ"))
    (ok (equal (romaji-to-katakana "wa") "ワ"))
    (ok (equal (romaji-to-katakana "wo") "ヲ"))))

(deftest test-katakana-voiced
  (testing "katakana voiced consonants"
    (ok (equal (romaji-to-katakana "ga") "ガ"))
    (ok (equal (romaji-to-katakana "gi") "ギ"))
    (ok (equal (romaji-to-katakana "gu") "グ"))
    (ok (equal (romaji-to-katakana "ge") "ゲ"))
    (ok (equal (romaji-to-katakana "go") "ゴ"))
    (ok (equal (romaji-to-katakana "za") "ザ"))
    (ok (equal (romaji-to-katakana "ji") "ジ"))
    (ok (equal (romaji-to-katakana "zu") "ズ"))
    (ok (equal (romaji-to-katakana "ze") "ゼ"))
    (ok (equal (romaji-to-katakana "zo") "ゾ"))
    (ok (equal (romaji-to-katakana "da") "ダ"))
    (ok (equal (romaji-to-katakana "di") "ヂ"))
    (ok (equal (romaji-to-katakana "du") "ヅ"))
    (ok (equal (romaji-to-katakana "de") "デ"))
    (ok (equal (romaji-to-katakana "do") "ド"))
    (ok (equal (romaji-to-katakana "ba") "バ"))
    (ok (equal (romaji-to-katakana "bi") "ビ"))
    (ok (equal (romaji-to-katakana "bu") "ブ"))
    (ok (equal (romaji-to-katakana "be") "ベ"))
    (ok (equal (romaji-to-katakana "bo") "ボ"))
    (ok (equal (romaji-to-katakana "pa") "パ"))
    (ok (equal (romaji-to-katakana "pi") "ピ"))
    (ok (equal (romaji-to-katakana "pu") "プ"))
    (ok (equal (romaji-to-katakana "pe") "ペ"))
    (ok (equal (romaji-to-katakana "po") "ポ"))))

(deftest test-katakana-youon
  (testing "katakana youon"
    (ok (equal (romaji-to-katakana "kya") "キャ"))
    (ok (equal (romaji-to-katakana "kyu") "キュ"))
    (ok (equal (romaji-to-katakana "kyo") "キョ"))
    (ok (equal (romaji-to-katakana "sha") "シャ"))
    (ok (equal (romaji-to-katakana "shu") "シュ"))
    (ok (equal (romaji-to-katakana "sho") "ショ"))
    (ok (equal (romaji-to-katakana "cha") "チャ"))
    (ok (equal (romaji-to-katakana "chu") "チュ"))
    (ok (equal (romaji-to-katakana "cho") "チョ"))
    (ok (equal (romaji-to-katakana "nya") "ニャ"))
    (ok (equal (romaji-to-katakana "nyu") "ニュ"))
    (ok (equal (romaji-to-katakana "nyo") "ニョ"))
    (ok (equal (romaji-to-katakana "hya") "ヒャ"))
    (ok (equal (romaji-to-katakana "hyu") "ヒュ"))
    (ok (equal (romaji-to-katakana "hyo") "ヒョ"))
    (ok (equal (romaji-to-katakana "mya") "ミャ"))
    (ok (equal (romaji-to-katakana "myu") "ミュ"))
    (ok (equal (romaji-to-katakana "myo") "ミョ"))
    (ok (equal (romaji-to-katakana "rya") "リャ"))
    (ok (equal (romaji-to-katakana "ryu") "リュ"))
    (ok (equal (romaji-to-katakana "ryo") "リョ"))
    (ok (equal (romaji-to-katakana "gya") "ギャ"))
    (ok (equal (romaji-to-katakana "gyu") "ギュ"))
    (ok (equal (romaji-to-katakana "gyo") "ギョ"))
    (ok (equal (romaji-to-katakana "ja") "ジャ"))
    (ok (equal (romaji-to-katakana "ju") "ジュ"))
    (ok (equal (romaji-to-katakana "jo") "ジョ"))
    (ok (equal (romaji-to-katakana "bya") "ビャ"))
    (ok (equal (romaji-to-katakana "byu") "ビュ"))
    (ok (equal (romaji-to-katakana "byo") "ビョ"))
    (ok (equal (romaji-to-katakana "pya") "ピャ"))
    (ok (equal (romaji-to-katakana "pyu") "ピュ"))
    (ok (equal (romaji-to-katakana "pyo") "ピョ"))))

(deftest test-katakana-sokuon
  (testing "katakana sokuon"
    (ok (equal (romaji-to-katakana "kka") "ッカ"))
    (ok (equal (romaji-to-katakana "tta") "ッタ"))
    (ok (equal (romaji-to-katakana "ppa") "ッパ"))
    (ok (equal (romaji-to-katakana "sshi") "ッシ"))
    (ok (equal (romaji-to-katakana "cchi") "ッチ"))))

(deftest test-katakana-small-kana
  (testing "katakana small kana"
    (ok (equal (romaji-to-katakana "xa") "ァ"))
    (ok (equal (romaji-to-katakana "xi") "ィ"))
    (ok (equal (romaji-to-katakana "xu") "ゥ"))
    (ok (equal (romaji-to-katakana "xe") "ェ"))
    (ok (equal (romaji-to-katakana "xo") "ォ"))
    (ok (equal (romaji-to-katakana "xtu") "ッ"))
    (ok (equal (romaji-to-katakana "xya") "ャ"))
    (ok (equal (romaji-to-katakana "xyu") "ュ"))
    (ok (equal (romaji-to-katakana "xyo") "ョ"))))

(deftest test-katakana-words
  (testing "katakana loanwords"
    ;; Use SKK-style long vowel notation with "-"
    (ok (equal (romaji-to-katakana "konpyu-ta-") "コンピューター"))
    ;; "we" maps to ゑ (archaic), for ウェ use "uxe" or similar
    (ok (equal (romaji-to-katakana "sofutowea") "ソフトヱア"))
    (ok (equal (romaji-to-katakana "inta-netto") "インターネット"))
    ;; Final "n" remains as input
    (multiple-value-bind (converted remaining)
        (romaji-to-katakana "pasokon")
      (ok (equal converted "パソコ"))
      (ok (equal remaining "n")))
    (ok (equal (romaji-to-katakana "terebi") "テレビ"))))

;;; Part 3: Hiragana/Katakana bidirectional conversion tests

(deftest test-bidirectional-conversion
  (testing "bidirectional hiragana/katakana conversion"
    ;; Hiragana -> Katakana -> Hiragana
    (let ((hiragana "あいうえおかきくけこ"))
      (ok (equal (katakana-to-hiragana (hiragana-to-katakana hiragana)) hiragana)))
    ;; Katakana -> Hiragana -> Katakana
    (let ((katakana "アイウエオカキクケコ"))
      (ok (equal (hiragana-to-katakana (katakana-to-hiragana katakana)) katakana)))
    ;; Complete hiragana row
    (ok (equal (hiragana-to-katakana "さしすせそ") "サシスセソ"))
    (ok (equal (hiragana-to-katakana "たちつてと") "タチツテト"))
    (ok (equal (hiragana-to-katakana "なにぬねの") "ナニヌネノ"))
    (ok (equal (hiragana-to-katakana "はひふへほ") "ハヒフヘホ"))
    (ok (equal (hiragana-to-katakana "まみむめも") "マミムメモ"))
    (ok (equal (hiragana-to-katakana "やゆよ") "ヤユヨ"))
    (ok (equal (hiragana-to-katakana "らりるれろ") "ラリルレロ"))
    (ok (equal (hiragana-to-katakana "わをん") "ワヲン"))
    ;; Voiced
    (ok (equal (hiragana-to-katakana "がぎぐげご") "ガギグゲゴ"))
    (ok (equal (hiragana-to-katakana "ざじずぜぞ") "ザジズゼゾ"))
    (ok (equal (hiragana-to-katakana "だぢづでど") "ダヂヅデド"))
    (ok (equal (hiragana-to-katakana "ばびぶべぼ") "バビブベボ"))
    (ok (equal (hiragana-to-katakana "ぱぴぷぺぽ") "パピプペポ"))))

;;; Part 3: Dictionary tests

(deftest test-dictionary-parse-edge-cases
  (testing "dictionary parsing edge cases"
    ;; Entry with multiple annotations
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "し /市;city/詩;poem/死;death/")))
      (ok (equal (car result) "し"))
      (ok (equal (cdr result) '("市" "詩" "死"))))
    ;; Entry with okurigana marker
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "あるk /歩/")))
      (ok (equal (car result) "あるk"))
      (ok (equal (cdr result) '("歩"))))
    ;; Single candidate
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "ほん /本/")))
      (ok (equal (car result) "ほん"))
      (ok (equal (cdr result) '("本"))))
    ;; Many candidates
    (let ((result (lem-skk-mode/dictionary::parse-dictionary-line
                   "かん /缶/感/管/巻/館/間/観/官/関/")))
      (ok (equal (car result) "かん"))
      (ok (= (length (cdr result)) 9)))))

;;; Part 3: Punctuation and special character tests

(deftest test-punctuation
  (testing "punctuation conversion"
    (ok (equal (romaji-to-hiragana "-") "ー"))
    (ok (equal (romaji-to-hiragana ".") "。"))
    (ok (equal (romaji-to-hiragana ",") "、"))
    (ok (equal (romaji-to-hiragana "[") "「"))
    (ok (equal (romaji-to-hiragana "]") "」"))
    (ok (equal (romaji-to-hiragana "/") "・"))))

(deftest test-mixed-input
  (testing "mixed romaji and punctuation"
    (ok (equal (romaji-to-hiragana "nihongo.") "にほんご。"))
    (ok (equal (romaji-to-hiragana "toukyou-to") "とうきょうーと"))))
