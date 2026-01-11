(defsystem "lem-skk-mode"
  :description "SKK (Simple Kana to Kanji) input method for Lem"
  :author "Lem Project"
  :license "MIT"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "state")
               (:file "romaji")
               (:file "dictionary")
               (:file "conversion")
               (:file "display")
               (:file "skk-mode"))
  :in-order-to ((test-op (test-op "lem-skk-mode/tests"))))

(defsystem "lem-skk-mode/tests"
  :description "Tests for lem-skk-mode"
  :depends-on ("lem-skk-mode" "lem-fake-interface" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
