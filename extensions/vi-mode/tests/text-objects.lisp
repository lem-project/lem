(defpackage :lem-vi-mode/tests/text-objects
  (:use :cl
        :lem
        :rove
        :lem-vi-mode/tests/utils)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :named-readtables
                :in-readtable))
(in-package :lem-vi-mode/tests/text-objects)

(in-readtable :interpol-syntax)

(deftest word-object
  (with-fake-interface ()
    (with-vi-buffer (#?"abc\n  [ ] def\n")
      (cmd "viw")
      (ok (buf= #?"abc\n<   [ ]>def\n")))))

(deftest broad-word-object
  (with-fake-interface ()
    (with-vi-buffer (#?"abc [d]ef.ghi \n")
      (cmd "viW")
      (ok (buf= #?"abc <def.gh[i]> \n"))
      (cmd "vvaW")
      (ok (buf= #?"abc <def.ghi[ ]>\n"))
      )))

(deftest double-quoted
  (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks t)
    (with-fake-interface ()
      (with-vi-buffer ("[ ]\"foo\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" [\"]foo\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"f[o]o\" \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"foo[\"] \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"[ ]>\"bar\"")))
      (with-vi-buffer (" \"foo\"[ ]\"bar\"")
        (cmd "va\"")
        ;; NOTE: This behavior is not same as Vim
        (ok (buf= " \"foo\"< \"bar[\"]>")))
      (with-vi-buffer (" \"foo\" \"bar[\"]")
        (cmd "va\"")
        (ok (buf= " \"foo\"< \"bar[\"]>")))

      ;; Escape Character
      ; v i
      (with-vi-buffer (" \" f[o]o \\\" bar \"")
        (cmd "vi\"")
        (ok (buf= " \"< foo \\\" bar[ ]>\"")))
      (with-vi-buffer (" \" foo \\\" b[a]r \"")
        (cmd "vi\"")
        (ok (buf= " \"< foo \\\" bar[ ]>\"")))
      ; v a
      (with-vi-buffer (" \" f[o]o \\\" bar \"")
        (cmd "va\"")
        (ok (buf= " <\" foo \\\" bar [\"]>")))
      (with-vi-buffer (" \" foo \\\" b[a]r \"")
        (cmd "va\"")
        (ok (buf= " <\" foo \\\" bar [\"]>"))))))

(deftest vi-operator-surrounding-blanks
  (with-fake-interface ()
    (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks nil)
      (with-vi-buffer (" \"f[o]o\"   \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo[\"]>   \"bar\""))))
    (with-global-variable-value (lem-vi-mode/text-objects:vi-operator-surrounding-blanks t)
      (with-vi-buffer (" \"f[o]o\"   \"bar\"")
        (cmd "va\"")
        (ok (buf= " <\"foo\"  [ ]>\"bar\""))))))

(deftest paragraph-object
  (with-fake-interface ()
    (with-vi-buffer (#?" \n \n f[o]o \n bar \n \n \n")
      (cmd "vip")
      (ok (buf= #?" \n \n< foo \n bar [\n]> \n \n")))
    (with-vi-buffer (#?" \n \n f[o]o \n bar \n \n \n")
      (cmd "vap")
      (ok (buf= #?" \n \n< foo \n bar \n \n [\n]>")))))

(deftest single-quoted
  (with-fake-interface ()
    ;; inner single quote
    (with-vi-buffer (" 'f[o]o' 'bar'")
      (cmd "vi'")
      (ok (buf= " '<fo[o]>' 'bar'")))
    ;; around single quote
    (with-vi-buffer (" 'f[o]o' 'bar'")
      (cmd "va'")
      (ok (buf= " <'foo[']> 'bar'")))))

(deftest back-quoted
  (with-fake-interface ()
    ;; inner back quote
    (with-vi-buffer (" `f[o]o` `bar`")
      (cmd "vi`")
      (ok (buf= " `<fo[o]>` `bar`")))
    ;; around back quote
    (with-vi-buffer (" `f[o]o` `bar`")
      (cmd "va`")
      (ok (buf= " <`foo[`]> `bar`")))))

(deftest bracket-object
  (with-fake-interface ()
    ;; inner bracket
    (with-vi-buffer (" \\[f[o]o bar\\] baz")
      (cmd "vi[")
      (ok (buf= " \\[<foo ba[r]>\\] baz")))
    (with-vi-buffer (" \\[f[o]o bar\\] baz")
      (cmd "vi]")
      (ok (buf= " \\[<foo ba[r]>\\] baz")))
    ;; around bracket
    (with-vi-buffer (" \\[f[o]o bar\\] baz")
      (cmd "va[")
      (ok (buf= " <\\[foo bar[\\]]> baz")))
    ;; nested brackets
    (with-vi-buffer (" \\[foo \\[b[a]r\\] baz\\]")
      (cmd "vi[")
      (ok (buf= " \\[foo \\[<ba[r]>\\] baz\\]")))
    ;; cursor inside inner bracket, then expand
    (with-vi-buffer (" \\[foo \\[b[a]r\\] baz\\]")
      (cmd "va[")
      (ok (buf= " \\[foo <\\[bar[\\]]> baz\\]")))))

(deftest curly-object
  (with-fake-interface ()
    ;; inner curly
    (with-vi-buffer (" {f[o]o bar} baz")
      (cmd "vi{")
      (ok (buf= " {<foo ba[r]>} baz")))
    (with-vi-buffer (" {f[o]o bar} baz")
      (cmd "vi}")
      (ok (buf= " {<foo ba[r]>} baz")))
    ;; around curly
    (with-vi-buffer (" {f[o]o bar} baz")
      (cmd "va{")
      (ok (buf= " <{foo bar[}]> baz")))
    ;; B alias
    (with-vi-buffer (" {f[o]o bar} baz")
      (cmd "viB")
      (ok (buf= " {<foo ba[r]>} baz")))
    (with-vi-buffer (" {f[o]o bar} baz")
      (cmd "vaB")
      (ok (buf= " <{foo bar[}]> baz")))))

(deftest angle-bracket-object
  (with-fake-interface ()
    ;; inner angle bracket
    (with-vi-buffer (" \\<f[o]o bar\\> baz")
      (cmd "vi<")
      (ok (buf= " \\<<foo ba[r]>\\> baz")))
    (with-vi-buffer (" \\<f[o]o bar\\> baz")
      (cmd "vi>")
      (ok (buf= " \\<<foo ba[r]>\\> baz")))
    ;; around angle bracket
    (with-vi-buffer (" \\<f[o]o bar\\> baz")
      (cmd "va<")
      (ok (buf= " <\\<foo bar[\\>]> baz")))))

(deftest tag-object
  (with-fake-interface ()
    ;; inner tag
    (with-vi-buffer ("\\<div\\>f[o]o\\</div\\>")
      (cmd "vit")
      (ok (buf= "\\<div\\><fo[o]>\\</div\\>")))
    ;; around tag
    (with-vi-buffer ("\\<div\\>f[o]o\\</div\\>")
      (cmd "vat")
      (ok (buf= "<\\<div\\>foo\\</div[\\>]>")))
    ;; tag with attributes
    (with-vi-buffer ("\\<div class=\"main\"\\>f[o]o\\</div\\>")
      (cmd "vit")
      (ok (buf= "\\<div class=\"main\"\\><fo[o]>\\</div\\>")))
    ;; nested tags
    (with-vi-buffer ("\\<div\\>\\<span\\>f[o]o\\</span\\>\\</div\\>")
      (cmd "vit")
      (ok (buf= "\\<div\\>\\<span\\><fo[o]>\\</span\\>\\</div\\>")))
    ;; multiline tag content
    (with-vi-buffer (#?"\\<div\\>\n  f[o]o\n\\</div\\>")
      (cmd "vit")
      (ok (buf= #?"\\<div\\><\n  foo[\n]>\\</div\\>")))))


