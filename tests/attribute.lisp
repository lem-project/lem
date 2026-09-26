(defpackage :lem-tests/attribute
  (:use :cl :rove))
(in-package :lem-tests/attribute)

(lem:define-attribute test-styled-attribute
  (t :foreground "#aabbcc" :italic t :underline t :underline-style :curly))

(deftest font-styles-are-attribute-slots
  (let ((attribute (lem:make-attribute :italic t :strikethrough t :dim t
                                       :underline t :underline-style :dotted)))
    (ok (lem:attribute-italic attribute))
    (ok (lem:attribute-strikethrough attribute))
    (ok (lem:attribute-dim attribute))
    (ok (eq :dotted (lem:attribute-underline-style attribute))))
  (let ((plain (lem:make-attribute)))
    (ok (null (lem:attribute-italic plain)))
    (ok (null (lem:attribute-strikethrough plain)))
    (ok (null (lem:attribute-dim plain)))
    (ok (null (lem:attribute-underline-style plain)))))

(deftest merging-keeps-font-styles-from-either-side
  (let ((merged (lem:merge-attribute (lem:make-attribute :italic t :underline-style :curly)
                                     (lem:make-attribute :foreground "#112233" :dim t))))
    (ok (lem:attribute-italic merged) "from under")
    (ok (eq :curly (lem:attribute-underline-style merged)) "from under")
    (ok (lem:attribute-dim merged) "from over"))
  (ok (eq :dashed (lem:attribute-underline-style
                   (lem:merge-attribute (lem:make-attribute :underline-style :curly)
                                        (lem:make-attribute :underline-style :dashed))))
      "over wins"))

(deftest attributes-differing-only-in-a-font-style-are-not-equal
  ;; Lem merges look-alike neighbours and skips unchanged lines by this.
  (flet ((with (&rest args)
           (apply #'lem:make-attribute :foreground "#aabbcc" :underline t args)))
    (ok (lem:attribute-equal (with) (with)))
    (ok (lem:attribute-equal (with :italic t) (with :italic t)))
    (ng (lem:attribute-equal (with) (with :italic t)))
    (ng (lem:attribute-equal (with) (with :strikethrough t)))
    (ng (lem:attribute-equal (with) (with :dim t)))
    (ng (lem:attribute-equal (with) (with :underline-style :curly)))
    (ng (lem:attribute-equal (with :underline-style :curly)
                             (with :underline-style :double)))))

(deftest a-font-style-changes-the-line-fingerprint
  ;; ITEM-CONTENT-HASH is internal to lem-core, and not exported on purpose:
  ;; it is the line fingerprint that decides whether a line is redrawn.
  ;; A slot it does not hash leaves a line whose only change is that slot
  ;; stale on screen, and nothing public shows that without a frontend, so
  ;; the test checks it directly, as tests/display-cache.lisp does.
  (flet ((hash (&rest args)
           (lem-core::item-content-hash (apply #'lem:make-attribute :foreground "#aabbcc" args))))
    (ok (= (hash :italic t) (hash :italic t)))
    (ng (= (hash) (hash :italic t)))
    (ng (= (hash) (hash :underline-style :curly)))))

(deftest a-theme-keeps-font-styles-it-does-not-name
  (let ((attribute (lem:ensure-attribute 'test-styled-attribute)))
    (lem:set-attribute attribute :foreground "#000000" :underline t)
    (ok (lem:attribute-italic attribute) "only colours named")
    (ok (eq :curly (lem:attribute-underline-style attribute)))
    (lem:set-attribute attribute :italic nil :underline-style nil)
    (ng (lem:attribute-italic attribute) "named, so set")
    (ng (lem:attribute-underline-style attribute))
    (lem:set-attribute-strikethrough attribute t)
    (ok (lem:attribute-strikethrough attribute))))

(lem:define-attribute test-linked-attribute
  (t :underline t :link "https://example.com"))

(deftest a-link-is-carried-merged-and-compared
  (let* ((look (lem:make-attribute :foreground "#aabbcc" :underline t))
         (link (lem:make-attribute :link "https://example.com"))
         (merged (lem:merge-attribute look link)))
    (ok (equal "https://example.com" (lem:attribute-link merged)))
    (ok (equal "#aabbcc" (lem:attribute-foreground merged)) "the look is kept")
    (ok (lem:attribute-equal merged (lem:merge-attribute look (lem:make-attribute :link "https://example.com")))
        "equal URLs, equal attributes")
    (ng (lem:attribute-equal look merged) "linked text is not merged into plain text")
    (ng (lem:attribute-equal merged (lem:merge-attribute look (lem:make-attribute :link "https://example.org"))))
    ;; ITEM-CONTENT-HASH is internal to lem-core, and not exported on
    ;; purpose: it is the line fingerprint that decides whether a line is
    ;; redrawn. A slot it does not hash leaves a line whose only change is
    ;; that slot stale on screen, and nothing public shows that without a
    ;; frontend, so the test checks it directly, as
    ;; tests/display-cache.lisp does.
    (ng (= (lem-core::item-content-hash merged)
           (lem-core::item-content-hash
            (lem:merge-attribute look (lem:make-attribute :link "https://example.org"))))
        "a line whose link changed is redrawn")))

(deftest a-theme-keeps-a-link-it-does-not-name
  (let ((attribute (lem:ensure-attribute 'test-linked-attribute)))
    (lem:set-attribute attribute :foreground "#000000" :underline t)
    (ok (equal "https://example.com" (lem:attribute-link attribute)) "only colours named")
    (lem:set-attribute attribute :link nil)
    (ng (lem:attribute-link attribute) "named, so set")
    (lem:set-attribute-link attribute "https://example.org")
    (ok (equal "https://example.org" (lem:attribute-link attribute)))))
