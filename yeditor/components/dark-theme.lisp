;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/dark-theme
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/palette
                #:*palette-themes-function* #:*palette-theme-aliases*)
  (:export #:le-dark-theme))
(in-package :yeditor-components/dark-theme)

;; Port the inherited light theme with warm paper/ink colors. Override all
;; inherited faces to keep pure black/white and bright magenta out of this UI.
(define-color-theme "yeditor-light" ("emacs-light")
  (:display-background-mode :light)
  (:foreground "#3c3836")
  (:background "#f2e5bc")
  (:inactive-window-background "#ebdbb2")
  (region :foreground nil :background "#d5c4a1")
  (modeline :foreground "#3c3836" :background "#d5c4a1")
  (modeline-inactive :foreground "#665c54" :background "#ebdbb2")
  (syntax-string-attribute :foreground "#526200")
  (syntax-comment-attribute :foreground "#786b5d")
  (syntax-keyword-attribute :foreground "#8f3f71")
  (syntax-constant-attribute :foreground "#9d4a0b")
  (syntax-function-name-attribute :foreground "#076678")
  (syntax-variable-attribute :foreground "#8f3f71")
  (syntax-type-attribute :foreground "#427b58"))

(define-component le-dark-theme
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (let ((previous-source *palette-themes-function*))
               (list (install-variable comp '*palette-themes-function*
                                       (lambda ()
                                         (append '("Theme: dark" "Theme: light")
                                                 (funcall previous-source))))
                   (install-variable comp '*palette-theme-aliases*
                                     '(("Theme: dark" . "ergo-gruvbox-soft")
                                       ("Theme: light" . "yeditor-light")))
                   (install-theme comp "ergo-gruvbox-soft")))))
