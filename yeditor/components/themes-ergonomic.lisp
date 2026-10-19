;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/themes-ergonomic
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-components/palette #:*palette-themes-function*)
  (:export :le-themes-ergonomic
           :le-theme-extended
           :*ergonomic-themes*
           :*default-ergonomic-theme*
           :ergonomic-theme-names))
(in-package :yeditor-components/themes-ergonomic)

(defvar *ergonomic-themes* nil
  "Alist (name . roles) of the curated pack, in definition order. ROLES is a plist of
   #RRGGBB colors: :bg :alt :sel :fg :dim :comment :warning and the syntax roles.")

(defparameter *default-ergonomic-theme* "ergo-gruvbox-soft")

(defun ergonomic-theme-names ()
  (mapcar (function car) *ergonomic-themes*))

(defmacro define-ergonomic-theme (name &rest roles)
  "Register NAME in the curated pack and define the Lem color theme generated from ROLES."
  (flet ((r (key) (or (getf roles key) (error "theme ~A: missing role ~S" name key))))
    `(progn
       (setf *ergonomic-themes*
             (append (remove ,name *ergonomic-themes* :key (function car) :test (function equal))
                     (list (cons ,name (list ,@roles)))))
       (define-color-theme ,name ()
         (:display-background-mode :dark)
         (:foreground ,(r :fg))
         (:background ,(r :bg))
         (:inactive-window-background ,(r :bg))
         (region :foreground nil :background ,(r :sel))
         (syntax-warning-attribute :foreground ,(r :warning))
         (syntax-string-attribute :foreground ,(r :string))
         (syntax-comment-attribute :foreground ,(r :comment))
         (syntax-keyword-attribute :foreground ,(r :keyword))
         (syntax-constant-attribute :foreground ,(r :constant))
         (syntax-function-name-attribute :foreground ,(r :function))
         (syntax-variable-attribute :foreground ,(r :variable))
         (syntax-type-attribute :foreground ,(r :type))
         (syntax-builtin-attribute :foreground ,(r :builtin))
         (modeline :foreground ,(r :fg) :background ,(r :alt))
         (modeline-inactive :foreground ,(r :dim) :background ,(r :alt))
         (modeline-name-attribute :foreground ,(r :fg))
         (inactive-modeline-name-attribute :foreground ,(r :dim))
         (modeline-major-mode-attribute :foreground ,(r :function))
         (inactive-modeline-major-mode-attribute :foreground ,(r :dim))
         (modeline-minor-modes-attribute :foreground ,(r :dim))
         (inactive-modeline-minor-modes-attribute :foreground ,(r :dim))
         (modeline-position-attribute :foreground ,(r :dim) :background ,(r :alt))
         (inactive-modeline-position-attribute :foreground ,(r :dim) :background ,(r :alt))
         (modeline-posline-attribute :foreground ,(r :bg) :background ,(r :dim))
         (inactive-modeline-posline-attribute :foreground ,(r :bg) :background ,(r :alt))))))

;;; The curated pack (plan Cap. 12-A.2). Colors follow each theme family; deliberate
;;; departures to satisfy the policy: Gruvbox Soft uses the Gruvbox Material accents
;;; (the bright Gruvbox red reaches OKLCH chroma 0.22), and Tokyo Night Storm and
;;; One Dark use their brighter foreground (#c0caf5, #bbc2cf) to reach 7:1 contrast.

(define-ergonomic-theme "ergo-gruvbox-soft"
  :bg "#32302f" :alt "#3c3836" :sel "#504945" :fg "#d4be98" :dim "#a89984" :comment "#928374"
  :warning "#ea6962" :keyword "#ea6962" :string "#a9b665" :function "#a9b665"
  :constant "#d3869b" :type "#d8a657" :variable "#7daea3" :builtin "#e78a4e")

(define-ergonomic-theme "ergo-everforest-dark"
  :bg "#2d353b" :alt "#343f44" :sel "#475258" :fg "#d3c6aa" :dim "#859289" :comment "#859289"
  :warning "#e67e80" :keyword "#e67e80" :string "#a7c080" :function "#a7c080"
  :constant "#d699b6" :type "#dbbc7f" :variable "#7fbbb3" :builtin "#e69875")

(define-ergonomic-theme "ergo-kanagawa-wave"
  :bg "#1f1f28" :alt "#2a2a37" :sel "#2d4f67" :fg "#dcd7ba" :dim "#727169" :comment "#727169"
  :warning "#c34043" :keyword "#957fb8" :string "#98bb6c" :function "#7e9cd8"
  :constant "#ffa066" :type "#7aa89f" :variable "#e6c384" :builtin "#7fb4ca")

(define-ergonomic-theme "ergo-nord"
  :bg "#2e3440" :alt "#3b4252" :sel "#434c5e" :fg "#d8dee9" :dim "#616e88" :comment "#616e88"
  :warning "#bf616a" :keyword "#81a1c1" :string "#a3be8c" :function "#88c0d0"
  :constant "#b48ead" :type "#8fbcbb" :variable "#d8dee9" :builtin "#5e81ac")

(define-ergonomic-theme "ergo-catppuccin-frappe"
  :bg "#303446" :alt "#292c3c" :sel "#414559" :fg "#c6d0f5" :dim "#737994" :comment "#737994"
  :warning "#e78284" :keyword "#ca9ee6" :string "#a6d189" :function "#8caaee"
  :constant "#ef9f76" :type "#e5c890" :variable "#c6d0f5" :builtin "#81c8be")

(define-ergonomic-theme "ergo-catppuccin-macchiato"
  :bg "#24273a" :alt "#1e2030" :sel "#363a4f" :fg "#cad3f5" :dim "#6e738d" :comment "#6e738d"
  :warning "#ed8796" :keyword "#c6a0f6" :string "#a6da95" :function "#8aadf4"
  :constant "#f5a97f" :type "#eed49f" :variable "#cad3f5" :builtin "#8bd5ca")

(define-ergonomic-theme "ergo-rose-pine-moon"
  :bg "#232136" :alt "#2a273f" :sel "#44415a" :fg "#e0def4" :dim "#6e6a86" :comment "#6e6a86"
  :warning "#eb6f92" :keyword "#3e8fb0" :string "#f6c177" :function "#ea9a97"
  :constant "#c4a7e7" :type "#9ccfd8" :variable "#e0def4" :builtin "#eb6f92")

(define-ergonomic-theme "ergo-rose-pine"
  :bg "#191724" :alt "#1f1d2e" :sel "#403d52" :fg "#e0def4" :dim "#6e6a86" :comment "#6e6a86"
  :warning "#eb6f92" :keyword "#31748f" :string "#f6c177" :function "#ebbcba"
  :constant "#c4a7e7" :type "#9ccfd8" :variable "#e0def4" :builtin "#eb6f92")

(define-ergonomic-theme "ergo-tokyo-night-storm"
  :bg "#24283b" :alt "#1f2335" :sel "#2e3c64" :fg "#c0caf5" :dim "#565f89" :comment "#565f89"
  :warning "#f7768e" :keyword "#bb9af7" :string "#9ece6a" :function "#7aa2f7"
  :constant "#ff9e64" :type "#2ac3de" :variable "#c0caf5" :builtin "#7dcfff")

(define-ergonomic-theme "ergo-night-owl"
  :bg "#011627" :alt "#0b2942" :sel "#1d3b53" :fg "#d6deeb" :dim "#637777" :comment "#637777"
  :warning "#ef5350" :keyword "#c792ea" :string "#ecc48d" :function "#82aaff"
  :constant "#f78c6c" :type "#ffcb8b" :variable "#7fdbca" :builtin "#addb67")

(define-ergonomic-theme "ergo-one-dark"
  :bg "#282c34" :alt "#21252b" :sel "#3e4451" :fg "#bbc2cf" :dim "#5c6370" :comment "#5c6370"
  :warning "#e06c75" :keyword "#c678dd" :string "#98c379" :function "#61afef"
  :constant "#d19a66" :type "#e5c07b" :variable "#e06c75" :builtin "#56b6c2")

;;; Components

(define-component le-themes-ergonomic
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp (quote *palette-themes-function*)
                                     (function ergonomic-theme-names))
                   (install-theme comp *default-ergonomic-theme*))))

(define-component le-theme-extended
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-variable comp (quote *palette-themes-function*)
                                     (function color-theme-names)))))
