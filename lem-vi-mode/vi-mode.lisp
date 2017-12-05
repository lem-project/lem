(defpackage :lem-vi-mode
  (:use :cl
        :lem
        :lem-vi-mode.mode
        :lem-vi-mode.modeline
        :lem-vi-mode.state
        :lem-vi-mode.commands
        :lem-vi-mode.ex
        :lem.universal-argument)
  (:import-from :lem-vi-mode.ex-command
                :define-ex-command)
  (:export :*command-keymap*
           :*insert-keymap*
           :*enable-hook*
           :*disable-hook*
           :define-ex-command))
(in-package :lem-vi-mode)

(defvar *command-keymap* (make-keymap :name '*command-keymap* :insertion-hook 'undefined-key))
(defvar *insert-keymap* (make-keymap :name '*insert-keymap*))

(define-vi-state command (:keymap *command-keymap*))

(define-vi-state insert (:keymap *insert-keymap*)
  (message " -- INSERT --"))

(add-hook *enable-hook*
          (lambda ()
            (initialize-vi-modeline)
            (change-state 'command)))

(add-hook *disable-hook*
          (lambda ()
            (finalize-vi-modeline)))

(define-key *command-keymap* "0" 'vi-move-to-beginning-of-line/universal-argument-0)
(define-key *command-keymap* "1" 'universal-argument-1)
(define-key *command-keymap* "2" 'universal-argument-2)
(define-key *command-keymap* "3" 'universal-argument-3)
(define-key *command-keymap* "4" 'universal-argument-4)
(define-key *command-keymap* "5" 'universal-argument-5)
(define-key *command-keymap* "6" 'universal-argument-6)
(define-key *command-keymap* "7" 'universal-argument-7)
(define-key *command-keymap* "8" 'universal-argument-8)
(define-key *command-keymap* "9" 'universal-argument-9)
(define-key *command-keymap* "l" 'vi-forward-char)
(define-key *command-keymap* "h" 'vi-backward-char)
(define-key *command-keymap* "j" 'vi-next-line)
(define-key *command-keymap* "k" 'vi-previous-line)
(define-key *command-keymap* "w" 'vi-forward-word-begin)
(define-key *command-keymap* "b" 'vi-backward-word-begin)
(define-key *command-keymap* "W" 'vi-forward-word-begin-broad)
(define-key *command-keymap* "B" 'vi-backward-word-begin-broad)
(define-key *command-keymap* "e" 'vi-forward-word-end)
(define-key *command-keymap* "E" 'vi-forward-word-end-broad)
(define-key *command-keymap* "$" 'vi-move-to-end-of-line)
(define-key *command-keymap* "^" 'vi-back-to-indentation)
(define-key *command-keymap* "{" 'backward-paragraph)
(define-key *command-keymap* "}" 'forward-paragraph)
(define-key *command-keymap* "x" 'vi-delete-next-char)
(define-key *command-keymap* "X" 'vi-delete-previous-char)
(define-key *command-keymap* "u" 'undo)
(define-key *command-keymap* "C-r" 'redo)
(define-key *command-keymap* "C-f" 'next-page)
(define-key *command-keymap* "C-b" 'previous-page)
(define-key *command-keymap* "%" 'vi-move-to-matching-paren)
(define-key *command-keymap* "/" 'vi-search-forward)
(define-key *command-keymap* "?" 'vi-search-backward)
(define-key *command-keymap* "n" 'vi-search-next)
(define-key *command-keymap* "N" 'vi-search-previous)
(define-key *command-keymap* "g g" 'vi-goto-first-line)
(define-key *command-keymap* "G" 'vi-goto-line)
(define-key *command-keymap* "C-w s" 'split-active-window-vertically)
(define-key *command-keymap* "C-w C-s" 'split-active-window-vertically)
(define-key *command-keymap* "C-w w" 'other-window)
(define-key *command-keymap* "C-w C-w" 'other-window)
(define-key *command-keymap* "C-w q" 'vi-quit)

(define-key *command-keymap* "i" 'vi-insert)
(define-key *command-keymap* ":" 'vi-ex)

(define-key *insert-keymap* "escape" 'vi-normal)

(define-command vi-insert () ()
  (change-state 'insert))

(define-command vi-normal () ()
  (change-state 'command))
