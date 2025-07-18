(defpackage :lem-vi-mode/binds
  (:use :cl
        :lem
        :lem/universal-argument
        :lem/abbrev
        :lem-vi-mode/states
        :lem-vi-mode/commands
        :lem-vi-mode/ex
        :lem-vi-mode/visual)
  (:import-from :lem-core
                :keymap-table)
  (:import-from :lem/prompt-window
                :prompt-previous-history
                :prompt-next-history))
(in-package :lem-vi-mode/binds)

(define-key *motion-keymap* "0" 'vi-move-to-beginning-of-line/universal-argument-0)
(define-key *motion-keymap* "1" 'universal-argument-1)
(define-key *motion-keymap* "2" 'universal-argument-2)
(define-key *motion-keymap* "3" 'universal-argument-3)
(define-key *motion-keymap* "4" 'universal-argument-4)
(define-key *motion-keymap* "5" 'universal-argument-5)
(define-key *motion-keymap* "6" 'universal-argument-6)
(define-key *motion-keymap* "7" 'universal-argument-7)
(define-key *motion-keymap* "8" 'universal-argument-8)
(define-key *motion-keymap* "9" 'universal-argument-9)
(define-key *motion-keymap* "l" 'vi-forward-char)
(define-key *motion-keymap* "Space" 'vi-forward-char)
(define-key *motion-keymap* "h" 'vi-backward-char)
(define-key *motion-keymap* "j" 'vi-next-line)
(define-key *motion-keymap* "+" 'vi-next-line)
(define-key *motion-keymap* "k" 'vi-previous-line)
(define-key *motion-keymap* "-" 'vi-previous-line)
(define-key *motion-keymap* "g j" 'vi-next-display-line)
(define-key *motion-keymap* "g k" 'vi-previous-display-line)
(define-key *motion-keymap* "w" 'vi-forward-word-begin)
(define-key *motion-keymap* "b" 'vi-backward-word-begin)
(define-key *motion-keymap* "W" 'vi-forward-word-begin-broad)
(define-key *motion-keymap* "B" 'vi-backward-word-begin-broad)
(define-key *motion-keymap* "e" 'vi-forward-word-end)
(define-key *motion-keymap* "E" 'vi-forward-word-end-broad)
(define-key *motion-keymap* "g ^" 'move-to-beginning-of-line)
(define-key *motion-keymap* "g 0" 'move-to-beginning-of-line)
(define-key *motion-keymap* "g $" 'move-to-end-of-line)
(define-key *motion-keymap* "$" 'vi-move-to-end-of-line)
(define-key *motion-keymap* "g _" 'vi-move-to-last-nonblank)
(define-key *motion-keymap* "H" 'vi-move-to-window-top)
(define-key *motion-keymap* "M" 'vi-move-to-window-middle)
(define-key *motion-keymap* "L" 'vi-move-to-window-bottom)
(define-key *motion-keymap* "C-d" 'vi-scroll-down)
(define-key *motion-keymap* "C-u" 'vi-scroll-up)
(define-key *motion-keymap* "^" 'vi-back-to-indentation)
(define-key *motion-keymap* "_" 'vi-back-to-indentation)
(define-key *motion-keymap* "{" 'backward-paragraph)
(define-key *motion-keymap* "}" 'forward-paragraph)
(define-key *motion-keymap* "C-f" 'next-page)
(define-key *motion-keymap* "C-b" 'previous-page)
(define-key *motion-keymap* "%" 'vi-move-to-matching-item)
(define-key *motion-keymap* "/" 'vi-search-forward)
(define-key *motion-keymap* "?" 'vi-search-backward)
(define-key *motion-keymap* "n" 'vi-search-next)
(define-key *motion-keymap* "N" 'vi-search-previous)
(define-key *motion-keymap* "*" 'vi-search-forward-symbol-at-point)
(define-key *motion-keymap* "#" 'vi-search-backward-symbol-at-point)
(define-key *motion-keymap* "g g" 'vi-goto-first-line)
(define-key *motion-keymap* "g e" 'vi-backward-word-end)
(define-key *motion-keymap* "g E" 'vi-backward-word-end-broad)
(define-key *motion-keymap* "G" 'vi-goto-line)
(define-key *motion-keymap* "|" 'vi-goto-column)
(define-key *motion-keymap* "Return" 'vi-return)
(define-key *motion-keymap* "f" 'vi-find-char)
(define-key *motion-keymap* "F" 'vi-find-char-backward)
(define-key *motion-keymap* "t" 'vi-find-char-before)
(define-key *motion-keymap* "T" 'vi-find-char-backward-after)
(define-key *motion-keymap* ";" 'vi-find-char-repeat)
(define-key *motion-keymap* "," 'vi-find-char-repeat-backward)
(define-key *normal-keymap* "z z" 'vi-scroll-line-to-center)
(define-key *normal-keymap* "z ." 'vi-scroll-line-to-center-back-to-indentation)
(define-key *normal-keymap* "z t" 'vi-scroll-line-to-top)
(define-key *normal-keymap* "z Return" 'vi-scroll-line-to-top-back-to-indentation)
(define-key *normal-keymap* "z b" 'vi-scroll-line-to-bottom)
(define-key *normal-keymap* "z -" 'vi-scroll-line-to-bottom-back-to-indentation)
(define-key *normal-keymap* "z +" 'vi-scroll-bottom-line-to-top)
(define-key *normal-keymap* "z ^" 'vi-scroll-top-line-to-bottom)
(define-key *normal-keymap* "Z Z" 'vi-write-quit)
(define-key *normal-keymap* "Z Q" 'vi-quit)

;; Window Management
(define-key *motion-keymap* "C-w h" 'vi-window-move-left)
(define-key *motion-keymap* "C-w C-h" 'vi-window-move-left)
(define-key *motion-keymap* "C-w j" 'vi-window-move-down)
(define-key *motion-keymap* "C-w C-j" 'vi-window-move-down)
(define-key *motion-keymap* "C-w k" 'vi-window-move-up)
(define-key *motion-keymap* "C-w C-k" 'vi-window-move-up)
(define-key *motion-keymap* "C-w l" 'vi-window-move-right)
(define-key *motion-keymap* "C-w C-l" 'vi-window-move-right)
(define-key *motion-keymap* "C-w s" 'vi-window-split-vertically)
(define-key *motion-keymap* "C-w S" 'vi-window-split-vertically)
(define-key *motion-keymap* "C-w C-s" 'vi-window-split-vertically)
(define-key *motion-keymap* "C-w v" 'vi-window-split-horizontally)
(define-key *motion-keymap* "C-w C-v" 'vi-window-split-horizontally)
(define-key *motion-keymap* "C-w n" 'vi-window-split-vertically-new)
(define-key *motion-keymap* "C-w C-n" 'vi-window-split-vertically-new)
(define-key *motion-keymap* "C-w w" 'next-window)
(define-key *motion-keymap* "C-w C-w" 'next-window)
(define-key *motion-keymap* "C-w p" 'previous-window)
(define-key *motion-keymap* "C-w C-p" 'previous-window)
(define-key *motion-keymap* "C-w q" 'vi-quit)
(define-key *motion-keymap* "C-w c" 'vi-close)
(define-key *motion-keymap* "C-w o" 'delete-other-windows)
(define-key *motion-keymap* "C-w C-o" 'delete-other-windows)

(define-key *motion-keymap* "C-o" 'vi-jump-back)
(define-key *motion-keymap* "C-i" 'vi-jump-next)
(define-key *motion-keymap* "'" 'vi-goto-mark)
(define-key *motion-keymap* ":" 'vi-ex)

(define-key *motion-keymap* "v" 'vi-visual-char)
(define-key *motion-keymap* "V" 'vi-visual-line)
(define-key *motion-keymap* "C-v" 'vi-visual-block)

(define-key *normal-keymap* "i" 'vi-insert)
(define-key *normal-keymap* "I" 'vi-insert-line)
(define-key *normal-keymap* "a" 'vi-append)
(define-key *normal-keymap* "A" 'vi-append-line)
(define-key *normal-keymap* "o" 'vi-open-below)
(define-key *normal-keymap* "O" 'vi-open-above)
(define-key *normal-keymap* "." 'vi-repeat)
(define-key *normal-keymap* "=" 'vi-indent)
(define-key *normal-keymap* "s" 'vi-substitute)
(define-key *normal-keymap* "S" 'vi-change-whole-line)
(define-key *normal-keymap* "x" 'vi-delete-next-char)
(define-key *normal-keymap* "X" 'vi-delete-previous-char)
(define-key *normal-keymap* "d" 'vi-delete)
(define-key *normal-keymap* "D" 'vi-delete-line)
(define-key *normal-keymap* "c" 'vi-change)
(define-key *normal-keymap* "C" 'vi-change-line)
(define-key *normal-keymap* "m" 'vi-set-mark)
(define-key *normal-keymap* "M-m" 'vi-delete-mark)
(define-key *normal-keymap* "g J" 'vi-join)
(define-key *normal-keymap* "J" 'vi-join-line)
(define-key *normal-keymap* "y" 'vi-yank)
(define-key *normal-keymap* "Y" 'vi-yank-line)
(define-key *normal-keymap* "p" 'vi-paste-after)
(define-key *normal-keymap* "P" 'vi-paste-before)
(define-key *normal-keymap* "r" 'vi-replace-char)
(define-key *normal-keymap* "R" 'vi-replace)
(define-key *normal-keymap* "g U" 'vi-upcase)
(define-key *normal-keymap* "g u" 'vi-downcase)
(define-key *normal-keymap* "g ~" 'vi-swapcase)
(define-key *normal-keymap* "~" 'vi-swapcase-and-forward-char)
(define-key *normal-keymap* "u" 'vi-undo)
(define-key *normal-keymap* "C-r" 'vi-redo)
(define-key *motion-keymap* 'delete-previous-char 'vi-backward-char)
(define-key *motion-keymap* 'self-insert 'undefined-key)
(define-key *normal-keymap* "q" 'vi-record-macro)
(define-key *normal-keymap* "@" 'vi-execute-macro)
(define-key *normal-keymap* "Q" 'vi-execute-last-recorded-macro)

(define-key *insert-keymap* "Escape" 'vi-end-insert)
(define-key *insert-keymap* "C-p" 'abbrev)
(define-key *insert-keymap* "C-w" 'vi-kill-last-word)
(define-key *insert-keymap* "Shift-Up" 'previous-page)
(define-key *insert-keymap* "Shift-Down" 'next-page)
(define-key *insert-keymap* "Shift-Left" 'vi-backward-word-begin)
(define-key *insert-keymap* "Shift-Right" 'vi-forward-word-begin)

(define-key *normal-keymap* "C-p" 'yank-pop)
(define-key *normal-keymap* "C-n" 'yank-pop-next)

(define-key *motion-keymap* "C-g" 'vi-keyboard-quit)
(define-key *inactive-keymap* "Escape" 'vi-keyboard-quit)

(define-key *visual-keymap* "Escape" 'vi-visual-end)
(define-key *visual-keymap* "A" 'vi-visual-append)
(define-key *visual-keymap* "I" 'vi-visual-insert)
(define-key *visual-keymap* "U" 'vi-upcase)
(define-key *visual-keymap* "u" 'vi-downcase)
(define-key *visual-keymap* "o" 'vi-visual-swap-points)
(define-key *visual-keymap* "O" 'vi-visual-opposite-side)

(define-key *replace-char-state-keymap* "C-g" 'escape)
(define-key *replace-char-state-keymap* "Escape" 'escape)

(define-key *ex-keymap* "C-p" 'prompt-previous-history)
(define-key *ex-keymap* "C-n" 'prompt-next-history)

(define-key *outer-text-objects-keymap* "w" 'vi-a-word)
(define-key *inner-text-objects-keymap* "w" 'vi-inner-word)
(define-key *outer-text-objects-keymap* "W" 'vi-a-broad-word)
(define-key *inner-text-objects-keymap* "W" 'vi-inner-broad-word)
(define-key *outer-text-objects-keymap* "\"" 'vi-a-double-quote)
(define-key *inner-text-objects-keymap* "\"" 'vi-inner-double-quote)
(define-key *outer-text-objects-keymap* "(" 'vi-a-paren)
(define-key *outer-text-objects-keymap* ")" 'vi-a-paren)
(define-key *outer-text-objects-keymap* "b" 'vi-a-paren)
(define-key *inner-text-objects-keymap* "(" 'vi-inner-paren)
(define-key *inner-text-objects-keymap* ")" 'vi-inner-paren)
(define-key *inner-text-objects-keymap* "b" 'vi-inner-paren)
(define-key *outer-text-objects-keymap* "p" 'vi-a-paragraph)
(define-key *inner-text-objects-keymap* "p" 'vi-inner-paragraph)

(setf (gethash (lem:make-key :sym "a") (keymap-table *operator-keymap*))
      (keymap-table *outer-text-objects-keymap*))
(setf (gethash (lem:make-key :sym "i") (keymap-table *operator-keymap*))
      (keymap-table *inner-text-objects-keymap*))
(setf (gethash (lem:make-key :sym "a") (keymap-table *visual-keymap*))
      (keymap-table *outer-text-objects-keymap*))
(setf (gethash (lem:make-key :sym "i") (keymap-table *visual-keymap*))
      (keymap-table *inner-text-objects-keymap*))
