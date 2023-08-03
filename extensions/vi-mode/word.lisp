(defpackage :lem-vi-mode/word
  (:use :cl :lem)
  (:export :forward-word-begin
           :backward-word-begin
           :forward-word-end))
(in-package :lem-vi-mode/word)

(defun hiragana-p (c)
  (<= #x3042 (char-code c) #x3093))

(defun katakana-p (c)
  (<= #x30A2 (char-code c) #x30F3))

(defun kanji-p (c)
  (or (<= #x4E00 (char-code c) #x9FFF)
      (find c "仝々〆〇ヶ")))

(defun paren-p (c)
  (or (syntax-open-paren-char-p c)
      (syntax-closed-paren-char-p c)))

(defun word-type (c largep)
  (flet ((w (small large) (if largep large small)))
    (cond ((syntax-space-char-p c)
           (w :space :space))
          ((hiragana-p c)
           (w :hiragana :symbol))
          ((katakana-p c)
           (w :katakana :symbol))
          ((kanji-p c)
           (w :kanji :symbol))
          ((or (syntax-open-paren-char-p c)
               (syntax-closed-paren-char-p c))
           (w :paren :symbol))
          (t
           (w :symbol :symbol)))))

(defun forward-word-begin (point n largep)
  (loop :repeat n
        :until (end-buffer-p point)
        :do (let ((wt (word-type (character-at point) largep)))
              (skip-chars-forward point (lambda (c) (eql wt (word-type c largep))))
              (skip-whitespace-forward point))))

(defun backward-word-begin (point n largep)
  (loop :repeat n
        :until (start-buffer-p point)
        :do (skip-whitespace-backward point)
            (let ((wt (word-type (character-at point -1) largep)))
              (skip-chars-backward point (lambda (c) (eql wt (word-type c largep)))))))

(defun forward-word-end (point n largep)
  (loop :repeat n
        :until (end-buffer-p point)
        :do (skip-whitespace-forward point)
            (let ((wt (word-type (character-at point) largep)))
              (skip-chars-forward point (lambda (c) (eql wt (word-type c largep)))))))
