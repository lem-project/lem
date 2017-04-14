(defpackage :lem.language-mode
  (:use :cl :lem)
  (:export
   :*language-mode-keymap*
   :line-comment
   :insertion-line-comment
   :language-mode
   :indent
   :newline-and-indent
   :indent-region))
(in-package :lem.language-mode)

(define-editor-variable line-comment nil)
(define-editor-variable insertion-line-comment nil)
(define-major-mode language-mode ()
    (:keymap *language-mode-keymap*)
  nil)

(define-key *language-mode-keymap* (kbd "C-i") 'indent)
(define-key *language-mode-keymap* (kbd "C-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "M-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "C-M-\\") 'indent-region)
(define-key *language-mode-keymap* (kbd "M-;") 'comment-or-uncomment-region)

(define-command indent (&optional (n 1)) ("p")
  (if (variable-value 'calc-indent-function)
      (indent-line (current-point))
      (self-insert n)))

(define-command newline-and-indent (n) ("p")
  (newline n)
  (indent))

(define-command indent-region (start end) ("r")
  (save-excursion
    (apply-region-lines start end 'indent-line)))

(defun space*-p (point)
  (with-point ((point point))
    (skip-whitespace-forward point t)
    (end-line-p point)))

(define-command comment-or-uncomment-region (arg) ("P")
  (if arg
      (uncomment-region)
      (comment-region)))

(define-command comment-region () ()
  (let ((line-comment (or (variable-value 'insertion-line-comment :buffer)
                          (variable-value 'line-comment :buffer))))
    (when line-comment
      (save-excursion
        (with-point ((start (current-point) :right-inserting)
                     (end (current-point) :left-inserting))
          (cond
            ((buffer-mark-p (current-buffer))
             (move-point start (region-beginning))
             (move-point end (region-end)))
            (t
             (line-start start)
             (line-end end)))
          (skip-whitespace-forward start)
          (let ((charpos (point-charpos start)))
            (loop
              (when (same-line-p start end)
                (cond ((space*-p start))
                      (t
                       (insert-string start line-comment)
                       (unless (space*-p end)
                         (insert-character end #\newline))))
                (return))
              (unless (space*-p start)
                (insert-string start line-comment))
              (line-offset start 1 charpos))))))))

(define-command uncomment-region () ()
  (let ((line-comment (variable-value 'line-comment :buffer))
        (insertion-line-comment (variable-value 'insertion-line-comment :buffer)))
    (when line-comment
      (with-point ((start (current-point) :right-inserting)
                   (end (current-point) :right-inserting))
        (cond
          ((buffer-mark-p (current-buffer))
           (move-point start (region-beginning))
           (move-point end (region-end)))
          (t
           (line-start start)
           (line-end end)))
        (character-offset start -1)
        (loop
          (unless (search-comment-start-backward end start)
            (return))
          (when (looking-at end line-comment)
            (let ((res (looking-at end insertion-line-comment)))
              (if res
                  (delete-character end (length res))
                  (loop :while (looking-at end line-comment)
                    :do (delete-character end (length line-comment)))))))))))
