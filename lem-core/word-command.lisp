(in-package :lem)

(export '(next-word
          prev-word
          delete-word
          backward-delete-word
          downcase-region
          uppercase-region
          capitalize-word
          lowercase-word
          uppercase-word
          forward-paragraph
          backward-paragraph
          kill-paragraph
          count-words))

(defun word-type (char)
  (when (characterp char)
    (cond ((char<= (code-char 12354) ;#\HIRAGANA_LETTER_A
                   char
                   (code-char 12435) ;#\HIRAGANA_LETTER_N
                   )
           :hiragana)
          ((char<= (code-char 12450) ;#\KATAKANA_LETTER_A
                   char
                   (code-char 12531) ;#\KATAKANA_LETTER_N
                   )
           :katakana)
          ((or (<= #x4E00
                   (char-code char)
                   #x9FFF)
               (find char "仝々〆〇ヶ"))
           :kanji)
          ((alphanumericp char)
           :alphanumeric))))

(defun word-offset (point n)
  (multiple-value-bind (skip-chars-forward
                        char-offset
                        end-buffer-p)
      (if (plusp n)
          (values #'skip-chars-forward
                  0
                  #'end-buffer-p)
          (values #'skip-chars-backward
                  -1
                  #'start-buffer-p))
    (loop :repeat (abs n)
          :do (funcall skip-chars-forward point (complement #'word-type))
              (when (funcall end-buffer-p point)
                (return))
              (let ((type (word-type (character-at point char-offset))))
                (if (null type)
                    (return nil)
                    (funcall skip-chars-forward
                             point
                             (lambda (c) (eql type (word-type c))))))
          :finally (return point))))

(define-key *global-keymap* "M-f" 'next-word)
(define-key *global-keymap* "C-right" 'next-word)
(define-command next-word (n) ("p")
  (word-offset (current-point) n))

(define-key *global-keymap* "M-b" 'prev-word)
(define-key *global-keymap* "C-left" 'prev-word)
(define-command prev-word (n) ("p")
  (word-offset (current-point) (- n)))

(define-key *global-keymap* "M-d" 'delete-word)
(define-key *global-keymap* "C-dc" 'delete-word)
(define-command delete-word (n) ("p")
  (with-point ((point (current-point) :right-inserting))
    (let ((start (current-point))
          (end (word-offset point n)))
      (if (point< start end)
          (kill-region start end)
          (kill-region end start)))))

(define-key *global-keymap* "C-M-h" 'backward-delete-word)
(define-key *global-keymap* "M-[backspace]" 'backward-delete-word)
(define-command backward-delete-word (n) ("p")
  (delete-word (- n)))

(defun case-region-aux (start end case-fun replace-char-p)
  (save-excursion
    (with-point ((point start :left-inserting))
      (loop :while (and (point< point end)
                        (not (end-buffer-p point)))
            :do (let ((c (character-at point 0)))
                  (cond ((char= c #\newline)
                         (character-offset point 1))
                        ((funcall replace-char-p c)
                         (delete-character point)
                         (insert-character point (funcall case-fun c)))
                        (t
                         (character-offset point 1))))))))

(define-key *global-keymap* "C-x C-l" 'downcase-region)
(define-command downcase-region (start end) ("r")
  (case-region-aux start end #'char-downcase #'identity))

(define-key *global-keymap* "C-x C-u" 'uppercase-region)
(define-command uppercase-region (start end) ("r")
  (case-region-aux start end #'char-upcase #'identity))

(defun case-word-aux (point n replace-char-p first-case rest-case)
  (dotimes (_ n)
    (skip-chars-forward point (complement #'word-type))
    (when (end-buffer-p point)
      (return))
    (let ((c (character-at point)))
      (delete-character point)
      (insert-character point (funcall first-case c))
      (with-point ((end (word-offset (copy-point point :temporary) 1) :left-inserting))
        (case-region-aux point
                         end
                         rest-case
                         replace-char-p)
        (move-point point end)))))

(define-key *global-keymap* "M-c" 'capitalize-word)
(define-command capitalize-word (&optional (n 1)) ("p")
  (case-word-aux (current-point) n #'alphanumericp #'char-upcase #'char-downcase))

(define-key *global-keymap* "M-l" 'lowercase-word)
(define-command lowercase-word (&optional (n 1)) ("p")
  (case-word-aux (current-point) n #'alphanumericp #'char-downcase #'char-downcase))

(define-key *global-keymap* "M-u" 'uppercase-word)
(define-command uppercase-word (&optional (n 1)) ("p")
  (case-word-aux (current-point) n #'alphanumericp #'char-upcase #'char-upcase))

(define-key *global-keymap* "M-}" 'forward-paragraph)
(define-command forward-paragraph (&optional (n 1)) ("p")
  (let ((point (current-point))
        (dir (if (plusp n) 1 -1)))
    (dotimes (_ (abs n))
      (loop :while (blank-line-p point)
            :do (unless (line-offset point dir)
                  (return-from forward-paragraph)))
      (loop :until (blank-line-p point)
            :do (unless (line-offset point dir)
                  (when (plusp dir) (buffer-end point))
                  (return-from forward-paragraph))))))

(define-key *global-keymap* "M-{" 'backward-paragraph)
(define-command backward-paragraph (&optional (n 1)) ("p")
  (forward-paragraph (- n)))

(define-key *global-keymap* "M-k" 'kill-paragraph)
(define-command kill-paragraph (&optional (n 1)) ("p")
  (dotimes (_ n t)
    (with-point ((start (current-point) :right-inserting))
      (forward-paragraph)
      (kill-region start
                   (current-point)))))

(defun %count-words (start end)
  (save-excursion
    (let ((wnum 0))
      (loop :for point := (copy-point start) :then (word-offset point 1)
            :while (and point (point< point end))
            :do (incf wnum))
      wnum)))

(define-key *global-keymap* "M-=" 'count-words)
(define-command count-words () ()
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end)
        (if (buffer-mark-p buffer)
            (values (region-beginning buffer)
                    (region-end buffer))
            (values (buffer-start-point buffer)
                    (buffer-end-point buffer)))
      (let ((chnum (length (points-to-string start end)))
            (wnum (%count-words start end))
            (linum (count-lines start end)))
        (message (format nil "~a has ~d lines, ~d words and ~d characters."
                         (if (buffer-mark-p buffer)
                             "Region"
                             "Buffer")
                         linum wnum chnum))))))
