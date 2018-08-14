(defpackage :lem-vi-mode.commands
  (:use :cl
        :lem
        :lem.universal-argument
        :lem.show-paren
        :lem-vi-mode.core
        :lem-vi-mode.word
        :lem-vi-mode.visual)
  (:export :vi-move-to-beginning-of-line/universal-argument-0
           :vi-forward-char
           :vi-backward-char
           :vi-next-line
           :vi-next-display-line
           :vi-previous-line
           :vi-previous-display-line
           :vi-forward-word-begin
           :vi-backward-word-begin
           :vi-forward-word-begin-broad
           :vi-backward-word-begin-broad
           :vi-forward-word-end
           :vi-forward-word-end-broad
           :vi-move-to-beginning-of-line
           :vi-move-to-end-of-line
           :vi-move-to-window-top
           :vi-move-to-window-middle
           :vi-move-to-window-bottom
           :vi-back-to-indentation
           :vi-indent
           :vi-substitute
           :vi-delete-next-char
           :vi-delete-previous-char
           :vi-delete
           :vi-delete-line
           :vi-clear
           :vi-clear-line
           :vi-join
           :vi-join-line
           :vi-yank
           :vi-yank-line
           :vi-paste-after
           :vi-paste-before
           :vi-replace-char
           :vi-move-to-matching-paren
           :vi-search-forward
           :vi-search-backward
           :vi-search-next
           :vi-search-previous
           :vi-goto-first-line
           :vi-goto-line
           :vi-find-char
           :vi-find-char-backward
           :vi-find-char-before
           :vi-find-char-backward-after
           :vi-write
           :vi-quit
           :vi-write-quit
           :vi-end-insert
           :vi-insert
           :vi-insert-line
           :vi-append
           :vi-append-line
           :vi-open-below
           :vi-open-adove
           :vi-normal
           :vi-keyboard-quit))
(in-package :lem-vi-mode.commands)

(defvar *cursor-offset* -1)

(defun bolp (point)
  (zerop (point-charpos point)))

(defun eolp (point)
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
            (1- len)))))

(defun goto-bol (point)
  (line-start point))

(defun goto-eol (point)
  (line-end point)
  (unless (bolp point)
    (character-offset point *cursor-offset*)))

(defun empty-line (point)
  (zerop (length (line-string point))))

;; Vim word
;; See http://vimdoc.sourceforge.net/htmldoc/motion.html#word
;; word = a sequence of letters, digits and underscores
(defun vi-word-char-p (char)
  (and (characterp char)
       (or (alphanumericp char)
           (char= char #\_))))

(defun vi-space-char-p (char)
  (and (characterp char)
       (or (char= char #\Space)
           (char= char #\Tab))))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (eolp p)
          (return)
          (character-offset p 1)))))

(define-command vi-backward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (bolp p)
          (return)
          (character-offset p -1)))))

(defun fall-within-line (point)
  (when (eolp point)
    (goto-eol point)))

(define-command vi-next-line (&optional (n 1)) ("p")
  (next-logical-line n)
  (fall-within-line (current-point)))

(define-command vi-next-display-line (&optional (n 1)) ("p")
  (next-line n)
  (fall-within-line (current-point)))

(define-command vi-previous-line (&optional (n 1)) ("p")
  (previous-logical-line n)
  (fall-within-line (current-point)))

(define-command vi-previous-display-line (&optional (n 1)) ("p")
  (previous-line n)
  (fall-within-line (current-point)))

(defun %vi-forward-word-begin (n)
  (when (< 0 n)
    (let ((p (current-point)))
      (cond
        ((vi-word-char-p (character-at p))
         (loop
           while (and (not (eolp p)) (vi-word-char-p (character-at p 1)))
           do (vi-forward-char))
         (character-offset p 1))
        ((vi-space-char-p (character-at p))
         (character-offset p 1)
         (skip-chars-forward p '(#\Space #\Tab)))
        (t
         (loop until (or (eolp p)
                         (vi-word-char-p (character-at p))
                         (vi-space-char-p (character-at p)))
               do (vi-forward-char))
         (when (eolp p)
           (character-offset p 1)))))
    (%vi-forward-word-begin (1- n))))

(define-command vi-forward-word-begin (&optional (n 1)) ("p")
  (when (and (eolp (current-point))
             (not (or *vi-delete-recursive*
                      *vi-yank-recursive*)))
    (line-offset (current-point) 1)
    (line-start (current-point)))
  (%vi-forward-word-begin n)
  (if (or *vi-delete-recursive*
          *vi-yank-recursive*)
      (skip-chars-forward (current-point) '(#\Space #\Tab))
      (skip-chars-forward (current-point) '(#\Space #\Tab #\Newline))))

(define-command vi-backward-word-begin (&optional (n 1)) ("p")
  (when (< 0 n)
    (let ((p (current-point)))
      (cond
        ((vi-word-char-p (character-at p 0))
         (vi-backward-char)
         (loop
           while (and (not (bolp p)) (vi-word-char-p (character-at p -1)))
           do (vi-backward-char)))
        (t
         (loop until (or (bolp p) (vi-word-char-p (character-at p 0)))
               do (vi-backward-char))
         (when (bolp p)
           (vi-previous-line)
           (vi-move-to-end-of-line)
           (vi-backward-word-begin n)))))
    (vi-backward-word-begin (1- n))))

(define-command vi-forward-word-begin-broad (&optional (n 1)) ("p")
  (forward-word-begin (current-point) n t))

(define-command vi-backward-word-begin-broad (&optional (n 1)) ("p")
  (backward-word-begin (current-point) n t))

(define-command vi-forward-word-end (&optional (n 1)) ("p")
  (character-offset (current-point) 1)
  (skip-chars-forward (current-point) (lambda (char)
                                        (or (char= char #\Newline)
                                            (vi-space-char-p char))))
  (%vi-forward-word-begin n)
  (vi-backward-char))

(define-command vi-forward-word-end-broad (&optional (n 1)) ("p")
  (forward-word-end (current-point) n t))

(define-command vi-move-to-beginning-of-line () ()
  (goto-bol (current-point)))

(define-command vi-move-to-end-of-line () ()
  (goto-eol (current-point)))

(define-command vi-move-to-window-top () ()
  (move-point (current-point) (window-view-point (current-window))))

(define-command vi-move-to-window-middle () ()
  (vi-move-to-window-top)
  (next-line (floor (/ (- (window-height (current-window)) 2) 2))))

(define-command vi-move-to-window-bottom () ()
  (vi-move-to-window-top)
  (next-line (- (window-height (current-window)) 2)))

(define-command vi-back-to-indentation () ()
  (back-to-indentation-command))

(defvar *vi-indent-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-indent (&optional (n 1)) ("p")
    (cond (*vi-indent-recursive*
           (indent-line (current-point))
           (throw tag t))
          ((visual-p)
           (apply-visual-range #'indent-region)
           (vi-visual-end))
          (t
           (let ((command (lookup-keybind (read-key))))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-indent-recursive* t)
                       (*cursor-offset* 0))
                   (catch tag
                     ;; Ignore End of Buffer error and continue the deletion.
                     (ignore-errors (call-command command n))
                     (with-point ((end (current-point)))
                       (when (point< end start)
                         (rotatef start end))
                       (indent-region start end)))))))))))

(define-command vi-substitute (&optional (n 1)) ("p")
  (vi-delete-next-char n)
  (change-state 'insert))

(define-command vi-delete-next-char (&optional (n 1)) ("p")
  (unless (empty-line (current-point))
    (delete-next-char n)
    (kill-append "" nil '(:vi-nolf))
    (fall-within-line (current-point))))

(define-command vi-delete-previous-char (&optional (n 1)) ("p")
  (unless (bolp (current-point))
    (delete-previous-char n)
    (kill-append "" nil '(:vi-nolf))))

(defvar *vi-delete-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-delete (&optional (n 1)) ("p")
    (cond (*vi-delete-recursive*
           (with-point ((start (current-point))
                        (end (current-point)))
             (line-start start)
             (line-end end)
             (let ((eob (not (character-offset end 1))))
               (kill-region start end)
               (if eob
                   (unless *vi-clear-recursive*
                     (delete-previous-char))
                   (when *vi-clear-recursive*
                     (insert-character (current-point) #\Newline)
                     (vi-previous-line)))))
           (throw tag t))
          ((visual-p)
           (with-output-to-string (out)
             (let ((*cursor-offset* 0))
               (apply-visual-range (lambda (start end)
                                     (unless (point< start end)
                                       (rotatef start end))
                                     (when (visual-line-p)
                                       (character-offset end 1))
                                     (write-string (points-to-string start end) out)
                                     (delete-between-points start end))))
             (unless (continue-flag :kill)
               (kill-ring-new))
             (kill-push (get-output-stream-string out))
             (unless (visual-line-p)
               (kill-append "" nil '(:vi-nolf))))
           (vi-visual-end))
          (t
           (let ((command (loop with uarg = nil
                                for key = (read-key)
                                while (char<= #\0 (key-to-char key) #\9)
                                do (setf uarg (+ (* (or uarg 0) 10)
                                                 (- (char-code (key-to-char key))
                                                    (char-code #\0))))
                                finally
                                   (when uarg (setf n uarg))
                                   (return (lookup-keybind key)))))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-delete-recursive* t)
                       (*cursor-offset* 0))
                   (catch tag
                     ;; Ignore End of Buffer error and continue the deletion.
                     (ignore-errors (call-command command n))
                     (with-point ((end (current-point)))
                       (when (point< end start)
                         (rotatef start end)
                         (character-offset end 1))
                       (when (point/= start end)
                         (cond
                           ((same-line-p start end)
                            (kill-region start end)
                            (kill-append "" nil '(:vi-nolf)))
                           (t
                            (unless (or (eq command 'vi-forward-word-end)
                                        (eq command 'vi-forward-word-begin))
                              (line-start start)
                              (line-end end))
                            (character-offset end 1)
                            (kill-region start end)))))))
                 (unless *vi-clear-recursive*
                   (fall-within-line (current-point))))))))))

(define-command vi-delete-line () ()
  (cond ((visual-block-p)
         (apply-visual-range (lambda (start end)
                               (kill-region start (line-end end)))))
        ((visual-p)
         (apply-visual-range (lambda (start end)
                               (kill-region (line-start start) (line-end end)))))
        (t
         (with-point ((start (current-point))
                      (end (current-point)))
           (kill-region start (line-end end)))
         (kill-append "" nil '(:vi-nolf))
         (unless *vi-clear-recursive*
           (fall-within-line (current-point))))))

(defvar *vi-clear-recursive* nil)
(define-command vi-clear () ()
  (let ((*vi-clear-recursive* t))
    (vi-delete))
  (vi-insert))

(define-command vi-clear-line () ()
  (let ((*vi-clear-recursive* t))
    (vi-delete-line))
  (vi-insert))

(define-command vi-join () ()
  (move-to-end-of-line)
  (delete-next-char))

(define-command vi-join-line () ()
  (move-to-end-of-line)
  (let ((p (current-point))
        (s '(#\space #\tab #\newline)))
    (delete-character p (- (skip-chars-forward p s)))
    (unless (syntax-space-char-p (character-at p -1))
      (insert-character p #\space))))

(defvar *vi-yank-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-yank (&optional (n 1)) ("p")
    (cond (*vi-yank-recursive*
           (with-point ((start (current-point))
                        (end (current-point)))
             (line-start start)
             (line-offset end n)
             (line-start end)
             (copy-region start end)
             (throw tag t)))
          ((visual-p)
           (with-output-to-string (out)
             (let ((*cursor-offset* 0))
               (apply-visual-range (lambda (start end)
                                     (unless (point< start end)
                                       (rotatef start end))
                                     (when (visual-line-p)
                                       (character-offset end 1))
                                     (write-string (points-to-string start end) out))))
             (unless (continue-flag :kill)
               (kill-ring-new))
             (kill-push (get-output-stream-string out))
             (unless (visual-line-p)
               (kill-append "" nil '(:vi-nolf))))
           (vi-visual-end))
          (t
           (let ((command (lookup-keybind (read-key))))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-yank-recursive* t)
                       (*cursor-offset* 0))
                   (catch tag
                     ;; Ignore End of Buffer error and continue the deletion.
                     (ignore-errors (call-command command n))
                     (with-point ((end (current-point)))
                       (when (point< end start)
                         (rotatef start end)
                         (character-offset end 1))
                       (when (point/= start end)
                         (cond
                           ((same-line-p start end)
                            (copy-region start end)
                            (kill-append "" nil '(:vi-nolf)))
                           (t
                            (line-start start)
                            (line-end end)
                            (character-offset end 1)
                            (copy-region start end)))))))
                 (move-point (current-point) start))))))))

(define-command vi-paste-after () ()
  (multiple-value-bind (string type)
      (lem::current-kill-ring)
    (declare (ignore string))
    (if (eql type :vi-nolf)
        (character-offset (current-point) 1)
        (progn
          (line-end (current-point))
          (or (character-offset (current-point) 1)
              (insert-character (current-point) #\Newline))))
    (yank)
    (character-offset (current-point) -1)
    (unless (eql type :vi-nolf)
      (line-start (current-point)))))

(define-command vi-paste-before () ()
  (multiple-value-bind (string type)
      (lem::current-kill-ring)
    (declare (ignore string))
    (unless (eql type :vi-nolf)
      (line-start (current-point)))
    (yank)))

(define-command vi-replace-char (c)
    ((list (key-to-char (read-key))))
  (delete-next-char 1)
  (insert-character (current-point) c)
  (backward-char 1))

(defun forward-matching-paren (p &optional skip)
  (with-point ((p p))
    (when (or skip (syntax-open-paren-char-p (character-at p)))
      (scan-lists p 1 0)
      (character-offset p *cursor-offset*))))

(defun backward-matching-paren (p)
  (when (syntax-closed-paren-char-p (character-at p))
    (scan-lists (character-offset (copy-point p :temporary) 1) -1 0)))

(define-command vi-move-to-matching-paren () ()
  (alexandria:when-let ((p (or (backward-matching-paren (current-point))
                               (forward-matching-paren (current-point) t))))

    (move-point (current-point) p)))

(let ((old-forward-matching-paren)
      (old-backward-matching-paren))
  (defun on-matching-paren ()
    (setf old-forward-matching-paren (variable-value 'forward-matching-paren :global))
    (setf old-backward-matching-paren (variable-value 'backward-matching-paren :global))
    (setf (variable-value 'forward-matching-paren :global) 'forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) 'backward-matching-paren))
  (defun off-matching-paren ()
    (setf (variable-value 'forward-matching-paren :global) old-forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) old-backward-matching-paren)))

(add-hook *enable-hook* 'on-matching-paren)
(add-hook *disable-hook* 'off-matching-paren)

(define-command vi-search-forward () ()
  (lem.isearch:isearch-forward-regexp "/"))

(define-command vi-search-backward () ()
  (lem.isearch:isearch-backward-regexp "?"))

(define-command vi-search-next (n) ("p")
  (lem.isearch:isearch-next-highlight n))

(define-command vi-search-previous (n) ("p")
  (lem.isearch:isearch-prev-highlight n))

(define-command vi-goto-first-line () ()
  (move-to-beginning-of-buffer)
  (skip-whitespace-forward (current-point) t))

(define-command vi-goto-line (arg) ("P")
  (if (null arg)
      (progn
        (move-to-end-of-buffer)
        (line-start (current-point)))
      (goto-line arg))
  (skip-whitespace-forward (current-point) t))

(define-command vi-find-char () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (with-point ((p (current-point))
                 (limit (current-point)))
      (character-offset p 1)
      (line-end limit)
      (when (search-forward p (string c) limit)
        (character-offset p -1)
        (move-point (current-point) p)))))

(define-command vi-find-char-backward () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (with-point ((p (current-point))
                 (limit (current-point)))
      (line-start limit)
      (when (search-backward p (string c) limit)
        (move-point (current-point) p)))))

(define-command vi-find-char-before () ()
  (vi-find-char)
  (vi-backward-char))

(define-command vi-find-char-backward-after () ()
  (vi-find-char-backward)
  (vi-forward-char))

(define-command vi-write () ()
  (lem:write-file (lem:buffer-filename (lem:current-buffer))))

(define-command vi-quit (&optional (ask t)) ((list t))
  (if (one-window-p)
      (exit-lem ask)
      (delete-current-window)))

(define-command vi-write-quit () ()
  (vi-write)
  (vi-quit nil))

(define-command vi-end-insert () ()
  (change-state 'command)
  (vi-backward-char 1))

(define-command vi-insert () ()
  (change-state 'insert))

(define-command vi-insert-line () ()
  (move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t)
  (change-state 'insert))

(define-command vi-append () ()
  (let ((p (current-point)))
    (unless (or (end-line-p p)
                (end-buffer-p p))
      (forward-char 1))
    (change-state 'insert)))

(define-command vi-append-line () ()
  (move-to-end-of-line)
  (change-state 'insert))

(define-command vi-open-below () ()
  (let* ((p (current-point))
         (column (with-point ((p (current-point)))
                   (point-column (or (and (line-offset p 1)
                                          (back-to-indentation p))
                                     (line-start p))))))
    (line-end p)
    (insert-character p #\newline)
    (move-to-column p column t)
    (change-state 'insert)))

(define-command vi-open-adove () ()
  (line-start (current-point))
  (open-line 1)
  (change-state 'insert))

(define-command vi-normal () ()
  (change-state 'command))

(define-command vi-keyboard-quit () ()
  (vi-visual-end)
  (keyboard-quit))
