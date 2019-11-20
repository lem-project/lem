(defpackage :lem-vi-mode.commands
  (:use :cl
        :lem
        :lem.universal-argument
        :lem.show-paren
        :lem-vi-mode.core
        :lem-vi-mode.word
        :lem-vi-mode.visual
        :lem-vi-mode.jump-motions)
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
           :vi-move-to-last-nonblank
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
           :vi-kill-last-word
           :vi-undo
           :vi-redo
           :vi-move-to-matching-paren
           :vi-search-forward
           :vi-search-backward
           :vi-search-next
           :vi-search-previous
           :vi-goto-first-line
           :vi-goto-line
           :vi-return
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
           :vi-jump-back
           :vi-jump-next
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

(defparameter *multiline-motion-commands*
  (list 'vi-next-line
        'vi-previous-line
        'vi-next-display-line
        'vi-previous-display-line
        'vi-move-to-window-top
        'vi-move-to-window-middle
        'vi-move-to-window-bottom
        'vi-goto-first-line
        'vi-goto-line))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (vi-move-to-beginning-of-line)))

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
        ((vi-word-char-p (character-at p -1))
         (loop
           while (and (not (bolp p)) (vi-word-char-p (character-at p -1)))
           do (vi-backward-char)))
        ((or (bolp p)
             (vi-space-char-p (character-at p -1)))
         (skip-chars-backward p '(#\Space #\Tab #\Newline #\Return))
         (vi-backward-word-begin n))
        (t
         (loop until (or (bolp p)
                         (vi-word-char-p (character-at p -1))
                         (vi-space-char-p (character-at p -1)))
               do (vi-backward-char)))))
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
  (unless (or *vi-delete-recursive*
              *vi-clear-recursive*)
    (vi-backward-char)))

(define-command vi-forward-word-end-broad (&optional (n 1)) ("p")
  (forward-word-end (current-point) n t))

(define-command vi-backward-word-end (&optional (n 1)) ("p")
  (character-offset (current-point) -1)
  (skip-chars-backward (current-point) (lambda (char)
                                         (or (char= char #\Newline)
                                             (vi-space-char-p char))))
  (vi-backward-word-begin n))

(define-command vi-move-to-beginning-of-line () ()
  (with-point ((start (current-point)))
    (line-start start)
    (or (text-property-at (current-point) :field -1)
        (previous-single-property-change (current-point)
                                         :field
                                         start)
        (move-point (current-point) start))))

(define-command vi-move-to-end-of-line () ()
  (goto-eol (current-point)))

(define-command vi-move-to-last-nonblank () ()
  (vi-move-to-end-of-line)
  (skip-whitespace-backward (current-point) t)
  (when (and (not (bolp (current-point)))
             (eql (character-at (current-point)) #\Space))
    (vi-backward-char)))

(define-command vi-move-to-window-top () ()
  (with-jump-motion
    (move-point (current-point) (window-view-point (current-window)))))

(define-command vi-move-to-window-middle () ()
  (with-jump-motion
    (vi-move-to-window-top)
    (next-line (floor (/ (- (window-height (current-window)) 2) 2)))))

(define-command vi-move-to-window-bottom () ()
  (with-jump-motion
    (vi-move-to-window-top)
    (next-line (- (window-height (current-window)) 2))))

(define-command vi-back-to-indentation () ()
  (vi-move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t))

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
  (cond
    ((visual-p)
     (vi-delete))
    (t
     (unless (empty-line (current-point))
       (delete-next-char n)
       (fall-within-line (current-point))))))

(define-command vi-delete-previous-char (&optional (n 1)) ("p")
  (unless (bolp (current-point))
    (delete-previous-char n)))

(defvar *vi-delete-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-delete (&optional (n 1)) ("p")
    (cond (*vi-delete-recursive*
           (with-point ((start (line-start (current-point)))
                        (end (line-end (current-point))))
             (let ((eob (not (character-offset end 1))))
               (kill-region start end)
               (kill-append "" nil '(:vi-line))
               (if eob
                   (unless (or (first-line-p (current-point))
                               *vi-clear-recursive*)
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
             (when (visual-line-p)
               (kill-append "" nil '(:vi-line))))
           (vi-visual-end))
          (t
           (let* ((uarg nil)
                  (command (loop for key = (read-key)
                                 for key-char = (key-to-char key)
                                 while (and key-char
                                            (char<= #\0 key-char #\9))
                                 do (setf uarg (+ (* (or uarg 0) 10)
                                                  (- (char-code key-char)
                                                     (char-code #\0))))
                                 finally
                                    (return (lookup-keybind key)))))
             (loop while (hash-table-p command)
                   for key = (read-key)
                   do (setf command (gethash key command)))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-delete-recursive* t)
                       (*cursor-offset* 0))
                   (catch tag
                     (dotimes (i n)
                       ;; Ignore End of Buffer error and continue the deletion.
                       (ignore-errors (apply command (and uarg (list uarg)))))
                     (with-point ((end (current-point)))
                       (when (eq (point-buffer start)
                                 (point-buffer end))
                         (when (point< end start)
                           (rotatef start end)
                           (character-offset end 1))
                         (when (point/= start end)
                           (let ((multiline (find command
                                                  *multiline-motion-commands*)))
                             (when multiline
                               (line-start start)
                               (line-end end)
                               (character-offset end 1))
                             (kill-region start end)
                             (when multiline
                               (kill-append "" nil '(:vi-line)))))))))
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
        (p1 (skip-chars-forward (current-point)
                                  #'syntax-space-char-p)))
     (delete-character p (- p1))
     (let ((c0 (character-at p))
           (c1 (character-at p -1)))
       (unless (or (syntax-space-char-p c1)
                   (and (equal c0 #\))
                        (equal c1 #\))))
         (insert-character p #\space)))))

(defvar *vi-yank-recursive* nil)
(let ((tag (gensym)))
  (define-command vi-yank (&optional (n 1)) ("p")
    (cond (*vi-yank-recursive*
           (with-point ((start (current-point))
                        (end (current-point)))
             (line-start start)
             (line-end end)
             (character-offset end 1)
             (copy-region start end)
             (kill-append "" nil '(:vi-line))
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
             (when (visual-line-p)
               (kill-append "" nil '(:vi-line))))
           (vi-visual-end))
          (t
           (let* ((uarg nil)
                  (command (loop for key = (read-key)
                                 while (char<= #\0 (key-to-char key) #\9)
                                 do (setf uarg (+ (* (or uarg 0) 10)
                                                  (- (char-code (key-to-char key))
                                                     (char-code #\0))))
                                 finally
                                    (return (lookup-keybind key)))))
             (loop while (hash-table-p command)
                   for key = (read-key)
                   do (setf command (gethash key command)))
             (when (symbolp command)
               (with-point ((start (current-point)))
                 (let ((*vi-yank-recursive* t)
                       (*cursor-offset* 0))
                   (catch tag
                     (dotimes (i n)
                       ;; Ignore End of Buffer error and continue the deletion.
                       (ignore-errors (apply command (and uarg (list uarg)))))
                     (with-point ((end (current-point)))
                       (when (point< end start)
                         (rotatef start end)
                         (character-offset end 1))
                       (when (point/= start end)
                         (let ((multiline (find command
                                                *multiline-motion-commands*)))
                           (when multiline
                             (line-start start)
                             (line-end end)
                             (character-offset end 1))
                           (copy-region start end)
                           (when multiline
                             (kill-append "" nil '(:vi-line))))))))
                 (move-point (current-point) start))))))))

(define-command vi-paste-after () ()
  (multiple-value-bind (string type)
      (lem::current-kill-ring)
    (cond
      ((visual-p)
       (let ((visual-line (visual-line-p)))
         (vi-delete)
         (when (and (not visual-line)
                    (eql type :vi-line))
           (insert-character (current-point) #\Newline)))
       (insert-string (current-point) string)
       (character-offset (current-point) -1))
      (t
       (if (eql type :vi-line)
           (progn
             (line-end (current-point))
             (or (character-offset (current-point) 1)
                 (insert-character (current-point) #\Newline)))
           (character-offset (current-point) 1))
       (yank)
       (character-offset (current-point) -1)
       (when (eql type :vi-line)
         (line-start (current-point)))))))

(define-command vi-paste-before () ()
  (multiple-value-bind (string type)
      (lem::current-kill-ring)
    (cond
      ((visual-p)
       (vi-delete)
       (when (eql type :vi-line)
         (insert-character (current-point) #\Newline))
       (insert-string (current-point) string)
       (character-offset (current-point) -1))
      (t
       (when (eql type :vi-line)
         (line-start (current-point)))
       (yank)))))

(define-command vi-replace-char (c)
    ((list (key-to-char (read-key))))
  (cond
    ((visual-p)
     (apply-visual-range (lambda (start end)
                           (let ((count (- (point-column end)
                                           (point-column start))))
                             (delete-between-points start end)
                             (insert-character start c count))))
     (vi-visual-end))
    (t
     (delete-character (current-point) 1)
     (insert-character (current-point) c)
     (backward-char 1))))

(define-command vi-kill-last-word (&optional (n 1)) ("p")
  (let ((p (copy-point (current-point))))
    (vi-backward-word-end n)
    (kill-region p (current-point))))

(define-command vi-undo (&optional (n 1)) ("p")
  (undo n)
  (fall-within-line (current-point)))

(define-command vi-redo (&optional (n 1)) ("p")
  (redo n)
  (fall-within-line (current-point)))

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
    (with-jump-motion
      (move-point (current-point) p))))

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
  (with-jump-motion
    (lem.isearch:isearch-forward-regexp "/")))

(define-command vi-search-backward () ()
  (with-jump-motion
    (lem.isearch:isearch-backward-regexp "?")))

(define-command vi-search-next (n) ("p")
  (with-jump-motion
    (dotimes (i n) (lem.isearch:isearch-next))))

(define-command vi-search-previous (n) ("p")
  (with-jump-motion
    (dotimes (i n) (lem.isearch:isearch-prev))))

(define-command vi-goto-first-line () ()
  (with-jump-motion
    (move-to-beginning-of-buffer)
    (skip-whitespace-forward (current-point) t)))

(define-command vi-goto-line (&optional arg) ("P")
  (with-jump-motion
    (if (null arg)
          (progn
            (move-to-end-of-buffer)
            (line-start (current-point)))
          (goto-line arg))
    (skip-whitespace-forward (current-point) t)))

(define-command vi-return (&optional (n 1)) ("p")
  (vi-next-line n)
  (vi-move-to-beginning-of-line))

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
  (vi-move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t)
  (change-state 'insert))

(define-command vi-append () ()
  (let ((p (current-point)))
    (unless (or (end-line-p p)
                (end-buffer-p p))
      (forward-char 1))
    (change-state 'insert)))

(define-command vi-append-line () ()
  (line-end (current-point))
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

(define-command vi-jump-back (&optional (n 1)) ("p")
  (dotimes (i n)
    (jump-back)))

(define-command vi-jump-next (&optional (n 1)) ("p")
  (dotimes (i n)
    (jump-next)))

(define-command vi-normal () ()
  (change-state 'command))

(define-command vi-keyboard-quit () ()
  (when (eq (current-state) 'modeline)
    (lem::minibuf-read-line-break))
  (vi-visual-end)
  (keyboard-quit))
