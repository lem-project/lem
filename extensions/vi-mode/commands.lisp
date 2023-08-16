(defpackage :lem-vi-mode/commands
  (:use :cl
        :lem
        :lem/universal-argument
        :lem/show-paren
        :lem-vi-mode/core
        :lem-vi-mode/word
        :lem-vi-mode/visual
        :lem-vi-mode/jump-motions)
  (:import-from :lem/common/killring
                :peek-killring-item)
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
           :vi-change
           :vi-change-line
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
           :vi-search-forward-symbol-at-point
           :vi-goto-first-line
           :vi-goto-line
           :vi-return
           :vi-find-char
           :vi-find-char-backward
           :vi-find-char-before
           :vi-find-char-backward-after
           :vi-find-char-repeat
           :vi-find-char-repeat-backward
           :vi-write
           :vi-quit
           :vi-write-quit
           :vi-end-insert
           :vi-insert
           :vi-insert-line
           :vi-append
           :vi-append-line
           :vi-open-below
           :vi-open-above
           :vi-jump-back
           :vi-jump-next
           :vi-normal
           :vi-keyboard-quit))
(in-package :lem-vi-mode/commands)

(defvar *cursor-offset* -1)
(defvar *vi-change-recursive* nil)
(defvar *vi-delete-recursive* nil)
(defvar *vi-yank-recursive* nil)

(defun bolp (point)
  "Return t if POINT is at the beginning of a line."
  (zerop (point-charpos point)))

(defun eolp (point)
  "Return t if POINT is at the end of line."
  (let ((len (length (line-string point))))
    (or (zerop len)
        (>= (point-charpos point)
            (1- len)))))

(defun goto-bol (point)
  "Goto beginning of a line."
  (line-start point))

(defun goto-eol (point)
  "Goto end of a line."
  (line-end point)
  (unless (bolp point)
    (character-offset point *cursor-offset*)))

(defun empty-line (point)
  "Return t if the POINT at line is empty."
  (zerop (length (line-string point))))

(defun read-universal-argument ()
  (loop :for key := (read-key)
        :for char := (key-to-char key)
        :while (and char (digit-char-p char))
        :collect (digit-char-p char) :into digits
        :finally (unread-key key)
                 (return-from read-universal-argument
                   (and digits
                        (parse-integer (format nil "~{~D~}" digits))))))

;; Vim word
;; See http://vimdoc.sourceforge.net/htmldoc/motion.html#word
;; word = a sequence of letters, digits, underscores, or a 
;; sequence of other non-blank characters.
(defun vi-word-char-p (char)
  (and (characterp char)
       (or (alphanumericp char)
           (char= char #\_)
           (char= char #\_)
           ;; TODO: Add mechanism to modify
           ;; these based on isKeyword from lisp.vim https://github.com/vim/vim/blob/master/runtime/syntax/lisp.vim#L74C1-L387
           ;; which is modified based on file extension.
           (char= char #\*)
           (char= char #\/)
           (char= char #\%)
           (char= char #\<)
           (char= char #\=)
           (char= char #\>)
           (char= char #\:)
           (char= char #\$)
           (char= char #\?)
           (char= char #\!)
           (char= char #\^)
           (char= char #\-))))

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
  (unless *vi-change-recursive*
    (if (or *vi-delete-recursive*
            *vi-yank-recursive*)
        (skip-chars-forward (current-point) '(#\Space #\Tab))
        (skip-chars-forward (current-point) '(#\Space #\Tab #\Newline)))))

(define-command vi-backward-word-begin (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (when (and (< 0 n)
	       (not (and (= (line-number-at-point p) 1)
			 (bolp p))))
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
	       do (vi-backward-char))))
      (vi-backward-word-begin (1- n)))))

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
              *vi-change-recursive*)
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
  (define-vi-operator vi-indent (&optional (n 1)) ("p")
    (cond (*vi-indent-recursive*
           (indent-line (current-point))
           (throw tag t))
          ((visual-p)
           (apply-visual-range #'indent-points)
           (vi-visual-end))
          (t
           (let ((uarg (or (read-universal-argument) n))
                 (command (read-command)))
             (with-point ((start (current-point)))
               (let ((*vi-indent-recursive* t)
                     (*cursor-offset* 0))
                 (catch tag
                   ;; Ignore End of Buffer error and continue the deletion.
                   (ignore-errors (call-command command uarg))
                   (with-point ((end (current-point)))
                     (when (point< end start)
                       (rotatef start end))
                     (indent-points start end))))))))))

(define-vi-operator vi-substitute (&optional (n 1)) ("p")
  (vi-delete-next-char n)
  (change-state 'insert))

(define-vi-operator vi-delete-next-char (&optional (n 1)) ("p")
  (cond
    ((visual-p)
     (vi-delete))
    (t
     (unless (empty-line (current-point))
       (delete-next-char n)
       (fall-within-line (current-point))))))

(define-vi-operator vi-delete-previous-char (&optional (n 1)) ("p")
  (unless (bolp (current-point))
    (delete-previous-char n)))

(let ((tag (gensym)))
  (define-vi-operator vi-delete (&optional (n 1)) ("p")
    (cond (*vi-delete-recursive*
           ;; TODO: universal argument
           (with-point ((start (line-start (current-point)))
                        (end (line-end (current-point))))
             (let ((eob (not (character-offset end 1))))
               (with-killring-context (:options :vi-line)
                 (kill-region start end))
               (if eob
                   (unless (or (first-line-p (current-point))
                               *vi-change-recursive*)
                     (delete-previous-char))
                   (when *vi-change-recursive*
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
             (with-killring-context (:options (when (visual-line-p) :vi-line)
                                     :appending (continue-flag :kill))
               (copy-to-clipboard-with-killring (get-output-stream-string out))))
           (vi-visual-end))
          (t
           (let ((uarg (or (read-universal-argument) n))
                 (command (read-command)))
             (with-point ((start (current-point)))
               (let ((*vi-delete-recursive* t)
                     (*cursor-offset* 0))
                 (catch tag
                   (dotimes (i n)
                     ;; Ignore End of Buffer error and continue the deletion.
                     (ignore-errors (call-command command uarg)))
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
                           (with-killring-context (:options (when multiline :vi-line))
                             (kill-region start end))))))))
               (unless *vi-change-recursive*
                 (fall-within-line (current-point)))))))))

(define-vi-operator vi-delete-line () ()
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
         (unless *vi-change-recursive*
           (fall-within-line (current-point))))))

(define-vi-operator vi-change () ()
  (let ((*vi-change-recursive* t))
    (vi-delete))
  (vi-insert))

(define-vi-operator vi-change-line () ()
  (let ((*vi-change-recursive* t))
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

(let ((tag (gensym)))
  (define-vi-operator vi-yank (&optional (n 1)) ("p")
    (cond (*vi-yank-recursive*
           ;; TODO: universal argument
           (with-point ((start (current-point))
                        (end (current-point)))
             (line-start start)
             (line-end end)
             (character-offset end 1)
             (with-killring-context (:options :vi-line)
               (copy-region start end))
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
             (with-killring-context (:options (when (visual-line-p) :vi-line)
                                     :appending (continue-flag :kill))
               (copy-to-clipboard-with-killring (get-output-stream-string out))))
           (vi-visual-end))
          (t
           (let ((uarg (or (read-universal-argument) n))
                 (command (read-command)))
             (with-point ((start (current-point)))
               (let ((*vi-yank-recursive* t)
                     (*cursor-offset* 0))
                 (catch tag
                   (dotimes (i n)
                     ;; Ignore End of Buffer error and continue the deletion.
                     (ignore-errors (call-command command uarg)))
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
                         (with-killring-context (:options (when multiline :vi-line))
                           (copy-region start end)))))))
               (move-point (current-point) start)))))))

(defun vi-yank-from-clipboard-or-killring ()
  (multiple-value-bind (str options) (peek-killring-item (current-killring) 0)
    (if str
        (values str options) 
        (and (enable-clipboard-p) (get-clipboard-data)))))

(define-command vi-paste-after () ()
  (multiple-value-bind (string type)
      (vi-yank-from-clipboard-or-killring)
    (cond
      ((visual-p)
       (let ((visual-line (visual-line-p)))
         (vi-delete)
         (when (and (not visual-line)
                    (member :vi-line type))
           (insert-character (current-point) #\Newline)))
       (insert-string (current-point) string)
       (character-offset (current-point) -1))
      (t
       (if (member :vi-line type)
           (progn
             (line-end (current-point))
             (or (character-offset (current-point) 1)
                 (insert-character (current-point) #\Newline)))
           (character-offset (current-point) 1))
       (yank)
       (character-offset (current-point) -1)
       (when (member :vi-line type)
         (line-start (current-point)))))))

(define-command vi-paste-before () ()
  (multiple-value-bind (string type)
      (vi-yank-from-clipboard-or-killring)
    (cond
      ((visual-p)
       (vi-delete)
       (when (member :vi-line type)
         (insert-character (current-point) #\Newline))
       (insert-string (current-point) string)
       (character-offset (current-point) -1))
      (t
       (when (member :vi-line type)
         (line-start (current-point)))
       (yank)))))

(define-vi-operator vi-replace-char (c)
    ((key-to-char (read-key)))
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

(defun vi-forward-matching-paren (window point &optional (offset *cursor-offset*))
  (declare (ignore window offset))
  (with-point ((point point))
    (when (syntax-open-paren-char-p (character-at point))
      (when (scan-lists point 1 0 t)
        (character-offset point *cursor-offset*)))))

(defun vi-backward-matching-paren (window point &optional (offset -1))
  (declare (ignore window offset))
  (when (syntax-closed-paren-char-p (character-at point))
    (scan-lists (character-offset (copy-point point :temporary) 1) -1 0 t)))

(define-command vi-move-to-matching-paren () ()
  (alexandria:when-let ((p (or (vi-backward-matching-paren (current-window) (current-point))
                               (vi-forward-matching-paren (current-window) (current-point) *cursor-offset*))))
    (with-jump-motion
      (move-point (current-point) p))))

(let ((old-forward-matching-paren)
      (old-backward-matching-paren))
  (defun on-matching-paren ()
    (setf old-forward-matching-paren (variable-value 'forward-matching-paren :global))
    (setf old-backward-matching-paren (variable-value 'backward-matching-paren :global))
    (setf (variable-value 'forward-matching-paren :global) 'vi-forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) 'vi-backward-matching-paren))
  (defun off-matching-paren ()
    (setf (variable-value 'forward-matching-paren :global) old-forward-matching-paren)
    (setf (variable-value 'backward-matching-paren :global) old-backward-matching-paren)))

(add-hook *enable-hook* 'on-matching-paren)
(add-hook *disable-hook* 'off-matching-paren)

(define-command vi-search-forward () ()
  (with-jump-motion
    (lem/isearch:isearch-forward-regexp "/")))

(define-command vi-search-backward () ()
  (with-jump-motion
    (lem/isearch:isearch-backward-regexp "?")))

(define-command vi-search-next (n) ("p")
  (with-jump-motion
    (dotimes (i n) (lem/isearch:isearch-next))))

(define-command vi-search-previous (n) ("p")
  (with-jump-motion
    (dotimes (i n) (lem/isearch:isearch-prev))))

(define-command vi-search-forward-symbol-at-point () ()
  (with-jump-motion
    (lem/isearch:isearch-forward-symbol-at-point)
    (lem/isearch:isearch-finish)
    (lem/isearch:isearch-next)))

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

(defvar *find-char-args* nil)

(defun %vi-find-char (c direction offset &key dont-keep)
  (check-type direction (member :forward :backward))
  (check-type offset integer)
  (unless dont-keep
    (setf *find-char-args* (list c direction offset)))
  (with-point ((p (current-point))
               (limit (current-point)))
    (character-offset p (* -1 offset))
    (if (eq direction :forward)
        (line-end limit)
        (line-start limit))
    (when (funcall (if (eq direction :forward)
                       'search-forward
                       'search-backward)
                   p
                   (string c)
                   limit)
      (character-offset p offset)
      (move-point (current-point) p))))

(define-command vi-find-char () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :forward -1)))

(define-command vi-find-char-backward () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :backward 0)))

(define-command vi-find-char-before () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :forward -2)))

(define-command vi-find-char-backward-after () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :backward 1)))

(define-command vi-find-char-repeat () ()
  (when *find-char-args*
    (apply #'%vi-find-char *find-char-args*)))

(define-command vi-find-char-repeat-backward () ()
  (when *find-char-args*
    (destructuring-bind (c direction offset)
        *find-char-args*
      (apply #'%vi-find-char (ecase direction
                               (:forward (list c :backward (1+ offset) :dont-keep t))
                               (:backward (list c :forward (1- offset) :dont-keep t)))))))

(define-command vi-write () ()
  (lem:write-file (lem:buffer-filename (lem:current-buffer))))

(define-command vi-quit (&optional (ask t)) ()
  (if (one-window-p)
      (exit-lem ask)
      (delete-active-window)))

(define-command vi-write-quit () ()
  (vi-write)
  (vi-quit nil))

(define-command vi-end-insert () ()
  (change-state 'normal)
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

(define-command vi-open-above () ()
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
  (change-state 'normal))

(define-command vi-keyboard-quit () ()
  (when (eq (current-state) 'modeline)
    (error 'editor-abort))
  (vi-visual-end)
  (keyboard-quit))
