(defpackage :lem-vi-mode/commands
  (:use :cl
        :lem
        :lem/universal-argument
        :lem/show-paren
        :lem-vi-mode/core
        :lem-vi-mode/word
        :lem-vi-mode/visual
        :lem-vi-mode/jump-motions
        :lem-vi-mode/commands/utils)
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
           :vi-upcase
           :vi-downcase
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

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (vi-move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (let* ((p (current-point))
         (max-offset (- (length (line-string p))
                        (point-charpos p))))
    (character-offset p (min n max-offset))
    (when (<= max-offset n)
      (character-offset p *cursor-offset*))))

(define-command vi-backward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (bolp p)
          (return)
          (character-offset p -1)))))

(define-vi-motion vi-next-line (&optional (n 1))
    (:type :line)
  (next-logical-line n)
  (fall-within-line (current-point)))

(define-vi-motion vi-next-display-line (&optional (n 1))
    (:type :line)
  (next-line n)
  (fall-within-line (current-point)))

(define-vi-motion vi-previous-line (&optional (n 1))
    (:type :line)
  (previous-logical-line n)
  (fall-within-line (current-point)))

(define-vi-motion vi-previous-display-line (&optional (n 1))
    (:type :line)
  (previous-line n)
  (fall-within-line (current-point)))

(define-command vi-forward-word-begin (&optional (n 1)) ("p")
  (dotimes (i n)
    (forward-word-begin #'char-type)))

(define-command vi-backward-word-begin (&optional (n 1)) ("p")
  (dotimes (i n)
    (backward-word-begin #'char-type)))

(define-vi-motion vi-forward-word-end (&optional (n 1))
    (:type :inclusive)
  (dotimes (i n)
    (forward-word-end #'char-type)))

(define-command vi-forward-word-begin-broad (&optional (n 1)) ("p")
  (dotimes (i n)
    (forward-word-begin #'broad-char-type)))

(define-command vi-backward-word-begin-broad (&optional (n 1)) ("p")
  (dotimes (i n)
    (backward-word-begin #'broad-char-type)))

(define-vi-motion vi-forward-word-end-broad (&optional (n 1))
    (:type :inclusive)
  (dotimes (i n)
    (forward-word-end #'broad-char-type)))

(define-command vi-backward-word-end (&optional (n 1)) ("p")
  (character-offset (current-point) -1)
  (skip-chars-backward (current-point) #'blank-char-p)
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

(define-vi-motion vi-move-to-window-top ()
    (:type :line
     :jump t)
  (move-point (current-point) (window-view-point (current-window))))

(define-vi-motion vi-move-to-window-middle ()
    (:type :line
     :jump t)
  (vi-move-to-window-top)
  (next-line (floor (/ (- (window-height (current-window)) 2) 2))))

(define-vi-motion vi-move-to-window-bottom ()
    (:type :line
     :jump t)
  (vi-move-to-window-top)
  (next-line (- (window-height (current-window)) 2)))

(define-command vi-back-to-indentation () ()
  (vi-move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t))

(define-vi-operator vi-indent (start end)
    (:restore-point t)
  (indent-points start end))

(define-vi-operator vi-substitute ()
    (:motion vi-forward-char
     :restore-point t)
  (vi-delete)
  (change-state 'insert))

(define-vi-operator vi-delete-next-char ()
    (:motion vi-forward-char)
  (vi-delete)
  (fall-within-line (current-point)))

(define-vi-operator vi-delete-previous-char ()
    (:motion vi-backward-char)
  (vi-delete))

(define-vi-operator vi-delete (start end type) ()
  (let ((pos (point-charpos (current-point))))
    (with-killring-context (:options (when (eq type :line) :vi-line)
                            :appending (when (eq type :block)
                                         (continue-flag :kill)))
      (kill-region-without-appending start end))
    (when (and (eq type :line)
               (eq 'vi-delete (command-name (this-command))))
      (if (last-line-p (current-point))
          (delete-previous-char)
          (delete-next-char))
      (setf (point-charpos (current-point))
            (max 0
                 (min (1- (length (line-string (current-point)))) pos))))
    (when (eq 'vi-delete (command-name (this-command)))
      (fall-within-line (current-point)))))

(define-vi-operator vi-delete-line (start end)
    (:motion vi-move-to-end-of-line)
  (when (visual-p)
    (line-start start)
    (line-end end)
    (character-offset end 1))
  (kill-region-without-appending start end)
  (fall-within-line (current-point)))

(define-vi-operator vi-change () ()
  (vi-delete)
  (change-state 'insert))

(define-vi-operator vi-change-line ()
    (:motion vi-move-to-end-of-line)
  (vi-change)
  (change-state 'insert))

(define-vi-motion vi-line (&optional (n 1)) ()
  (line-offset (current-point) (1- n)))

(define-vi-operator vi-join (start end) (:motion vi-line)
  (let ((count
          (max 1 (- (line-number-at-point end)
                    (line-number-at-point start)))))
    (move-point (current-point) start)
    (dotimes (i count)
      (move-to-end-of-line)
      (delete-next-char)))
  (fall-within-line (current-point)))

(define-vi-operator vi-join-line (start end) (:motion vi-line)
  (let ((count
          (max 1 (- (line-number-at-point end)
                    (line-number-at-point start)))))
    (move-point (current-point) start)
    (dotimes (i count)
      (move-to-end-of-line)
      (let ((p (current-point))
            (p1
              (skip-chars-forward (current-point)
                                  ;; Skip space chars, but skip Newlines only once.
                                  (let ((nl-count 0))
                                    (lambda (c)
                                      (if (char= c #\Newline)
                                          (<= (incf nl-count) 1)
                                          (syntax-space-char-p c)))))))
        (delete-character p (- p1))
        ;; Don't add a space when there's trailing spaces, or the next line is empty or starts with a ')'
        (unless (or (member (character-at p) '(#\Newline #\)))
                    (syntax-space-char-p (character-at p -1)))
          (insert-character p #\Space))))
    (vi-backward-char)))

(define-vi-operator vi-yank (start end type) (:restore-point t)
  (with-killring-context (:options (when (eq type :line) :vi-line))
    (copy-region start end)))

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
             (insert-character (current-point) #\Newline))
           (character-offset (current-point) 1))
       (with-point ((p (current-point)))
         (yank)
         (move-point (current-point) p))))))

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
       (with-point ((p (current-point)))
         (cond
           ((member :vi-line type)
            (line-start (current-point))
            (yank)
            (insert-character (current-point) #\Newline))
           (t
            (yank)))
         (move-point (current-point) p))))))

(define-vi-operator vi-replace-char (start end)
    (:motion vi-forward-char)
  (move-point (current-point) start)
  (let* ((c (key-to-char (read-key)))
         (string-to-replace
           ;; Replace all chars in the region except newlines
           (with-output-to-string (s)
             (map-region start end
                         (lambda (string lastp)
                           (format s
                                   "~v@{~C~:*~}~*~@[~%~]"
                                   (length string)
                                   c
                                   (not lastp)))))))
    (delete-between-points start end)
    (insert-string start string-to-replace)
    (if (visual-p)
        (move-point (current-point) start)
        (character-offset (current-point) *cursor-offset*))))

(define-vi-operator vi-kill-last-word (start end)
    (:motion vi-backward-word-end)
  (kill-region-without-appending start end))

(define-vi-operator vi-upcase (start end) ()
  (uppercase-region start end)
  (move-point (current-point) start))

(define-vi-operator vi-downcase (start end) ()
  (downcase-region start end)
  (move-point (current-point) start))

(define-command vi-undo (&optional (n 1)) ("p")
  (undo n)
  (fall-within-line (current-point)))

(define-command vi-redo (&optional (n 1)) ("p")
  (redo n)
  (fall-within-line (current-point)))

(defun vi-forward-matching-paren (window point &optional (offset -1))
  (declare (ignore window))
  (with-point ((point point))
    (when (syntax-open-paren-char-p (character-at point))
      (when (scan-lists point 1 0 t)
        (character-offset point offset)))))

(defun vi-backward-matching-paren (window point &optional (offset -1))
  (declare (ignore window offset))
  (when (syntax-closed-paren-char-p (character-at point))
    (scan-lists (character-offset (copy-point point :temporary) 1) -1 0 t)))

(define-vi-motion vi-move-to-matching-paren ()
    (:type :inclusive
     :jump t)
  (alexandria:when-let ((p (or (vi-backward-matching-paren (current-window) (current-point))
                               (vi-forward-matching-paren (current-window) (current-point)))))
    (move-point (current-point) p)))

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

(define-vi-motion vi-goto-first-line ()
    (:type :line
     :jump t)
  (move-to-beginning-of-buffer)
  (skip-whitespace-forward (current-point) t))

(define-vi-motion vi-goto-line (n)
    (:type :line
     :jump t)
  (if (null n)
      (progn
        (move-to-end-of-buffer)
        (line-start (current-point)))
      (goto-line n))
  (skip-whitespace-forward (current-point) t))

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

(define-vi-motion vi-find-char () (:type :inclusive)
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :forward -1)))

(define-vi-motion vi-find-char-backward () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :backward 0)))

(define-vi-motion vi-find-char-before () (:type :inclusive)
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :forward -2)))

(define-vi-motion vi-find-char-backward-after () ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char c :backward 1)))

(define-vi-motion vi-find-char-repeat () (:type :inclusive)
  (when *find-char-args*
    (apply #'%vi-find-char *find-char-args*)))

(define-vi-motion vi-find-char-repeat-backward () ()
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
