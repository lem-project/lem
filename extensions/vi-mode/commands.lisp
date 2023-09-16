(defpackage :lem-vi-mode/commands
  (:use :cl
        :lem
        :lem/universal-argument
        :lem/show-paren
        :lem-vi-mode/core
        :lem-vi-mode/word
        :lem-vi-mode/visual
        :lem-vi-mode/jump-motions
        :lem-vi-mode/registers
        :lem-vi-mode/text-objects
        :lem-vi-mode/commands/utils)
  (:import-from :lem-vi-mode/states
                :*motion-keymap*
                :normal
                :insert
                :replace-state)
  (:import-from :lem-vi-mode/commands/utils
                :visual-region)
  (:import-from :lem/common/killring
                :peek-killring-item)
  (:import-from :lem-vi-mode/window
                :move-to-window-top
                :move-to-window-middle
                :move-to-window-bottom)
  (:import-from :lem-vi-mode/utils
                :kill-region-without-appending)
  (:import-from :lem/isearch
                :*isearch-finish-hooks*)
  (:import-from :lem/kbdmacro
                :*macro-running-p*)
  (:import-from :alexandria
                :when-let
                :last-elt)
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
           :vi-change-whole-line
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
           :vi-record-macro
           :vi-execute-macro
           :vi-execute-last-recorded-macro
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
           :vi-a-word
           :vi-inner-word
           :vi-a-double-quote
           :vi-inner-double-quote
           :vi-a-paren
           :vi-inner-paren
           :vi-repeat
           :vi-normal
           :vi-keyboard-quit))
(in-package :lem-vi-mode/commands)

(defun extract-count-keys (keys)
  (loop for key in keys
        for cmd = (lem-core::keymap-find-keybind *motion-keymap* key nil)
        unless (member cmd '(lem/universal-argument:universal-argument-0
                             lem/universal-argument:universal-argument-1
                             lem/universal-argument:universal-argument-2
                             lem/universal-argument:universal-argument-3
                             lem/universal-argument:universal-argument-4
                             lem/universal-argument:universal-argument-5
                             lem/universal-argument:universal-argument-6
                             lem/universal-argument:universal-argument-7
                             lem/universal-argument:universal-argument-8
                             lem/universal-argument:universal-argument-9)
                       :test 'eq)
        collect key))

(define-command vi-move-to-beginning-of-line/universal-argument-0 () ()
  (if (mode-active-p (current-buffer) 'universal-argument)
      (universal-argument-0)
      (vi-move-to-beginning-of-line)))

(define-command vi-forward-char (&optional (n 1)) ("p")
  (let* ((p (current-point))
         (max-offset (- (length (line-string p))
                        (point-charpos p))))
    (character-offset p (min n max-offset))))

(define-command vi-backward-char (&optional (n 1)) ("p")
  (let ((p (current-point)))
    (dotimes (_ n)
      (if (bolp p)
          (return)
          (character-offset p -1)))))

(define-motion vi-next-line (&optional (n 1)) ("p")
    (:type :line)
  (next-logical-line n))

(define-motion vi-line (&optional (n 1)) ("p")
    ()
  (line-offset (current-point) (1- n)))

(define-motion vi-next-display-line (&optional (n 1)) ("p")
    (:type :line)
  (next-line n))

(define-motion vi-previous-line (&optional (n 1)) ("p")
    (:type :line)
  (previous-logical-line n))

(define-motion vi-previous-display-line (&optional (n 1)) ("p")
    (:type :line)
  (previous-line n))

(defun on-only-space-line-p (point)
  (with-point ((p point))
    (line-end p)
    (skip-whitespace-backward p t)
    (bolp p)))

(define-command vi-forward-word-begin (&optional (n 1)) ("p")
  (let ((start-line (line-number-at-point (current-point)))
        (origin (copy-point (current-point))))
    (dotimes (i n)
      (forward-word-begin #'word-char-type))
    ;; In operator-pending mode, this motion behaves differently.
    (when (operator-pending-mode-p)
      (with-point ((p (current-point)))
        ;; Go back to the end of the previous line when the END point is in the next line.
        ;; For example, when the cursor is at [b],
        ;;   foo [b]ar
        ;;     baz
        ;; 'dw' deletes only the 'bar', instead of deleting to the beginning of the next word.
        (skip-whitespace-backward p t)
        (when (and (point< origin p)
                   (bolp p))
          (line-offset p -1)
          (line-end p)
          (loop while (and (< start-line
                              (line-number-at-point p))
                           (on-only-space-line-p p))
                do (line-offset p -1)
                   (line-end p))
          ;; Skip this line if the previous line is empty
          (when (bolp p)
            (character-offset p 1))
          (move-point (current-point) p))))))

(define-command vi-backward-word-begin (&optional (n 1)) ("p")
  (dotimes (i n)
    (backward-word-begin #'word-char-type)))

(define-motion vi-forward-word-end (&optional (n 1)) ("p")
    (:type :inclusive)
  (dotimes (i n)
    (forward-word-end #'word-char-type)))

(define-command vi-forward-word-begin-broad (&optional (n 1)) ("p")
  (dotimes (i n)
    (forward-word-begin #'broad-word-char-type)))

(define-command vi-backward-word-begin-broad (&optional (n 1)) ("p")
  (dotimes (i n)
    (backward-word-begin #'broad-word-char-type)))

(define-motion vi-forward-word-end-broad (&optional (n 1)) ("p")
    (:type :inclusive)
  (dotimes (i n)
    (forward-word-end #'broad-word-char-type)))

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

(define-command vi-move-to-end-of-line (&optional (n 1)) ("p")
  (vi-line n)
  (line-end (current-point)))

(define-command vi-move-to-last-nonblank () ()
  (vi-move-to-end-of-line)
  (skip-whitespace-backward (current-point) t)
  (when (and (not (bolp (current-point)))
             (eql (character-at (current-point)) #\Space))
    (vi-backward-char)))

(define-motion vi-move-to-window-top () ()
    (:type :line
     :jump t)
  (move-to-window-top))

(define-motion vi-move-to-window-middle () ()
    (:type :line
     :jump t)
  (move-to-window-middle))

(define-motion vi-move-to-window-bottom () ()
    (:type :line
     :jump t)
  (move-to-window-bottom))

(define-command vi-back-to-indentation () ()
  (vi-move-to-beginning-of-line)
  (skip-whitespace-forward (current-point) t))

(define-operator vi-indent (start end type) ("<R>")
    (:move-point nil)
  (when (and (eq type :line)
             (point/= start end)
             (zerop (point-charpos end)))
    (character-offset end -1))
  (indent-points start end))

;; FIXME: support block
(define-operator vi-substitute (beg end) ("<r>")
    (:motion vi-forward-char)
  (vi-delete beg end :inclusive)
  (change-state 'insert))

(define-operator vi-delete-next-char (beg end) ("<r>")
    (:motion vi-forward-char)
  (vi-delete beg end :inclusive))

(define-operator vi-delete-previous-char (beg end) ("<r>")
    (:motion vi-backward-char)
  (vi-delete beg end :exclusive))

(define-operator vi-delete (start end type) ("<R>")
    (:move-point nil)
  (when (point= start end)
    (return-from vi-delete))
  (let ((pos (point-charpos (current-point)))
        (ends-with-newline (char= (character-at end -1) #\Newline)))
    (delete-region start end :type type)
    (when (and (eq type :line)
               (not ends-with-newline)
               (not (= (position-at-point start) 1)))
      (delete-previous-char))
    (when (eq 'vi-delete (command-name (this-command)))
      (case type
        (:line
         (move-to-column (current-point)
                         (max 0
                              (min (1- (length (line-string (current-point)))) pos))))
        (:block
         (move-to-line (current-point) (min (line-number-at-point start)
                                            (line-number-at-point end)))
         (move-to-column (current-point) (min (point-column start)
                                              (point-column end)))))
      ;; After 'dw' or 'dW', move to the first non-blank char
      (when (and (this-motion-command)
                 (member (command-name (this-motion-command))
                         '(vi-forward-word-begin
                           vi-forward-word-begin-broad)
                         :test 'eq))
        (skip-chars-forward (current-point) '(#\Space #\Tab))))))

(define-operator vi-delete-line (start end type) ("<R>")
    (:motion vi-move-to-end-of-line)
  (when (or (eq type :line)
            (visual-char-p))
    (line-start start)
    (line-end end)
    (character-offset end 1))
  (if (eq type :block)
      (let ((column (point-charpos start)))
        (apply-region-lines start end
                            (lambda (p)
                              (with-point ((s p) (e p))
                                (move-to-column s column)
                                (line-end e)
                                (kill-region s e)))))
      (kill-region-without-appending start end)))

(define-operator vi-change (beg end type) ("<R>")
    ()
  (when (point= beg end)
    (return-from vi-change))
  (let ((end-with-newline (char= (character-at end -1) #\Newline)))
    (vi-delete beg end type)
    (when (eq type :line)
      (cond
        (end-with-newline
         (insert-character (current-point) #\Newline)
         (character-offset (current-point) -1))
        (t
         (insert-character (current-point) #\Newline)))
      (indent-line (current-point))))
  (change-state 'insert))

(define-operator vi-change-whole-line (beg end) ("<r>")
    (:motion vi-line)
  (line-start beg)
  (or (line-offset end 1 0)
      (line-end end))
  (vi-change beg end :line))

(define-operator vi-change-line (beg end type) ("<R>")
    (:motion vi-move-to-end-of-line)
  (vi-change beg end type)
  (change-state 'insert))

(define-operator vi-join (start end) ("<r>")
    (:motion vi-line)
  (let ((count
          (max 1 (- (line-number-at-point end)
                    (line-number-at-point start)))))
    (move-point (current-point) start)
    (dotimes (i count)
      (move-to-end-of-logical-line)
      (delete-next-char))))

(define-operator vi-join-line (start end type) ("<R>")
    (:motion vi-line)
  (when (and (eq type :line)
             (point/= start end)
             (zerop (point-charpos end)))
    (character-offset end -1))
  (let ((count
          (max 1 (- (line-number-at-point end)
                    (line-number-at-point start)))))
    (move-point (current-point) start)
    (dotimes (i count)
      (move-to-end-of-logical-line)
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

(define-operator vi-yank (start end type) ("<R>")
    (:move-point nil)
  (yank-region start end :type type)
  (case type
    (:block
     (move-to-line (current-point) (min (line-number-at-point start)
                                        (line-number-at-point end)))
     (move-to-column (current-point) (min (point-column start)
                                          (point-column end))))
    (:line
     (move-to-column start (point-charpos (current-point)))
     (move-point (current-point) start))
    (otherwise
     (move-point (current-point) start))))

(define-operator vi-yank-line (start end type) ("<R>")
    (:motion vi-move-to-end-of-line)
  (vi-yank start end type))

(defun vi-yank-from-clipboard-or-killring ()
  (multiple-value-bind (str options) (peek-killring-item (current-killring) 0)
    (if str
        (values str
                (cond
                  ((member :vi-line options) :line)
                  ((member :vi-block options) :block)
                  (t :char)))
        (and (enable-clipboard-p) (get-clipboard-data)))))

(define-command vi-paste-after () ()
  (multiple-value-bind (string type)
      (vi-yank-from-clipboard-or-killring)
    (cond
      ((visual-p)
       (let ((visual-line (visual-line-p)))
         (multiple-value-bind (beg end type)
             (visual-region)
           (vi-delete beg end type))
         (when (and (not visual-line)
                    (eq type :line))
           (insert-character (current-point) #\Newline))))
      (t
       (if (eq type :line)
           (progn
             (or (line-offset (current-point) 1 0)
                 (progn
                   (line-end (current-point))
                   (insert-character (current-point) #\Newline))))
           (character-offset (current-point) 1))))
    (paste-yank string type :after)))

(define-command vi-paste-before () ()
  (multiple-value-bind (string type)
      (vi-yank-from-clipboard-or-killring)
    (cond
      ((visual-p)
       (multiple-value-bind (beg end type)
           (visual-region)
         (vi-delete beg end type))
       (when (eq type :line)
         (insert-character (current-point) #\Newline)))
      (t
       (when (eq type :line)
         (line-start (current-point)))))
    (paste-yank string type :before)))

(defun read-key-to-replace ()
  (with-temporary-state 'replace-state
    (let ((command (read-command)))
      (unless command
        (escape))
      (call-command command (universal-argument-of-this-command)))))

(define-operator vi-replace-char (start end type char) ("<R>" (read-key-to-replace))
    (:motion vi-forward-char
     :move-point nil)
  (if (eq type :block)
      (progn
        (let ((line (min (line-number-at-point start)
                         (line-number-at-point end)))
              (col (min (point-column start)
                        (point-column end))))
          (apply-visual-range
           (lambda (start end)
             (vi-replace-char start end :inclusive char)))
          (move-to-line (current-point) line)
          (move-to-column (current-point) col)))
      (let ((string-to-replace
              ;; Replace all chars in the region except newlines
              (with-output-to-string (s)
                (map-region start end
                            (lambda (string lastp)
                              (format s
                                      "~v@{~C~:*~}~*~@[~%~]"
                                      (length string)
                                      char
                                      (not lastp)))))))
        (delete-between-points start end)
        (insert-string start string-to-replace)
        (if (visual-p)
            (move-point (current-point) start)
            (character-offset (current-point) *cursor-offset*)))))

(define-operator vi-kill-last-word (start end) ("<r>")
    (:motion vi-backward-word-end)
  (kill-region-without-appending start end))

(define-operator vi-upcase (start end type) ("<R>")
    (:move-point t)
  (if (eq type :block)
      (apply-visual-range #'uppercase-region)
      (uppercase-region start end)))

(define-operator vi-downcase (start end type) ("<R>")
    (:move-point t)
  (if (eq type :block)
      (apply-visual-range #'downcase-region)
      (downcase-region start end)))

(define-command vi-undo (&optional (n 1)) ("p")
  (undo n))

(define-command vi-redo (&optional (n 1)) ("p")
  (redo n))

(defvar *kbdmacro-recording-register* nil)
(defvar *last-recorded-macro* nil)

(defun read-register ()
  (let ((key (read-key)))
    (cond
      ((eq key (make-key :ctrl t :sym "g"))
       (keyboard-quit))
      ((eq key (make-key :sym "Escape"))
       (escape))
      (t
       (lem-core:key-to-char key)))))

(define-command vi-record-macro (register) ((or *kbdmacro-recording-register*
                                                (read-register)))
  (cond
    ((macro-register-p register)
     (cond
       ;; When recording
       (*kbdmacro-recording-register*
        ;; Finish recording
        (lem/kbdmacro:kbdmacro-end)
        (setf (register register)
              ;; Omit the last 'q'
              (let* ((last-key
                       (last-elt lem/kbdmacro::*last-macro-chars*))
                     (last-cmd
                       (find-keybind last-key)))
                (if (eq last-cmd 'vi-record-macro)
                    (butlast lem/kbdmacro::*last-macro-chars*)
                    lem/kbdmacro::*last-macro-chars*)))
        (setf *kbdmacro-recording-register* nil
              *last-recorded-macro* (downcase-char register)))
       (t
        ;; Start recording
        (setf *kbdmacro-recording-register* register)
        (lem/kbdmacro:kbdmacro-start))))
    (t
     (editor-error "Invalid register: ~A" register))))

(define-command vi-execute-macro (n macro) ("p" (read-register))
  (cond
    ((macro-register-p macro)
     (let ((keyseq (register macro)))
       (cond
         ((listp keyseq)
          (let ((*macro-running-p* t))
            (buffer-disable-undo-boundary (lem:current-buffer))
            (unwind-protect
                 (dotimes (i n)
                   (execute-key-sequence keyseq))
              (buffer-enable-undo-boundary (lem:current-buffer)))))
         (t
          (editor-error "No macro is recorded at the register '~A'" macro)))))
    (t
     (editor-error "Invalid register: ~A" macro))))

(define-command vi-execute-last-recorded-macro (&optional (n 1)) ("p")
  (unless *last-recorded-macro*
    (editor-error "No keyboard macro is recorded yet"))
  (vi-execute-macro n *last-recorded-macro*))

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

(define-motion vi-move-to-matching-paren () ()
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

(defvar *last-search-direction* nil)

(defun vi-isearch-finish-hook (string)
  (setf (register #\/) string)
  (remove-hook *isearch-finish-hooks* 'vi-isearch-finish-hook))

(define-command vi-search-forward () ()
  (setf *last-search-direction* :forward)
  (add-hook *isearch-finish-hooks* 'vi-isearch-finish-hook)
  (with-jump-motion
    (lem/isearch::isearch-start "/"
                                (lambda (point string)
                                  (alexandria:when-let (p (lem/isearch::search-forward-regexp
                                                           (copy-point lem/isearch::*isearch-start-point* :temporary)
                                                           string))
                                    (character-offset p (- (length string)))
                                    (move-point point p)))
                                #'lem/isearch::search-forward-regexp
                                #'lem/isearch::search-backward-regexp
                                "")))

(define-command vi-search-backward () ()
  (setf *last-search-direction* :backward)
  (add-hook *isearch-finish-hooks* 'vi-isearch-finish-hook)
  (with-jump-motion
    (lem/isearch:isearch-backward-regexp "?")))

(defun vi-search-repeat-forward (n)
  (when-let ((query (register #\/)))
    (lem/isearch::change-previous-string query)
    (with-jump-motion
      (with-point ((p (current-point)))
        (vi-forward-char (length lem/isearch::*isearch-string*))
        (loop repeat n
              for found = (lem/isearch:isearch-next)
              unless found
              do (move-point (current-point) p)
                 (return)
              finally
                 (vi-backward-char (length lem/isearch::*isearch-string*)))))))

(defun vi-search-repeat-backward (n)
  (when-let ((query (register #\/)))
    (lem/isearch::change-previous-string query)
    (with-jump-motion
      (dotimes (i n) (lem/isearch:isearch-prev)))))

(define-command vi-search-next (n) ("p")
  (case *last-search-direction*
    (:forward (vi-search-repeat-forward n))
    (:backward (vi-search-repeat-backward n))))

(define-command vi-search-previous (n) ("p")
  (case *last-search-direction*
    (:forward (vi-search-repeat-backward n))
    (:backward (vi-search-repeat-forward n))))

(define-command vi-search-forward-symbol-at-point () ()
  (with-jump-motion
    (lem/isearch:isearch-forward-symbol-at-point)
    (lem/isearch:isearch-finish)
    (lem/isearch:isearch-next)))

(define-motion vi-goto-first-line (&optional (n 1)) ("p")
    (:type :line
     :jump t)
  (let ((col (point-charpos (current-point))))
    (goto-line n)
    (move-to-column (current-point) col)))

(define-motion vi-goto-line (&optional n) ("P")
    (:type :line
     :jump t
     :default-n-arg nil)
  (let ((col (point-charpos (current-point))))
    (cond
      ((null n)
       (move-to-end-of-buffer)
       (when (and (bolp (current-point))
                  (eolp (current-point)))
         (line-offset (current-point) -1)))
      (t (goto-line n)))
    (move-to-column (current-point) col)))

(define-command vi-return (&optional (n 1)) ("p")
  (vi-next-line n)
  (vi-move-to-beginning-of-line))

(defvar *find-char-args* nil)

(defun %vi-find-char (count c direction offset &key dont-keep)
  (check-type count (integer 0))
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
    (let ((lem:*case-fold-search* t))
      (when (loop repeat count
                  for result = (funcall (if (eq direction :forward)
                                            'search-forward
                                            'search-backward)
                                        p
                                        (string c)
                                        limit)
                  unless result
                    do (return nil)
                  finally (return t))
        (character-offset p offset)
        (move-point (current-point) p)))))

(define-motion vi-find-char (&optional (n 1)) ("p")
    (:type :inclusive)
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char n c :forward -1)))

(define-motion vi-find-char-backward (&optional (n 1)) ("p")
    ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char n c :backward 0)))

(define-motion vi-find-char-before (&optional (n 1)) ("p")
    (:type :inclusive)
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char n c :forward -2)))

(define-motion vi-find-char-backward-after (&optional (n 1)) ("p")
    ()
  (alexandria:when-let (c (key-to-char (read-key)))
    (%vi-find-char n c :backward 1)))

(define-motion vi-find-char-repeat (&optional (n 1)) ("p")
    (:type :inclusive)
  (when *find-char-args*
    (apply #'%vi-find-char n *find-char-args*)))

(define-motion vi-find-char-repeat-backward (&optional (n 1)) ("p")
    ()
  (when *find-char-args*
    (destructuring-bind (c direction offset)
        *find-char-args*
      (apply #'%vi-find-char n (ecase direction
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
  (let ((p (current-point)))
    (line-end p)
    (change-state 'insert)
    (insert-character p #\Newline)
    (indent-line (current-point))))

(define-command vi-open-above () ()
  (line-start (current-point))
  (change-state 'insert)
  (open-line 1)
  (let ((column (with-point ((p (current-point)))
                  (point-column (or (and (line-offset p 1)
                                         (back-to-indentation p))
                                    (line-start p))))))
    (move-to-column (current-point) column t)))

(define-command vi-jump-back (&optional (n 1)) ("p")
  (dotimes (i n)
    (jump-back)))

(define-command vi-jump-next (&optional (n 1)) ("p")
  (dotimes (i n)
    (jump-next)))

(define-command vi-repeat (n) ("P")
  (when *last-repeat-keys*
    (let ((*enable-repeat-recording* nil)
          (prev-state (current-state)))
      (let ((keyseq (if n
                        (append
                         (map 'list (lambda (char) (lem:make-key :sym (string char)))
                              (princ-to-string n))
                         (extract-count-keys *last-repeat-keys*))
                        *last-repeat-keys*))
            ;; Clear the universal argument for vi-repeat
            (lem/universal-argument::*argument* (lem/universal-argument::make-arg-state)))
        (execute-key-sequence keyseq)
        (unless (state= prev-state (current-state))
          (change-state prev-state))))))

(define-text-object-command vi-a-word (count) ("p")
    (:expand-selection t)
  (a-range-of 'word-object (current-state) count))

(define-text-object-command vi-inner-word (count) ("p")
    (:expand-selection t)
  (inner-range-of 'word-object (current-state) count))

(define-text-object-command vi-a-double-quote () ()
    ()
  (a-range-of 'double-quoted-object (current-state) 1))

(define-text-object-command vi-inner-double-quote () ()
    ()
  (inner-range-of 'double-quoted-object (current-state) 1))

(define-text-object-command vi-a-paren (count) ("p")
    (:expand-selection t)
  (a-range-of 'paren-object (current-state) count))

(define-text-object-command vi-inner-paren (count) ("p")
    (:expand-selection t)
  (inner-range-of 'paren-object (current-state) count))

(define-command vi-normal () ()
  (change-state 'normal))

(define-command vi-keyboard-quit () ()
  (when (eq (current-state) 'modeline)
    (error 'editor-abort))
  (vi-visual-end)
  (keyboard-quit))
