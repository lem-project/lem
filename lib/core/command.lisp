(in-package :lem)

(export '(*set-location-hook*
          exit-lem
          quick-exit
          keyboard-quit
          self-insert-before-hook
          self-insert-after-hook
          self-insert
          unmark-buffer
          *read-only-function*
          toggle-read-only
          rename-buffer
          quoted-insert
          newline
          open-line
          delete-next-char
          delete-previous-char
          copy-region
          copy-region-to-clipboard
          kill-region
          kill-region-to-clipboard
          kill-line
          yank
          yank-pop
          yank-pop-next
          yank-to-clipboard
          paste-from-clipboard
          next-line
          next-logical-line
          previous-line
          previous-logical-line
          forward-char
          backward-char
          move-to-beginning-of-buffer
          move-to-end-of-buffer
          move-to-beginning-of-line
          move-to-end-of-line
          next-page
          previous-page
          entab-line
          detab-line
          next-page-char
          previous-page-char
          delete-blank-lines
          just-one-space
          delete-indentation
          transpose-characters
          back-to-indentation-command
          undo
          redo
          mark-set
          exchange-point-mark
          goto-line
          filter-buffer
          pipe-command
          delete-trailing-whitespace
          load-library
          lem-version
          compare-windows))

(defvar *set-location-hook* '())

(defun delete-character-with-killring (point n killp)
  (let ((string (delete-character point n)))
    (when (and killp string) (kill-push string))
    t))

(define-key *global-keymap* "C-x C-c" 'exit-lem)
(define-command exit-lem (&optional (ask t)) ((list t))
  (when (or (null ask)
            (not (any-modified-buffer-p))
            (prompt-for-y-or-n-p "Modified buffers exist. Leave anyway"))
    (exit-editor)))

(define-command quick-exit () ()
  (save-some-buffers t)
  (exit-editor))

(define-key *global-keymap* "C-g" 'keyboard-quit)
(define-command keyboard-quit () ()
  (error 'editor-abort))

(define-editor-variable self-insert-before-hook '())
(define-editor-variable self-insert-after-hook '())

(define-command self-insert (n) ("p")
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c
           (run-hooks (variable-value 'self-insert-before-hook) c)
           (insert-character (current-point) c n)
           (run-hooks (variable-value 'self-insert-after-hook) c))
          (t
           (undefined-key)))))

(define-key *global-keymap* "M-~" 'unmark-buffer)
(define-command unmark-buffer () ()
  (buffer-unmark (current-buffer))
  t)

(defvar *read-only-function* nil)

(define-key *global-keymap* "C-x C-q" 'toggle-read-only)
(define-command toggle-read-only () ()
  (setf (buffer-read-only-p (current-buffer))
        (not (buffer-read-only-p (current-buffer))))
  (when *read-only-function*
    (funcall *read-only-function*
             (buffer-read-only-p (current-buffer))))
  t)

(define-command rename-buffer (name) ("sRename buffer: ")
  (buffer-rename (current-buffer) name)
  t)

(define-key *global-keymap* "C-q" 'quoted-insert)
(define-command quoted-insert (&optional (n 1)) ("p")
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (dotimes (_ n t)
      (insert-character (current-point) char 1))))

(define-key *global-keymap* "Return" 'newline)
(define-command newline (&optional (n 1)) ("p")
  (insert-character (current-point) #\newline n))

(define-key *global-keymap* "C-o" 'open-line)
(define-command open-line (n) ("p")
  (let ((point (current-point)))
    (insert-character (current-point) #\newline n)
    (character-offset point (- n))))

(define-key *global-keymap* "C-d" 'delete-next-char)
(define-key *global-keymap* "Delete" 'delete-next-char)
(define-command delete-next-char (&optional n) ("P")
  (when (end-buffer-p (current-point))
    (editor-error "End of buffer"))
  (when n
    (unless (continue-flag :kill)
      (kill-ring-new)))
  (delete-character-with-killring (current-point)
                                  (or n 1)
                                  (if n t nil)))


(define-key *global-keymap* "C-h" 'delete-previous-char)
(define-key *global-keymap* "Backspace" 'delete-previous-char)
(define-command delete-previous-char (&optional n) ("P")
  (if (and (buffer-mark (current-buffer))
           (buffer-mark-p (current-buffer)))
      (let ((start (region-beginning))
            (end (region-end)))
        (delete-character start (count-characters start end)))
      (progn
        (backward-char (or n 1))
        (handler-case (delete-next-char n)
          (read-only-error (e)
            (forward-char (or n 1))
            (error e))))))

(define-key *global-keymap* "M-w" 'copy-region)
(define-command copy-region (start end) ("r")
  (unless (continue-flag :kill)
    (kill-ring-new))
  (kill-push (points-to-string start end))
  (buffer-mark-cancel (current-buffer))
  t)

(define-command copy-region-to-clipboard (start end) ("r")
  (copy-to-clipboard (points-to-string start end)))

(define-key *global-keymap* "C-w" 'kill-region)
(define-command kill-region (start end) ("r")
  (when (point< end start)
    (rotatef start end))
  (unless (continue-flag :kill)
    (kill-ring-new))
  (delete-character-with-killring start (count-characters start end) t)
  t)

(define-command kill-region-to-clipboard (start end) ("r")
  (copy-region-to-clipboard start end)
  (delete-character start (count-characters start end)))

(define-key *global-keymap* "C-k" 'kill-line)
(define-command kill-line (&optional arg) ("P")
  (with-point ((start (current-point) :right-inserting))
    (cond
      ((null arg)
       (let ((p (current-point)))
         (cond ((end-buffer-p p)
                (editor-error "End of buffer"))
               ((end-line-p p)
                (character-offset p 1))
               (t (line-end p)))
         (kill-region start p)))
      (t
       (line-offset (current-point) arg)
       (let ((end (current-point)))
         (kill-region start end))))))

(define-key *global-keymap* "C-y" 'yank)
(define-command yank (&optional arg) ("P")
  (let ((string (if (null arg)
                    (current-kill-ring)
                    (kill-ring-nth arg))))
    (setf (buffer-value (current-buffer) 'yank-start) (copy-point (current-point) :temporary))
    (insert-string (current-point) string)
    (setf (buffer-value (current-buffer) 'yank-end) (copy-point (current-point) :temporary))
    (continue-flag :yank)
    t))

(define-key *global-keymap* "M-y" 'yank-pop)
(define-command yank-pop (&optional n) ("p")
  (let ((start (buffer-value (current-buffer) 'yank-start))
        (end (buffer-value (current-buffer) 'yank-end))
        prev-yank-p)
    (when (continue-flag :yank) (setq prev-yank-p t))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (kill-ring-rotate)
           (yank n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command yank-pop-next (&optional n) ("p")
  (let ((start (buffer-value (current-buffer) 'yank-start))
        (end (buffer-value (current-buffer) 'yank-end))
        prev-yank-p)
    (when (continue-flag :yank) (setq prev-yank-p t))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (kill-ring-rotate-undo)
           (yank n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command yank-to-clipboard (&optional arg) ("p")
  (let ((string (if (null arg)
                    (current-kill-ring)
                    (kill-ring-nth arg))))
    (copy-to-clipboard string)
    t))

(define-command paste-from-clipboard () ()
  (insert-string (current-point) (get-clipboard-data))
  t)

(defvar *next-line-prev-column* nil)

(define-key *global-keymap* "C-n" 'next-line)
(define-key *global-keymap* "Down" 'next-line)

(defun next-line-aux (n
                      point-column-fn
                      forward-line-fn
                      move-to-column-fn)
  (unless (continue-flag :next-line)
    (setq *next-line-prev-column* (funcall point-column-fn (current-point))))
  (unless (prog1 (funcall forward-line-fn (current-point) n)
            (funcall move-to-column-fn (current-point) *next-line-prev-column*))
    (cond ((plusp n)
           (move-to-end-of-buffer)
           (editor-error "End of buffer"))
          (t
           (move-to-beginning-of-buffer)
           (editor-error "Beginning of buffer")))))

(define-command next-line (&optional n) ("p")
  (next-line-aux n
                 #'point-virtual-line-column
                 #'move-to-next-virtual-line
                 #'move-to-virtual-line-column))

(define-command next-logical-line (&optional n) ("p")
  (next-line-aux n
                 #'point-column
                 #'line-offset
                 #'move-to-column))

(define-key *global-keymap* "C-p" 'previous-line)
(define-key *global-keymap* "Up" 'previous-line)
(define-command previous-line (&optional (n 1)) ("p")
  (next-line (- n)))

(define-command previous-logical-line (&optional (n 1)) ("p")
  (next-logical-line (- n)))

(define-key *global-keymap* "C-f" 'forward-char)
(define-key *global-keymap* "Right" 'forward-char)
(define-command forward-char (&optional (n 1)) ("p")
  (or (character-offset (current-point) n)
      (editor-error "End of buffer")))

(define-key *global-keymap* "C-b" 'backward-char)
(define-key *global-keymap* "Left" 'backward-char)
(define-command backward-char (&optional (n 1)) ("p")
  (or (character-offset (current-point) (- n))
      (editor-error "Beginning of buffer")))

(define-key *global-keymap* "M-<" 'move-to-beginning-of-buffer)
(define-command move-to-beginning-of-buffer () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-start (current-point))
  t)

(define-key *global-keymap* "M->" 'move-to-end-of-buffer)
(define-command move-to-end-of-buffer () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-end (current-point))
  t)

(define-key *global-keymap* "C-a" 'move-to-beginning-of-line)
(define-key *global-keymap* "Home" 'move-to-beginning-of-line)
(define-command move-to-beginning-of-line () ()
  (let ((bol (backward-line-wrap (copy-point (current-point) :temporary)
                                 (current-window)
                                 t)))
    (or (text-property-at (current-point) :field -1)
        (previous-single-property-change (current-point)
                                         :field
                                         bol)
        (move-point (current-point) bol)))
  t)

(define-key *global-keymap* "C-e" 'move-to-end-of-line)
(define-key *global-keymap* "End" 'move-to-end-of-line)
(define-command move-to-end-of-line () ()
  (or (and (forward-line-wrap (current-point) (current-window))
           (character-offset (current-point) -1))
      (line-end (current-point)))
  t)

(define-key *global-keymap* "C-v" 'next-page)
(define-key *global-keymap* "PageDown" 'next-page)
(define-command next-page (&optional n) ("P")
  (if n
      (scroll-down n)
      (cond
        ((line-offset (current-point)
                      (1- (window-height (current-window))))
         (window-recenter (current-window))
         t)
        (t
         (buffer-end (current-point))
         (window-recenter (current-window))
         t))))

(define-key *global-keymap* "M-v" 'previous-page)
(define-key *global-keymap* "PageUp" 'previous-page)
(define-command previous-page (&optional n) ("P")
  (if n
      (scroll-up n)
      (cond
        ((line-offset (current-point)
                      (- (1- (window-height (current-window)))))
         (window-recenter (current-window))
         t)
        (t
         (buffer-start (current-point))
         (window-recenter (current-window))
         t))))

(defun delete-while-whitespaces (ignore-newline-p)
  (let ((n (skip-chars-forward (current-point)
                               (if ignore-newline-p
                                   '(#\space #\tab)
                                   '(#\space #\tab #\newline)))))
    (delete-character (current-point) (- n))))

(defun tab-line-aux (n make-space-str)
  (let ((p (current-point)))
    (dotimes (_ n t)
      (with-point ((p2 (back-to-indentation p)))
        (let ((count (point-column p2)))
          (multiple-value-bind (div mod)
              (floor count (variable-value 'tab-width))
            (line-start p)
            (delete-between-points p p2)
            (insert-string p (funcall make-space-str div))
            (insert-character p #\space mod)))
        (unless (line-offset p 1)
          (return))))))

(define-command entab-line (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-command detab-line (n) ("p")
  (tab-line-aux n
                (lambda (n)
                  (make-string (* n (variable-value 'tab-width))
                               :initial-element #\space))))

(define-key *global-keymap* "C-x ]" 'next-page-char)
(define-command next-page-char (&optional (n 1)) ("p")
  (let ((point (current-point)))
    (dotimes (_ (abs n))
      (loop
        (unless (line-offset point (if (plusp n) 1 -1))
          (return-from next-page-char))
        (when (eql #\page (character-at point 0))
          (return))))))

(define-key *global-keymap* "C-x [" 'previous-page-char)
(define-command previous-page-char (&optional (n 1)) ("p")
  (next-page-char (- n)))

(define-key *global-keymap* "C-x C-o" 'delete-blank-lines)
(define-command delete-blank-lines () ()
  (let ((point (current-point)))
    (loop
      (unless (blank-line-p point)
        (line-offset point 1)
        (return))
      (unless (line-offset point -1)
        (return)))
    (loop
      (when (end-buffer-p point)
        (return))
      (let ((nblanks (blank-line-p point)))
        (if nblanks
            (delete-character point nblanks)
            (return))))))

(define-key *global-keymap* "M-Space" 'just-one-space)
(define-command just-one-space () ()
  (skip-whitespace-backward (current-point) t)
  (delete-while-whitespaces t)
  (insert-character (current-point) #\space 1)
  t)

(define-key *global-keymap* "M-^" 'delete-indentation)
(define-command delete-indentation () ()
  (with-point ((p (current-point)))
    (line-start p)
    (unless (start-buffer-p p)
      (delete-character p -1)
      (skip-whitespace-backward p t)
      (loop :while (and (syntax-space-char-p (character-at p))
                        (not (end-buffer-p p)))
            :do (delete-character p))
      (unless (or (start-line-p p)
                  (syntax-closed-paren-char-p (character-at p))
                  (with-point ((p p))
                    (and (character-offset p -1)
                         (let ((c (character-at p)))
                           (or (end-line-p p)
                               (syntax-open-paren-char-p c)
                               (syntax-expr-prefix-char-p c))))))
        (insert-character p #\space)))))

(define-key *global-keymap* "C-t" 'transpose-characters)
(define-command transpose-characters () ()
  (let ((point (current-point)))
    (cond ((start-line-p point))
          ((end-line-p point)
           (let ((c1 (character-at point -1))
                 (c2 (character-at point -2)))
             (unless (eql c2 #\newline)
               (delete-character point -2)
               (insert-string point (format nil "~C~C" c1 c2)))))
          (t
           (let ((c1 (character-at point 0))
                 (c2 (character-at point -1)))
             (delete-character point 1)
             (delete-character point -1)
             (insert-string point (format nil "~C~C" c1 c2)))))))

(define-key *global-keymap* "M-m" 'back-to-indentation-command)
(define-command back-to-indentation-command () ()
  (back-to-indentation (current-point))
  t)

(define-key *global-keymap* "C-\\" 'undo)
(define-command undo (n) ("p")
  (dotimes (_ n t)
    (unless (buffer-undo (current-point))
      (editor-error "Undo Error"))))

(define-key *global-keymap* "C-_" 'redo)
(define-command redo (n) ("p")
  (dotimes (_ n t)
    (unless (buffer-redo (current-point))
      (editor-error "Redo Error"))))

(define-key *global-keymap* "C-@" 'mark-set)
(define-key *global-keymap* "C-Space" 'mark-set)
(define-command mark-set () ()
  (run-hooks *set-location-hook* (current-point))
  (set-current-mark (current-point))
  (message "Mark set"))

(define-key *global-keymap* "C-x C-x" 'exchange-point-mark)
(define-command exchange-point-mark () ()
  (check-marked)
  (let ((mark (buffer-mark (current-buffer)))
        (point (copy-point (buffer-point (current-buffer)) :temporary)))
    (move-point (current-point) mark)
    (set-current-mark point))
  t)

(define-key *global-keymap* "C-x h" 'mark-set-whole-buffer)
(define-command mark-set-whole-buffer () ()
  (buffer-end (current-point))
  (set-current-mark (current-point))
  (buffer-start (current-point))
  (message "Mark set whole buffer"))

(define-key *global-keymap* "M-g" 'goto-line)
(define-command goto-line (n) ("nLine to GOTO: ")
  (cond ((< n 1)
         (setf n 1))
        ((< #1=(buffer-nlines (current-buffer)) n)
         (setf n #1#)))
  (run-hooks *set-location-hook* (current-point))
  (line-offset (buffer-start (current-point)) (1- n))
  t)

(define-key *global-keymap* "C-x #" 'filter-buffer)
(define-command filter-buffer (cmd) ("sFilter buffer: ")
  (let ((buffer (current-buffer))
        (line-number (line-number-at-point (current-point)))
        (charpos (point-charpos (current-point))))
    (multiple-value-bind (start end)
        (cond ((buffer-mark-p buffer)
               (values (region-beginning buffer)
                       (region-end buffer)))
              (t
               (values (buffer-start-point buffer)
                       (buffer-end-point buffer))))
      (let ((string (points-to-string start end))
            output-value
            error-output-value
            status)
        (let ((output-string
                (with-output-to-string (output)
                  (with-input-from-string (input string)
                    (multiple-value-setq
                        (output-value error-output-value status)
                      (uiop:run-program cmd
                                        :directory (buffer-directory buffer)
                                        :input input
                                        :output output
                                        :error-output output
                                        :ignore-error-status t))))))
          (delete-between-points start end)
          (insert-string start output-string)
          (move-to-line (current-point) line-number)
          (line-offset (current-point) 0 charpos)
          (zerop status))))))

(define-key *global-keymap* "C-x @" 'pipe-command)
(define-command pipe-command (str) ("sPipe command: ")
  (let ((directory (buffer-directory)))
    (let ((output-string
            (with-output-to-string (out)
              (uiop:run-program str
                                :directory directory
                                :output out
                                :error-output out
                                :ignore-error-status t))))
      (unless (string= output-string "")
        (with-pop-up-typeout-window (out (make-buffer "*Command*") :focus nil :erase t :read-only nil)
          (write-string output-string out))))))

(define-command delete-trailing-whitespace (buffer) ((list (current-buffer)))
  (save-excursion
    (setf (current-buffer) buffer)
    (let ((p (current-point)))
      (buffer-start p)
      (loop
        (line-end p)
        (let ((n (skip-whitespace-backward p t)))
          (unless (zerop n)
            (delete-character p n)))
        (unless (line-offset p 1)
          (return))))
    (move-to-end-of-buffer)
    (delete-blank-lines)))

(define-command load-library (name)
    ((list (prompt-for-library "load library: " 'load-library)))
  (message "Loading ~A." name)
  (cond #+quicklisp((ignore-errors (ql:quickload (format nil "lem-~A" name) :silent t))
                    (message "Loaded ~A." name))
        (t (message "Can't find Library ~A." name))))

(defun get-git-hash (&optional (system :lem))
  (let* ((component (asdf:find-system system))
         (path (when component
                 (asdf:component-pathname component)))
         (git-path (when path
                     (merge-pathnames ".git/" path))))
    (when (uiop:directory-exists-p git-path)
      (uiop:with-current-directory (path)
        (string-trim
         (list #\Newline #\Space)
         (with-output-to-string (stream)
           (uiop:run-program "git rev-parse --short HEAD"
                             :output stream)))))))

(defvar *git-revision* (get-git-hash :lem))

(define-command lem-version (&optional name) ("p")
  (let ((version
          (format nil "lem ~A~@[-~A~] (~A-~A)"
                  (asdf:component-version (asdf:find-system :lem))
                  *git-revision*
                  (machine-type)
                  (machine-instance))))
    (when (eql name 1)
      (message "~A" version))
    version))

(define-command compare-windows (ignore-whitespace) ("p")
  (setf ignore-whitespace (/= ignore-whitespace 1))
  (when (one-window-p)
    (editor-error "Separate window for compare-windows."))
  (flet ((next-char (p)
           (loop
             :for c := (character-at p)
             :do (when (not (and ignore-whitespace
                                 (syntax-space-char-p c)))
                   (return c))
                 (unless (character-offset p 1)
                   (return nil)))))
    (loop :with window1 := (current-window)
          :with window2 := (get-next-window window1)
          :with p1 := (window-point window1)
          :with p2 := (window-point window2)
          :for c1 := (next-char p1)
          :for c2 := (next-char p2)
          :until (or (null c1)
                     (null c2)
                     (not (eql c1 c2)))
          :while (and (character-offset p1 1)
                      (character-offset p2 1)))))
