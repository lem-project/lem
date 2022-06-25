(in-package :lem)

(export '(*set-location-hook*
          exit-lem
          quick-exit
          keyboard-quit
          nop-command
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
          move-to-beginning-of-logical-line
          move-to-end-of-line
          move-to-end-of-logical-line
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

(defclass movable-advice () ())
(defclass jump-cursor-advice () ())

(defmethod execute :around ((command movable-advice) argument)
  (do-multiple-cursors (:only-fake-cursors t)
    (handler-case
        (call-next-method)
      (move-cursor-error ())))
  (call-next-method))

(defmethod execute :around ((command jump-cursor-advice) argument)
  (prog1 (call-next-method)
    (clear-cursors (current-buffer))))

(define-key *global-keymap* "C-x C-c" 'exit-lem)
(define-command exit-lem (&optional (ask t)) ()
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

(define-key *global-keymap* "Escape" 'escape)
(define-command escape () ()
  (error 'editor-abort :message nil))

(define-key *global-keymap* "NopKey" 'nop-command)
(define-command nop-command () ())

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
    (self-insert-aux char n)))

(define-key *global-keymap* "Return" 'newline)
(define-command newline (&optional (n 1)) ("p")
  (self-insert-aux #\newline n))

(define-key *global-keymap* "C-o" 'open-line)
(define-command open-line (n) ("p")
  (self-insert-aux #\newline n t))

(define-key *global-keymap* "C-d" 'delete-next-char)
(define-key *global-keymap* "Delete" 'delete-next-char)
(define-command delete-next-char (&optional n) ("P")
  ;; TODO: multiple cursors
  (when (end-buffer-p (current-point))
    (error 'end-of-buffer :point (current-point)))
  (let ((repeat-command (continue-flag :kill)))
    (do-multiple-cursors ()
      (let ((killp (not (null n)))
            (killed-string (delete-character (current-point) (or n 1))))
        (when killp
          (with-killring (:new (not repeat-command))
            (kill-push killed-string)))))))

(define-key *global-keymap* "C-h" 'delete-previous-char)
(define-key *global-keymap* "Backspace" 'delete-previous-char)
(define-command delete-previous-char (&optional n) ("P")
  (if (and (buffer-mark (current-buffer))
           (buffer-mark-p (current-buffer)))
      ;; TODO: multiple cursors
      (let ((start (region-beginning))
            (end (region-end)))
        (delete-character start (count-characters start end)))
      (do-multiple-cursors ()
        (backward-char (or n 1))
        (handler-case (let ((*kill-before-p* t))
                        (delete-next-char n))
          (read-only-error (e)
            (forward-char (or n 1))
            (error e))))))

(define-key *global-keymap* "M-w" 'copy-region)
(define-command copy-region (start end) ("r")
  ;; TODO: multiple cursors
  (with-killring (:new (not (continue-flag :kill)))
    (kill-push (points-to-string start end)))
  (buffer-mark-cancel (current-buffer))
  t)

(define-command copy-region-to-clipboard (start end) ("r")
  (copy-to-clipboard (points-to-string start end)))

(define-key *global-keymap* "C-w" 'kill-region)
(define-command kill-region (start end) ("r")
  ;; TODO: multiple cursors
  (when (point< end start)
    (rotatef start end))
  (let ((repeat-command (continue-flag :kill)))
    (do-multiple-cursors ()
      (let ((killed-string (delete-character start (count-characters start end))))
        (with-killring (:new (not repeat-command))
          (kill-push killed-string))))))

(define-command kill-region-to-clipboard (start end) ("r")
  ;; TODO: multiple cursors
  (copy-region-to-clipboard start end)
  (delete-character start (count-characters start end)))

(define-key *global-keymap* "C-k" 'kill-line)
(define-command kill-line (&optional arg) ("P")
  ;; TODO: multiple cursors
  (with-point ((start (current-point) :right-inserting))
    (cond
      ((null arg)
       (let ((p (current-point)))
         (cond ((end-buffer-p p)
                (error 'end-of-buffer :point p))
               ((end-line-p p)
                (character-offset p 1))
               (t (line-end p)))
         (kill-region start p)))
      (t
       (or (line-offset (current-point) arg)
           (buffer-end (current-point)))
       (let ((end (current-point)))
         (kill-region start end))))))

(define-key *global-keymap* "C-y" 'yank)
(define-command yank (&optional arg) ("P")
  ;; TODO: multiple cursors
  (let ((string (if (null arg)
                    (current-kill-ring)
                    (kill-ring-nth (1- arg)))))
    (setf (buffer-value (current-buffer) 'yank-start) (copy-point (current-point) :temporary))
    (insert-string (current-point) string)
    (setf (buffer-value (current-buffer) 'yank-end) (copy-point (current-point) :temporary))
    (continue-flag :yank)
    t))

(define-key *global-keymap* "M-y" 'yank-pop)
(define-command yank-pop (&optional n) ("p")
  ;; TODO: multiple cursors
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
  ;; TODO: multiple cursors
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
  ;; TODO: multiple cursors
  (let ((string (if (null arg)
                    (current-kill-ring)
                    (kill-ring-nth (1- arg)))))
    (copy-to-clipboard string)
    t))

(define-command paste-from-clipboard () ()
  (do-multiple-cursors ()
    (insert-string (current-point) (get-clipboard-data)))
  t)

(define-key *global-keymap* "C-n" 'next-line)
(define-key *global-keymap* "Down" 'next-line)

(let ((saved-column nil))

  (defun get-next-line-context-column ()
    saved-column)

  (defun save-next-line-context-column (column)
    (setf saved-column column)))

(defun next-line-aux (n
                      point-column-fn
                      forward-line-fn
                      move-to-column-fn)
  (unless (continue-flag :next-line)
    (save-next-line-context-column (funcall point-column-fn (current-point))))
  (unless (prog1 (funcall forward-line-fn (current-point) n)
            (funcall move-to-column-fn (current-point) (get-next-line-context-column)))
    (cond ((plusp n)
           (move-to-end-of-buffer)
           (error 'end-of-buffer :point (current-point)))
          ((minusp n)
           (move-to-beginning-of-buffer)
           (error 'beginning-of-buffer :point (current-point))))))

(define-command (next-line (:advice-classes movable-advice)) (&optional n) ("p")
  (next-line-aux n
                 #'point-virtual-line-column
                 #'move-to-next-virtual-line
                 #'move-to-virtual-line-column))

(define-command (next-logical-line (:advice-classes movable-advice)) (&optional n) ("p")
  (next-line-aux n
                 #'point-column
                 #'line-offset
                 #'move-to-column))

(define-key *global-keymap* "C-p" 'previous-line)
(define-key *global-keymap* "Up" 'previous-line)
(define-command (previous-line (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-line (- n)))

(define-command (previous-logical-line (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-logical-line (- n)))

(define-key *global-keymap* "C-f" 'forward-char)
(define-key *global-keymap* "Right" 'forward-char)
(define-command (forward-char (:advice-classes movable-advice))
    (&optional (n 1)) ("p")
  (or (character-offset (current-point) n)
      (error 'end-of-buffer :point (current-point))))

(define-key *global-keymap* "C-b" 'backward-char)
(define-key *global-keymap* "Left" 'backward-char)
(define-command (backward-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (or (character-offset (current-point) (- n))
      (error 'beginning-of-buffer :point (current-point))))

(define-key *global-keymap* "M-<" 'move-to-beginning-of-buffer)
(define-command (move-to-beginning-of-buffer (:advice-classes jump-cursor-advice)) () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-start (current-point))
  t)

(define-key *global-keymap* "M->" 'move-to-end-of-buffer)
(define-command (move-to-end-of-buffer (:advice-classes jump-cursor-advice)) () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-end (current-point))
  t)

(define-key *global-keymap* "C-a" 'move-to-beginning-of-line)
(define-key *global-keymap* "Home" 'move-to-beginning-of-line)
(define-command (move-to-beginning-of-line (:advice-classes movable-advice)) () ()
  (let ((bol (backward-line-wrap (copy-point (current-point) :temporary)
                                 (current-window)
                                 t)))
    (or (text-property-at (current-point) :field -1)
        (previous-single-property-change (current-point)
                                         :field
                                         bol)
        (move-point (current-point) bol)))
  t)
(define-command (move-to-beginning-of-logical-line (:advice-classes movable-advice)) () ()
  (line-start (current-point))
  t)

(define-key *global-keymap* "C-e" 'move-to-end-of-line)
(define-key *global-keymap* "End" 'move-to-end-of-line)
(define-command (move-to-end-of-line (:advice-classes movable-advice)) () ()
  (or (and (forward-line-wrap (current-point) (current-window))
           (character-offset (current-point) -1))
      (line-end (current-point)))
  t)
(define-command (move-to-end-of-logical-line (:advice-classes movable-advice)) () ()
  (line-end (current-point))
  t)

(define-key *global-keymap* "C-v" 'next-page)
(define-key *global-keymap* "PageDown" 'next-page)
(define-command (next-page (:advice-classes movable-advice)) (&optional n) ("P")
  (if n
      (scroll-down n)
      (progn
        (next-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

(define-key *global-keymap* "M-v" 'previous-page)
(define-key *global-keymap* "PageUp" 'previous-page)
(define-command (previous-page (:advice-classes movable-advice)) (&optional n) ("P")
  (if n
      (scroll-up n)
      (progn
        (previous-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

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
  (do-multiple-cursors ()
    (tab-line-aux n
                  #'(lambda (n)
                      (make-string n :initial-element #\tab)))))

(define-command detab-line (n) ("p")
  (do-multiple-cursors ()
    (tab-line-aux n
                  (lambda (n)
                    (make-string (* n (variable-value 'tab-width))
                                 :initial-element #\space)))))

(define-key *global-keymap* "C-x ]" 'next-page-char)
(define-command (next-page-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (let ((point (current-point)))
    (dotimes (_ (abs n))
      (loop
        (unless (line-offset point (if (plusp n) 1 -1))
          (return-from next-page-char))
        (when (eql #\page (character-at point 0))
          (return))))))

(define-key *global-keymap* "C-x [" 'previous-page-char)
(define-command (previous-page-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-page-char (- n)))

(define-key *global-keymap* "C-x C-o" 'delete-blank-lines)
(define-command delete-blank-lines () ()
  (do-multiple-cursors ()
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
              (return)))))))

(defun delete-while-whitespaces (ignore-newline-p)
  (let ((n (skip-chars-forward (current-point)
                               (if ignore-newline-p
                                   '(#\space #\tab)
                                   '(#\space #\tab #\newline)))))
    (delete-character (current-point) (- n))))

(define-key *global-keymap* "M-Space" 'just-one-space)
(define-command just-one-space () ()
  (do-multiple-cursors ()
    (skip-whitespace-backward (current-point) t)
    (delete-while-whitespaces t)
    (insert-character (current-point) #\space 1))
  t)

(define-key *global-keymap* "M-^" 'delete-indentation)
(define-command delete-indentation () ()
  (do-multiple-cursors ()
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
          (insert-character p #\space))))))

(define-key *global-keymap* "C-t" 'transpose-characters)
(define-command transpose-characters () ()
  (do-multiple-cursors ()
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
               (insert-string point (format nil "~C~C" c1 c2))))))))

(define-key *global-keymap* "M-m" 'back-to-indentation-command)
(define-command (back-to-indentation-command (:advice-classes movable-advice)) () ()
  (back-to-indentation (current-point))
  t)

(define-key *global-keymap* "C-\\" 'undo)
(define-command undo (n) ("p")
  ;; TODO: multiple cursors
  (dotimes (_ n t)
    (unless (buffer-undo (current-point))
      (editor-error "Undo Error"))))

(define-key *global-keymap* "C-_" 'redo)
(define-command redo (n) ("p")
  ;; TODO: multiple cursors
  (dotimes (_ n t)
    (unless (buffer-redo (current-point))
      (editor-error "Redo Error"))))

(defun *crement-aux (fn)
  (let ((point (current-point)))
    (skip-symbol-backward point)
    (with-point ((start point))
      (skip-symbol-forward point)
      (let ((word (points-to-string start point)))
        (let ((n (handler-case (parse-integer word)
                   (error ()
                     (editor-error "not integer")))))
          (delete-between-points start point)
          (insert-string point (princ-to-string (funcall fn n))))))))

(define-command increment () ()
  (do-multiple-cursors ()
    (*crement-aux #'1+)))

(define-command decrement () ()
  (do-multiple-cursors ()
    (*crement-aux #'1-)))

(define-key *global-keymap* "C-@" 'mark-set)
(define-key *global-keymap* "C-Space" 'mark-set)
(define-command mark-set () ()
  ;; TODO: multiple cursors
  (run-hooks *set-location-hook* (current-point))
  (set-current-mark (current-point))
  (message "Mark set"))

(define-key *global-keymap* "C-x C-x" 'exchange-point-mark)
(define-command exchange-point-mark () ()
  ;; TODO: multiple cursors
  (check-marked)
  (let ((mark (buffer-mark (current-buffer)))
        (point (copy-point (buffer-point (current-buffer)) :temporary)))
    (move-point (current-point) mark)
    (set-current-mark point))
  t)

(define-key *global-keymap* "C-x h" 'mark-set-whole-buffer)
(define-command (mark-set-whole-buffer (:advice-classes jump-cursor-advice)) () ()
  (buffer-end (current-point))
  (set-current-mark (current-point))
  (buffer-start (current-point))
  (message "Mark set whole buffer"))

(define-key *global-keymap* "M-g" 'goto-line)
(define-command (goto-line (:advice-classes jump-cursor-advice)) (n) ("nLine to GOTO: ")
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
          (when (zerop status)
            (delete-between-points start end)
            (insert-string start output-string)
            (move-to-line (current-point) line-number)
            (line-offset (current-point) 0 charpos)
            t))))))

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

(define-command delete-trailing-whitespace (&optional (buffer (current-buffer))) ()
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
    ((prompt-for-library "load library: " :history-symbol 'load-library))
  (message "Loading ~A." name)
  (cond ((ignore-errors (maybe-quickload (format nil "lem-~A" name) :silent t))
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
      (show-message (princ-to-string version)))
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
