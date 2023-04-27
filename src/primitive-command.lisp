(in-package :lem)

(defvar *set-location-hook* '())

(defclass movable-advice () ())
(defclass jump-cursor-advice () ())
(defclass editable-advice () ())

(define-command undefined-key () ()
  (editor-error "Key not found: ~A"
                (keyseq-to-string (last-read-key-sequence))))

(define-command exit-lem (&optional (ask t)) ()
  (when (or (null ask)
            (not (any-modified-buffer-p))
            (prompt-for-y-or-n-p "Modified buffers exist. Leave anyway"))
    (exit-editor)))

(define-command quick-exit () ()
  (save-some-buffers t)
  (exit-editor))

(define-command keyboard-quit () ()
  (error 'editor-abort))

(define-command escape () ()
  (error 'editor-abort :message nil))

(define-command nop-command () ())

(define-command unmark-buffer () ()
  (buffer-unmark (current-buffer)))

(defvar *read-only-function* nil)

(define-command toggle-read-only () ()
  (setf (buffer-read-only-p (current-buffer))
        (not (buffer-read-only-p (current-buffer))))
  (when *read-only-function*
    (funcall *read-only-function*
             (buffer-read-only-p (current-buffer)))))

(define-command rename-buffer (name) ("sRename buffer: ")
  (buffer-rename (current-buffer) name))

(define-command quoted-insert (&optional (n 1)) ("p")
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (self-insert-aux char n)))

(define-command (newline (:advice-classes editable-advice)) (&optional (n 1)) ("p")
  (self-insert-aux #\newline n))

(define-command (open-line (:advice-classes editable-advice)) (n) ("p")
  (self-insert-aux #\newline n t))

(define-command (delete-next-char (:advice-classes editable-advice)) (&optional n) ("P")
  (unless (end-buffer-p (current-point))
    (let ((repeat-command (continue-flag :kill))
          (killp (not (null n)))
          (killed-string (delete-character (current-point) (or n 1))))
      (when killp
        (with-killring-context (:appending repeat-command)
          (copy-to-clipboard-with-killring killed-string))))))

(defun delete-cursor-region (point)
  (let ((start (cursor-region-beginning point))
        (end (cursor-region-end point)))
    (delete-character start (count-characters start end))))

(defun delete-previous-char-1 (n)
  (backward-char (or n 1))
  (handler-case (with-killring-context (:before-inserting t)
                  (delete-next-char n))
    (read-only-error (e)
      (forward-char (or n 1))
      (error e))))

(define-command (delete-previous-char (:advice-classes editable-advice)) (&optional n) ("P")
  (cond ((mark-active-p (cursor-mark (current-point)))
         (delete-cursor-region (current-point)))
        (t
         (delete-previous-char-1 n))))

(defun copy-cursor-region (point)
  (with-killring-context (:appending (continue-flag :kill))
    (let ((start (cursor-region-beginning point))
          (end (cursor-region-end point)))
      (copy-to-clipboard-with-killring (points-to-string start end)))
    (mark-cancel (cursor-mark point))))

(define-command copy-region (start end) ("r")
  (with-killring-context (:appending (continue-flag :kill))
    (copy-to-clipboard-with-killring (points-to-string start end)))
  (buffer-mark-cancel (current-buffer)))

(define-command copy-region-to-clipboard (start end) ("r")
  (copy-to-clipboard (points-to-string start end)))

(defun kill-cursor-region (point)
  (let* ((start (cursor-region-beginning point))
         (end (cursor-region-end point))
         (killed-string (delete-character start (count-characters start end))))
    (with-killring-context (:appending (continue-flag :kill))
      (copy-to-clipboard-with-killring killed-string))
    (mark-cancel (cursor-mark point))))

(define-command kill-region (start end) ("r")
  (when (point< end start)
    (rotatef start end))
  (let ((repeat-command (continue-flag :kill)))
    (let ((killed-string (delete-character start (count-characters start end))))
      (with-killring-context (:appending repeat-command)
        (copy-to-clipboard-with-killring killed-string)))))

(define-command kill-region-to-clipboard (start end) ("r")
  (copy-region-to-clipboard start end)
  (delete-character start (count-characters start end)))

(define-command (kill-line (:advice-classes editable-advice)) (&optional arg) ("P")
  (save-excursion
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
           (kill-region start end)))))))

(defun yank-1 (arg)
  (let ((string (if (null arg)
                    (yank-from-clipboard-or-killring)
                    (peek-killring-item (current-killring) (1- arg)))))
    (change-yank-start (current-point)
                       (copy-point (current-point) :right-inserting))
    (insert-string (current-point) string)
    (change-yank-end (current-point)
                     (copy-point (current-point) :left-inserting))
    (continue-flag :yank)))

(define-command yank (&optional arg) ("P")
  (yank-1 arg))

(define-command (yank-pop (:advice-classes editable-advice)) (&optional n) ("p")
  (let ((start (cursor-yank-start (current-point)))
        (end (cursor-yank-end (current-point)))
        (prev-yank-p (continue-flag :yank)))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (rotate-killring (current-killring))
           (yank-1 n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command (yank-pop-next (:advice-classes editable-advice)) (&optional n) ("p")
  (let ((start (cursor-yank-start (current-point)))
        (end (cursor-yank-end (current-point)))
        (prev-yank-p (continue-flag :yank)))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (rotate-killring-undo (current-killring))
           (yank-1 n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command yank-to-clipboard (&optional arg) ("p")
  (let ((string
          (peek-killring-item (current-killring)
                              (if (null arg) 0 (1- arg)))))
    (copy-to-clipboard string)))

(define-command (paste-from-clipboard (:advice-classes editable-advice)) () ()
  (insert-string (current-point) (get-clipboard-data)))

(defun next-line-aux (n
                      point-column-fn
                      forward-line-fn
                      move-to-column-fn)
  (if (continue-flag :next-line)
      (assert (not (null (cursor-saved-column (current-point)))))
      (setf (cursor-saved-column (current-point))
            (funcall point-column-fn (current-point))))
  (unless (prog1 (funcall forward-line-fn (current-point) n)
            (funcall move-to-column-fn (current-point) (cursor-saved-column (current-point))))
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

(define-command (previous-line (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-line (- n)))

(define-command (previous-logical-line (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-logical-line (- n)))

(define-command (forward-char (:advice-classes movable-advice))
    (&optional (n 1)) ("p")
  (or (character-offset (current-point) n)
      (error 'end-of-buffer :point (current-point))))

(define-command (backward-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (or (character-offset (current-point) (- n))
      (error 'beginning-of-buffer :point (current-point))))

(define-command (move-to-beginning-of-buffer (:advice-classes jump-cursor-advice)) () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-start (current-point)))

(define-command (move-to-end-of-buffer (:advice-classes jump-cursor-advice)) () ()
  (run-hooks *set-location-hook* (current-point))
  (buffer-end (current-point)))

(define-command (move-to-beginning-of-line (:advice-classes movable-advice)) () ()
  (let ((bol (backward-line-wrap (copy-point (current-point) :temporary)
                                 (current-window)
                                 t)))
    (or (text-property-at (current-point) :field -1)
        (previous-single-property-change (current-point)
                                         :field
                                         bol)
        (move-point (current-point) bol))))

(define-command (move-to-beginning-of-logical-line (:advice-classes movable-advice)) () ()
  (line-start (current-point)))

(define-command (move-to-end-of-line (:advice-classes movable-advice)) () ()
  (or (and (forward-line-wrap (current-point) (current-window))
           (character-offset (current-point) -1))
      (line-end (current-point))))

(define-command (move-to-end-of-logical-line (:advice-classes movable-advice)) () ()
  (line-end (current-point)))

(define-command (next-page (:advice-classes movable-advice)) (&optional n) ("P")
  (if n
      (scroll-down n)
      (progn
        (next-line (1- (window-height (current-window))))
        (window-recenter (current-window)))))

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

(define-command (entab-line (:advice-classes editable-advice)) (n) ("p")
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-command (detab-line (:advice-classes editable-advice)) (n) ("p")
  (tab-line-aux n
                (lambda (n)
                  (make-string (* n (variable-value 'tab-width))
                               :initial-element #\space))))

(define-command (next-page-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (let ((point (current-point)))
    (dotimes (_ (abs n))
      (loop
        (unless (line-offset point (if (plusp n) 1 -1))
          (return-from next-page-char))
        (when (eql #\page (character-at point 0))
          (return))))))

(define-command (previous-page-char (:advice-classes movable-advice)) (&optional (n 1)) ("p")
  (next-page-char (- n)))

(define-command (delete-blank-lines (:advice-classes editable-advice)) () ()
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

(defun delete-while-whitespaces (ignore-newline-p)
  (let ((n (skip-chars-forward (current-point)
                               (if ignore-newline-p
                                   '(#\space #\tab)
                                   '(#\space #\tab #\newline)))))
    (delete-character (current-point) (- n))))

(define-command (just-one-space (:advice-classes editable-advice)) () ()
  (skip-whitespace-backward (current-point) t)
  (delete-while-whitespaces t)
  (insert-character (current-point) #\space 1))

(define-command (delete-indentation (:advice-classes editable-advice)) () ()
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

(define-command (transpose-characters (:advice-classes editable-advice)) () ()
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

(define-command (back-to-indentation-command (:advice-classes movable-advice)) () ()
  (back-to-indentation (current-point)))

(define-command undo (n) ("p")
  ;; TODO: multiple cursors
  (dotimes (_ n t)
    (unless (buffer-undo (current-point))
      (editor-error "Undo Error"))))

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

(define-command (increment (:advice-classes editable-advice)) () ()
  (*crement-aux #'1+))

(define-command (decrement (:advice-classes editable-advice)) () ()
  (*crement-aux #'1-))

(define-command mark-set () ()
  (run-hooks *set-location-hook* (current-point))
  (set-cursor-mark (current-point) (current-point))
  (message "Mark set"))

(define-command exchange-point-mark () ()
  (check-marked)
  (alexandria:when-let ((mark (mark-point (cursor-mark (current-point)))))
    (with-point ((current (current-point)))
      (move-point (current-point) mark)
      (set-cursor-mark (current-point) current))))

(define-command (mark-set-whole-buffer (:advice-classes jump-cursor-advice)) () ()
  (buffer-end (current-point))
  (set-current-mark (current-point))
  (buffer-start (current-point))
  (message "Mark set whole buffer"))

(define-command (goto-line (:advice-classes jump-cursor-advice)) (n) ("nLine to GOTO: ")
  (cond ((< n 1)
         (setf n 1))
        ((< #1=(buffer-nlines (current-buffer)) n)
         (setf n #1#)))
  (run-hooks *set-location-hook* (current-point))
  (line-offset (buffer-start (current-point)) (1- n)))

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
            (line-offset (current-point) 0 charpos)))))))

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

(defun buffer-context-menu (buffer)
  (buffer-value buffer 'context-menu))

(defun (setf buffer-context-menu) (context-menu buffer)
  (setf (buffer-value buffer 'context-menu) context-menu))

(define-command show-context-menu () ()
  (let ((context-menu (buffer-context-menu (current-buffer))))
    (when context-menu
      (lem-if:display-context-menu (implementation) context-menu))))

(define-command font-size-increase () ()
  (lem-if:increase-font-size (implementation)))

(define-command font-size-decrease () ()
  (lem-if:decrease-font-size (implementation)))
