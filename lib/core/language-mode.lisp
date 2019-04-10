(defpackage :lem.language-mode
  (:use :cl :lem :lem.sourcelist)
  (:export
   :*language-mode-keymap*
   :idle-function
   :beginning-of-defun-function
   :end-of-defun-function
   :line-comment
   :insertion-line-comment
   :find-definitions-function
   :find-references-function
   :xref-mode-tag
   :completion-spec
   :go-to-location
   :indent
   :newline-and-indent
   :indent-region-command
   :xref-headline-attribute
   :xref-content-attribute
   :xref-insert-content
   :make-xref-location
   :make-xref-references
   :xref-location-filespec
   :xref-location-position
   :xref-location-content
   :xref-references-type
   :xref-references-locations
   :xref-filespec-to-buffer
   :xref-filespec-to-filename
   :move-to-xref-location-position
   :indent-size)
  #+sbcl
  (:lock t))
(in-package :lem.language-mode)

(define-editor-variable idle-function nil)
(define-editor-variable beginning-of-defun-function nil)
(define-editor-variable end-of-defun-function nil)
(define-editor-variable line-comment nil)
(define-editor-variable insertion-line-comment nil)
(define-editor-variable find-definitions-function nil)
(define-editor-variable find-references-function nil)
(define-editor-variable xref-mode-tag nil)
(define-editor-variable completion-spec nil)
(define-editor-variable indent-size 2)

(defun prompt-for-symbol (prompt history-name)
  (prompt-for-line prompt "" nil nil history-name))

(defvar *idle-timer* nil)

(defun language-idle-function ()
  (alexandria:when-let ((fn (variable-value 'idle-function :buffer)))
    (funcall fn)))

(define-major-mode language-mode ()
    (:keymap *language-mode-keymap*)
  (when (or (null *idle-timer*)
            (not (timer-alive-p *idle-timer*)))
    (setf *idle-timer*
          (start-idle-timer 200 t 'language-idle-function
                            (lambda (condition)
                              (stop-timer *idle-timer*)
                              (pop-up-backtrace condition)
                              (setf *idle-timer* nil))
                            "language-idle-function"))))

(define-key *language-mode-keymap* "C-M-a" 'beginning-of-defun)
(define-key *language-mode-keymap* "C-M-e" 'end-of-defun)
(define-key *language-mode-keymap* "Tab" 'indent-line-and-complete-symbol)
(define-key *global-keymap* "C-j" 'newline-and-indent)
(define-key *global-keymap* "M-j" 'newline-and-indent)
(define-key *language-mode-keymap* "C-M-\\" 'indent-region-command)
(define-key *language-mode-keymap* "M-;" 'comment-or-uncomment-region)
(define-key *language-mode-keymap* "M-." 'find-definitions)
(define-key *language-mode-keymap* "M-_" 'find-references)
(define-key *language-mode-keymap* "M-?" 'find-references)
(define-key *language-mode-keymap* "M-," 'pop-definition-stack)
(define-key *language-mode-keymap* "C-M-i" 'complete-symbol)
(define-key *global-keymap* "M-(" 'insert-\(\))
(define-key *global-keymap* "M-)" 'move-over-\))

(defun beginning-of-defun-1 (n)
  (alexandria:when-let ((fn (variable-value 'beginning-of-defun-function :buffer)))
    (when fn (funcall fn (current-point) n))))

(define-command beginning-of-defun (n) ("p")
  (if (minusp n)
      (end-of-defun (- n))
      (beginning-of-defun-1 n)))

(define-command end-of-defun (n) ("p")
  (if (minusp n)
      (beginning-of-defun (- n))
      (alexandria:if-let ((fn (variable-value 'end-of-defun-function :buffer)))
        (funcall fn (current-point) n)
        (beginning-of-defun-1 (- n)))))

(define-command indent (&optional (n 1)) ("p")
  (if (variable-value 'calc-indent-function)
      (indent-line (current-point))
      (self-insert n)))

(define-command newline-and-indent (n) ("p")
  (with-point ((p (current-point)))
    (newline n)
    (indent)
    (let ((old-charpos (point-charpos p)))
      (line-end p)
      (let ((offset (skip-whitespace-backward p t)))
        (when (plusp offset)
          (delete-character p offset)))
      (line-offset p 0 old-charpos))))

(define-command indent-region-command (start end) ("r")
  (indent-region start end))

(defun space*-p (point)
  (with-point ((point point))
    (skip-whitespace-forward point t)
    (end-line-p point)))

(defun indentation-point-p (point)
  (with-point ((p point))
    (back-to-indentation p)
    (point<= point p)))

(define-command comment-or-uncomment-region () ()
  (if (commented-region-p)
      (uncomment-region)
      (comment-region)))

(defun set-region-point (start end)
  (cond
    ((buffer-mark-p (current-buffer))
     (move-point start (region-beginning))
     (move-point end (region-end)))
    (t
     (line-start start)
     (line-end end))))

(defun commented-region-p ()
  (alexandria:when-let ((line-comment (variable-value 'line-comment :buffer)))
    (with-point ((start (current-point))
                 (end (current-point)))
      (set-region-point start end)
      (loop
        (skip-whitespace-forward start)
        (when (point>= start end)
          (return t))
        (unless (looking-at start line-comment)
          (return nil))
        (unless (line-offset start 1)
          (return t))))))

(define-command comment-region () ()
  (let ((line-comment (or (variable-value 'insertion-line-comment :buffer)
                          (variable-value 'line-comment :buffer))))
    (when line-comment
      (save-excursion
        (with-point ((start (current-point) :right-inserting)
                     (end (current-point) :left-inserting))
          (set-region-point start end)
          (skip-whitespace-forward start)
          (when (point>= start end)
            (insert-string (current-point) line-comment)
            (return-from comment-region))
          (let ((charpos (point-charpos start)))
            (loop
              (when (same-line-p start end)
                (cond ((space*-p start))
                      ((indentation-point-p end))
                      (t
                       (insert-string start line-comment)
                       (unless (space*-p end)
                         (insert-character end #\newline))))
                (return))
              (unless (space*-p start)
                (insert-string start line-comment))
              (line-offset start 1 charpos))))))))

(define-command uncomment-region () ()
  (let* ((line-comment (variable-value 'line-comment :buffer))
         (insertion-line-comment (or (variable-value 'insertion-line-comment :buffer)
                                     line-comment)))
    (when line-comment
      (with-point ((start (current-point) :right-inserting)
                   (end (current-point) :right-inserting))
        (set-region-point start end)
        (let ((p start))
          (loop
            (parse-partial-sexp p end nil t)
            (when (point<= end p) (return))
            (when (looking-at p line-comment)
              (let ((res (looking-at p insertion-line-comment)))
                (if res
                    (delete-character p (length res))
                    (loop :while (looking-at p line-comment)
                          :do (delete-character p (length line-comment))))))
            (unless (line-offset p 1) (return))))))))

(define-attribute xref-headline-attribute
  (t :bold-p t))

(define-attribute xref-content-attribute
  (:dark :foreground "cyan" :bold-p t)
  (:light :foreground "blue" :bold-p t))

(defgeneric xref-insert-content (content point)
  (:method (content point)
    (xref-insert-content (princ-to-string content) point))
  (:method ((content string) point)
   (insert-string point
                  (concatenate 'string
                               "  " content)
                  :attribute 'xref-content-attribute)))

(defstruct xref-location
  (filespec nil :read-only t :type (or buffer string pathname))
  (position 1 :read-only t)
  (content "" :read-only t))

(defstruct xref-references
  (type nil :read-only t)
  (locations nil :read-only t))

(defun xref-filespec-to-buffer (filespec)
  (cond ((bufferp filespec)
         filespec)
        (t
         (assert (or (stringp filespec) (pathnamep filespec)))
         (when (probe-file filespec)
           (or (get-buffer (file-namestring filespec))
               (find-file-buffer filespec))))))

(defun xref-filespec-to-filename (filespec)
  (etypecase filespec
    (buffer (buffer-filename filespec))
    (string filespec)
    (pathname (namestring filespec))))

(defun move-to-xref-location-position (point position)
  (etypecase position
    (integer
     (move-to-position point position))
    (cons
     (move-to-line point (car position))
     (line-offset point 0 (cdr position)))
    (point
     (let ((line-number (line-number-at-point position))
           (charpos (point-charpos position)))
       (move-to-line point line-number)
       (line-offset point 0 charpos)))))

(defun go-to-location (location set-buffer-fn)
  (let ((buffer (xref-filespec-to-buffer (xref-location-filespec location))))
    (unless buffer (editor-error "~A does not exist." (xref-location-filespec location)))
    (funcall set-buffer-fn buffer)
    (let ((position (xref-location-position location))
          (point (buffer-point buffer)))
      (move-to-xref-location-position point position))))

(define-command find-definitions () ()
  (alexandria:when-let (fn (variable-value 'find-definitions-function :buffer))
    (let ((locations (funcall fn (current-point))))
      (unless locations
        (editor-error "No definitions found"))
      (push-location-stack (current-point))
      (setf locations (uiop:ensure-list locations))
      (if (null (rest locations))
          (progn
            (go-to-location (first locations) #'switch-to-buffer)
            (jump-highlighting))
          (let ((prev-file nil))
            (with-sourcelist (sourcelist "*definitions*")
              (dolist (location locations)
                (let ((file (xref-filespec-to-filename (xref-location-filespec location)))
                      (content (xref-location-content location)))
                  (append-sourcelist sourcelist
                                     (lambda (p)
                                       (unless (equal prev-file file)
                                         (insert-string p file :attribute 'xref-headline-attribute)
                                         (insert-character p #\newline))
                                       (xref-insert-content content p))
                                     (let ((location location))
                                       (alexandria:curry #'go-to-location location)))
                  (setf prev-file file)))))))))

(define-command find-references () ()
  (alexandria:when-let (fn (variable-value 'find-references-function :buffer))
    (let ((refs (funcall fn (current-point))))
      (unless refs
        (editor-error "No references found"))
      (push-location-stack (current-point))
      (with-sourcelist (sourcelist "*references*")
        (dolist (ref (uiop:ensure-list refs))
          (let ((type (xref-references-type ref)))
            (when type
              (append-sourcelist sourcelist
                                 (lambda (p)
                                   (insert-string p (princ-to-string type)
                                                  :attribute 'xref-headline-attribute))
                                 nil))
            (dolist (location (xref-references-locations ref))
              (let ((content (xref-location-content location)))
                (append-sourcelist
                 sourcelist
                 (lambda (p)
                   (xref-insert-content content p))
                 (let ((location location))
                   (alexandria:curry #'go-to-location location)))))))))))

(defvar *xref-stack-table* (make-hash-table :test 'equal))
(defvar *xref-history-table* (make-hash-table :test 'equal))

(defun xref-table-key (buffer)
  (or (variable-value 'xref-mode-tag :buffer buffer)
      (buffer-major-mode buffer)))

(defun push-location-stack (point)
  (run-hooks *set-location-hook* point)
  (let* ((buffer (point-buffer point))
         (key (xref-table-key buffer))
         (elt (list (buffer-name buffer)
                    (line-number-at-point point)
                    (point-charpos point))))
    (setf (gethash key *xref-history-table*)
          (cons elt (delete elt (gethash key *xref-history-table*)
                            :test #'equal)))
    (push elt (gethash key *xref-stack-table*))))

(define-command pop-definition-stack () ()
  (let ((elt (pop (gethash (xref-table-key (current-buffer))
                           *xref-stack-table*))))
    (when elt
      (destructuring-bind (buffer-name line-number charpos) elt
        (unless (get-buffer buffer-name)
          (pop-definition-stack)
          (return-from pop-definition-stack))
        (run-hooks *set-location-hook* (current-point))
        (select-buffer buffer-name)
        (move-to-line (current-point) line-number)
        (line-offset (current-point) 0 charpos)
        (jump-highlighting)))))

(define-command complete-symbol () ()
  (alexandria:when-let (fn (variable-value 'completion-spec :buffer))
    (lem.completion-mode:run-completion fn)))

(define-command indent-line-and-complete-symbol () ()
  (if (variable-value 'calc-indent-function :buffer)
      (let* ((p (current-point))
             (old (point-charpos p)))
        (let ((charpos (point-charpos p)))
          (handler-case (indent-line p)
            (editor-condition ()
              (line-offset p 0 charpos))))
        (when (= old (point-charpos p))
          (complete-symbol)))
      (complete-symbol)))

(define-command insert-\(\) () ()
  (let ((p (current-point)))
    (insert-character p #\()
    (insert-character p #\))
    (character-offset p -1)))

(defun backward-search-rper ()
  (save-excursion
    (do* ((p (character-offset (current-point) -1))
          (c (character-at p)
             (character-at p)))
        ((char= #\) c) p)
      (unless (syntax-space-char-p c)
        (return nil))
      (character-offset p -1))))

(defun backward-delete-to-rper ()
  (save-excursion
    (do* ((p (character-offset (current-point) -1))
          (c (character-at p)
             (character-at p)))
        ((char= #\) c) p)
      (unless (syntax-space-char-p c)
        (return nil))
      (delete-character p)
      (character-offset p -1))))

(define-command move-over-\) () ()
  (let ((rper (backward-search-rper)))
    (if rper
        (progn
          (backward-delete-to-rper)
          (scan-lists (current-point) 1 1 T)
          (newline-and-indent 1))
        (progn
          (scan-lists (current-point) 1 1 T)
          (newline-and-indent 1)))))
