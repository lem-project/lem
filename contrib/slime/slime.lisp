(in-package :cl-user)
(defpackage :lem-slime
  (:use :cl :lem)
  (:export :slime-note-attribute
           :slime-entry-attribute
           :slime-headline-attribute
           :*impl-name*)
  (:import-from :lem-slime.errors
                :disconnected)
  (:local-nicknames
   (:swank-protocol :lem-slime.swank-protocol)))
(in-package :lem-slime)

(defparameter *default-port* 4005)

;; (1)とコメントで印を付けているところはコネクションを複数管理するときに今のやり方ではまずいところ

(defvar *slime-prompt-string*) ;(1)
(defvar *connection* nil)
(defvar *write-string-function* 'write-string-to-output-buffer)
(defvar *last-compilation-result* nil)
(defvar *indent-table* (make-hash-table :test 'equal))

(define-attribute slime-note-attribute
  (t :foreground "red" :underline-p t))

(define-attribute slime-entry-attribute
  (:dark :foreground "cyan" :bold-p t)
  (t :foreground "blue" :bold-p t))

(define-attribute slime-headline-attribute
  (t :bold-p t))

(define-major-mode slime-mode nil
    (:name "slime"
     :keymap *slime-mode-keymap*
     :syntax-table lem-lisp-syntax.syntax-table:*syntax-table*)
  (modeline-add-status-list (lambda (window)
                              (buffer-package (window-buffer window) "CL-USER"))
                            (current-buffer))
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'calc-indent))

(define-key *slime-mode-keymap* "C-M-a" 'lem.lisp-mode:lisp-beginning-of-defun)
(define-key *slime-mode-keymap* "C-M-e" 'lem.lisp-mode:lisp-end-of-defun)
(define-key *slime-mode-keymap* "C-c ;" 'lem.lisp-mode:lisp-comment-or-uncomment-region)
(define-key *slime-mode-keymap* "C-M-q" 'slime-indent-sexp)
(define-key *slime-mode-keymap* "C-c M-p" 'slime-set-package)
(define-key *slime-mode-keymap* "C-c C-e" 'slime-eval-last-expression)
(define-key *slime-mode-keymap* "C-M-x" 'slime-eval-defun)
(define-key *slime-mode-keymap* "C-c C-r" 'slime-eval-region)
(define-key *slime-mode-keymap* "C-c C-l" 'slime-load-file)
(define-key *slime-mode-keymap* "Spc" 'slime-space)
(define-key *slime-mode-keymap* "C-c M-c" 'slime-remove-notes)
(define-key *slime-mode-keymap* "C-c C-k" 'slime-compile-and-load-file)
(define-key *slime-mode-keymap* "C-c C-c" 'slime-compile-defun)
(define-key *slime-mode-keymap* "C-c C-m" 'slime-macroexpand)
(define-key *slime-mode-keymap* "C-c M-m" 'slime-macroexpand-all)
(define-key *slime-mode-keymap* "M-." 'slime-edit-definition)
(define-key *slime-mode-keymap* "M-," 'slime-pop-find-definition-stack)
(define-key *slime-mode-keymap* "M-_" 'slime-edit-uses)
(define-key *slime-mode-keymap* "M-?" 'slime-edit-uses)
(define-key *slime-mode-keymap* "C-M-i" 'slime-completion-symbol-at-point)
(define-key *slime-mode-keymap* "C-c C-d C-a" 'slime-echo-arglist)
(define-key *slime-mode-keymap* "C-c C-d a" 'slime-apropos)
(define-key *slime-mode-keymap* "C-c C-d z" 'slime-apropos-all)
(define-key *slime-mode-keymap* "C-c C-d p" 'slime-apropos-package)
(define-key *slime-mode-keymap* "C-c C-d d" 'slime-describe-symbol)
(define-key *slime-mode-keymap* "C-c C-d h" 'slime-documentation-lookup)

(defun connected-p ()
  (not (null *connection*)))

(defun check-connection ()
  (unless (connected-p)
    (editor-error "Not connected.")))

(defun buffer-package (buffer &optional default)
  (or (lem.lisp-mode::lisp-buffer-package buffer)
      default))

(defun current-package ()
  (or (buffer-package (current-buffer))
      (swank-protocol:connection-package *connection*)))

(defun features ()
  (when (connected-p)
    (swank-protocol:connection-features *connection*)))

(defun indentation-update (info)
  (loop :for (name indent packages) :in info :do
    (lem-lisp-syntax.indent:set-indentation name indent))
  #+(or)
  (loop :for (name indent packages) :in info
        :do (dolist (package packages)
              (unless (gethash package *indent-table*)
                (setf (gethash package *indent-table*)
                      (make-hash-table :test 'equal)))
              (setf (gethash name (gethash package *indent-table*)) indent))))

(defun indent-spec (string)
  (when (connected-p)
    (let* ((parts (uiop:split-string string :separator ":"))
           (length (length parts))
           (package))
      (cond ((= length 1)
             (setq package (current-package)))
            ((or (= length 2)
                 (and (= length 3)
                      (string= "" (second parts))))
             (setq package (first parts))))
      (let ((table (gethash package *indent-table*)))
        (when table
          (values (gethash (first (last parts)) table)))))))

(defun calc-indent (point)
  (let ((lem-lisp-syntax.indent:*get-method-function* #'indent-spec))
    (lem-lisp-syntax.indent:calc-indent point)))

(defun get-buffer-from-file (file)
  (dolist (buffer (buffer-list))
    (when (uiop:pathname-equal file (buffer-filename buffer))
      (return buffer))))

(defun slime-eval-internal (emacs-rex-fun rex-arg package)
  (let ((tag (gensym)))
    (catch tag
      (funcall emacs-rex-fun
               *connection*
               rex-arg
               :continuation (lambda (result)
                               (alexandria:destructuring-case result
                                 ((:ok value)
                                  (throw tag value))
                                 ((:abort condition)
                                  (declare (ignore condition))
                                  (editor-error "Synchronous Lisp Evaluation aborted"))))
               :package package
               :thread t)
      (loop (sit-for 10)))))

(defun slime-eval-from-string (string &optional (package (current-package)))
  (slime-eval-internal 'swank-protocol:emacs-rex-string string package))

(defun slime-eval (sexp &optional (package (current-package)))
  (slime-eval-internal 'swank-protocol:emacs-rex sexp package))

(defun eval-async (form &optional cont thread package)
  (swank-protocol:emacs-rex
   *connection*
   form
   :continuation (lambda (value)
                   (alexandria:destructuring-ecase value
                     ((:ok result)
                      (when cont
                        (funcall cont result)))
                     ((:abort condition)
                      (message "Evaluation aborted on ~A." condition))))
   :thread thread
   :package package))

(defun eval-with-transcript (form)
  (swank-protocol:emacs-rex
   *connection*
   form
   :continuation (lambda (value)
                   (alexandria:destructuring-case value
                     ((:ok x)
                      (message "~A" x))
                     ((:abort condition)
                      (message "Evaluation aborted on ~A." condition))))
   :package (current-package)
   :thread t))

(defun re-eval-defvar (string)
  (eval-with-transcript `(swank:re-evaluate-defvar ,string)))

(defun interactive-eval (string)
  (eval-with-transcript `(swank:interactive-eval ,string)))

(defun new-package (name prompt-string)
  (setf (swank-protocol:connection-package *connection*) name)
  (setf *slime-prompt-string* prompt-string)
  t)

(defun read-package-name ()
  (check-connection)
  (let ((package-names (mapcar #'string-downcase
                               (slime-eval
                                '(swank:list-all-package-names t)))))
    (string-upcase (prompt-for-line
                    "Package: " ""
                    (lambda (str)
                      (completion str package-names))
                    (lambda (str)
                      (find str package-names :test #'string=))
                    'mh-slime-package))))

(define-command slime-indent-sexp () ()
  (lem.lisp-mode:lisp-indent-sexp))

(define-command slime-set-package (package-name) ((list (read-package-name)))
  (check-connection)
  (cond ((string= package-name ""))
        ((eq (current-buffer) (repl-buffer))
         (destructuring-bind (name prompt-string)
             (slime-eval `(swank:set-package ,package-name))
           (new-package name prompt-string)
           (lem.listener-mode:listener-reset-prompt (repl-buffer))))
        (t
         (setf (buffer-value (current-buffer) "package") package-name))))

(define-command slime-eval-last-expression () ()
  (check-connection)
  (refresh-output-buffer)
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (interactive-eval (points-to-string start end))))

(define-command slime-eval-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem.lisp-mode::top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (let ((string (points-to-string start end)))
        (refresh-output-buffer)
        (if (ppcre:scan "^\\(defvar\\b" string)
            (re-eval-defvar string)
            (interactive-eval string))))))

(define-command slime-eval-region (start end) ("r")
  (check-connection)
  (refresh-output-buffer)
  (interactive-eval (points-to-string start end)))

(define-command slime-load-file (filename) ("fLoad File: ")
  (check-connection)
  (when (uiop:pathname-equal filename (buffer-directory))
    (setf filename (buffer-filename (current-buffer))))
  (when (and (cl-fad:file-exists-p filename)
             (not (cl-fad:directory-pathname-p filename)))
    (refresh-output-buffer)
    (eval-with-transcript `(swank:load-file ,filename))))

(defun get-operator-name ()
  (with-point ((point (current-point)))
    (scan-lists point -1 1)
    (character-offset point 1)
    (symbol-string-at-point point)))

(define-command slime-echo-arglist () ()
  (check-connection)
  (let ((name (get-operator-name))
        (package (current-package)))
    (when name
      (eval-async `(swank:operator-arglist ,name ,package)
                  (lambda (arglist)
                    (when arglist
                      (message "~A" (ppcre:regex-replace-all "\\s+" arglist " "))))
                  t))))

(define-command slime-space (n) ("p")
  (insert-character (current-point) #\space n)
  (slime-echo-arglist))

(defun check-parens ()
  (with-point ((point (current-point)))
    (buffer-start point)
    (loop :while (form-offset point 1))
    (skip-space-and-comment-forward point)
    (end-buffer-p point)))

(defun compilation-finished (result)
  (setf *last-compilation-result* result)
  (destructuring-bind (notes successp duration loadp fastfile)
      (rest result)
    (show-compile-result notes duration
                         (if (not loadp)
                             successp
                             (and fastfile successp)))
    (highlight-notes notes)
    (when (and loadp fastfile successp)
      (eval-async `(swank:load-file ,fastfile) nil t))))

(defun show-compile-result (notes secs successp)
  (message (format nil "~{~A~^ ~}"
                   (remove-if #'null
                              (list (if successp
                                        "Compilation finished"
                                        "Compilation failed")
                                    (unless notes
                                      "(No warnings)")
                                    (when secs
                                      (format nil "[~,2f secs]" secs)))))))

(defun make-highlight-overlay (pos buffer)
  (with-point ((point (buffer-point buffer)))
    (move-to-position point pos)
    (skip-chars-backward point #'syntax-symbol-char-p)
    (make-overlay point
                  (or (form-offset (copy-point point :temporary) 1)
                      (buffer-end-point buffer))
                  'slime-note-attribute)))

(defvar *note-overlays* nil)

(defun highlight-notes (notes)
  (slime-remove-notes)
  (let ((overlays '()))
    (lem.sourcelist:with-sourcelist (sourcelist "*slime-compilations*")
      (dolist (note notes)
        (optima:match note
          ((and (optima:property :location
                                 (or (list :location
                                           (list :buffer buffer-name)
                                           (list :offset pos _)
                                           _)
                                     (list :location
                                           (list :file file)
                                           (list :position pos)
                                           _)))
                (or (optima:property :message message) (and))
                (or (optima:property :source-context source-context) (and)))
           (let ((jump-fun (if buffer-name
                               (lambda ()
                                 (let ((buffer (get-buffer buffer-name)))
                                   (when buffer
                                     (setf (current-window) (pop-to-buffer buffer))
                                     (move-to-position (current-point) pos))))
                               (lambda ()
                                 (find-file file)
                                 (move-to-position (current-point) pos)))))
             (lem.sourcelist:append-sourcelist
              sourcelist
              (let ((name (or buffer-name file)))
                (lambda (cur-point)
                  (insert-string cur-point name :attribute 'lem.grep:title-attribute)
                  (insert-string cur-point ":")
                  (insert-string cur-point (princ-to-string pos) :attribute 'lem.grep:position-attribute)
                  (insert-string cur-point ":")
                  (insert-character cur-point #\newline 1)
                  (insert-string cur-point message)
                  (insert-character cur-point #\newline)
                  (insert-string cur-point source-context)))
              jump-fun)
             (push (make-highlight-overlay pos
                                           (if buffer-name
                                               (get-buffer buffer-name)
                                               (get-buffer-from-file file)))
                   overlays))))))
    (when overlays
      (setf *note-overlays* overlays))))

(define-command slime-remove-notes () ()
  (mapc #'delete-overlay *note-overlays*))

(define-command slime-compile-and-load-file () ()
  (check-connection)
  (when (buffer-modified-p (current-buffer))
    (when (prompt-for-y-or-n-p "Save file")
      (save-buffer)))
  (let ((file (buffer-filename (current-buffer))))
    (refresh-output-buffer)
    (eval-async `(swank:compile-file-for-emacs ,file t)
                #'compilation-finished
                t
                (buffer-package (current-buffer)))))

(define-command slime-compile-region (start end) ("r")
  (check-connection)
  (let ((string (lem::points-to-string start end))
        (position `((:position ,(position-at-point start))
                    (:line
                     ,(line-number-at-point (current-point))
                     ,(point-charpos (current-point))))))
    (refresh-output-buffer)
    (eval-async `(swank:compile-string-for-emacs ,string
                                                 ,(buffer-name (current-buffer))
                                                 ',position
                                                 ,(buffer-filename (current-buffer))
                                                 nil)
                #'compilation-finished
                t
                (buffer-package (current-buffer)))))

(define-command slime-compile-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem.lisp-mode::top-of-defun point)
    (with-point ((start point)
                 (end point))
      (form-offset end 1)
      (slime-compile-region start end))))

(defun form-string-at-point ()
  (with-point ((point (current-point)))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (with-point ((start point)
                 (end point))
      (form-offset end 1)
      (points-to-string start end))))

(defun macroexpand-internal (expander buffer-name)
  (let ((string (slime-eval `(,expander ,(form-string-at-point)))))
    (with-pop-up-typeout-window (out (get-buffer-create buffer-name) :focus t :erase t)
      (princ string out))))

(define-command slime-macroexpand () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-1 "*slime-macroexpand*"))

(define-command slime-macroexpand-all () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-all "*slime-macroexpand-all*"))

(defun symbol-completion (str &optional (package (current-package)))
  (let ((result (slime-eval-from-string
                 (format nil "(swank:fuzzy-completions ~S ~S)"
                         str
                         package)
                 "COMMON-LISP")))
    (when result
      (destructuring-bind (completions timeout-p) result
        (declare (ignore timeout-p))
        (completion-hypheen str (mapcar #'first completions))))))

(defun read-symbol-name (prompt &optional (initial ""))
  (let ((package (current-package)))
    (prompt-for-line prompt
                     initial
                     (lambda (str)
                       (symbol-completion str package))
                     nil
                     'mh-read-symbol)))

(defvar *edit-definition-stack* nil)

(defun push-edit-definition (point)
  (push (list (buffer-name (point-buffer point))
              (position-at-point point))
        *edit-definition-stack*))

(define-command slime-edit-definition () ()
  (check-connection)
  (let* ((name (read-symbol-name "Edit Definition of: "
                                 (or (symbol-string-at-point (current-point)) "")))
         (definitions (slime-eval `(swank:find-definitions-for-emacs ,name)))
         (found-list '()))
    (let ((point (lem-lisp-syntax.enclosing:search-local-definition
                  (current-point) name)))
      (when point
        (push-edit-definition (current-point))
        (move-point (current-point) point)
        (return-from slime-edit-definition)))
    (dolist (def definitions)
      (optima:match def
        ((list title
               (list :location
                     (list :file file)
                     (list :position offset)
                     (list :snippet _)))
         (push (list title file offset) found-list))))
    (when found-list
      (push-edit-definition (current-point))
      (if (null (rest found-list))
          (destructuring-bind (title file offset)
              (first found-list)
            (declare (ignore title))
            (find-file file)
            (move-to-position (current-point) offset))
          (let ((prev-file nil))
            (lem.sourcelist:with-sourcelist (sourcelist "*slime-definitions*")
              (dolist (elt found-list)
                (destructuring-bind (title file offset) elt
                  (lem.sourcelist:append-sourcelist
                   sourcelist
                   (lambda (cur-point)
                     (unless (and prev-file (string= prev-file file))
                       (insert-string cur-point file
                                      :attribute 'slime-headline-attribute)
                       (insert-character cur-point #\newline))
                     (insert-string cur-point
                                    (format nil "  ~A" title)
                                    :attribute 'slime-entry-attribute))
                   (lambda ()
                     (find-file file)
                     (move-to-position (current-point) offset)))
                  (setf prev-file file)))))))))

(define-command slime-pop-find-definition-stack () ()
  (let ((elt (pop *edit-definition-stack*)))
    (when elt
      (destructuring-bind (buffer-name position) elt
        (let ((buffer (get-buffer-create buffer-name)))
          (switch-to-buffer buffer)
          (move-to-position (current-point) position))))))

(define-command slime-edit-uses () ()
  (check-connection)
  (let* ((symbol (read-symbol-name "Edit uses of: " (or (symbol-string-at-point (current-point)) "")))
         (result (slime-eval `(swank:xrefs '(:calls :macroexpands :binds
                                             :references :sets :specializes)
                                           ,symbol)))
         (found nil))
    (lem.sourcelist:with-sourcelist (sourcelist "*slime-xrefs*")
      (loop :for (type . definitions) :in result
            :for defs := (loop :for def :in definitions
                               :collect (optima:match def
                                          ((list name
                                                 (list :location
                                                       (list :file file)
                                                       (list :position offset)
                                                       (list :snippet snippet)))
                                           (list name file offset snippet))))
            :do (when defs
                  (setf found t)
                  (lem.sourcelist:append-sourcelist
                   sourcelist
                   (lambda (cur-point)
                     (insert-string cur-point (princ-to-string type)
                                    :attribute 'slime-headline-attribute
                                    ))
                   nil)
                  (loop :for def :in defs
                        :do (destructuring-bind (name file offset snippet) def
                              (declare (ignore snippet))
                              (lem.sourcelist:append-sourcelist
                               sourcelist
                               (lambda (cur-point)
                                 (insert-string cur-point
                                                (format nil "  ~A" name)
                                                :attribute 'slime-entry-attribute))
                               (lambda ()
                                 (find-file file)
                                 (move-to-position (current-point) offset))))))))
    (cond
      (found
       (push-edit-definition (current-point)))
      (t
       (message "No xref information found for ~A" symbol)))))

(define-command slime-completion-symbol-at-point () ()
  (check-connection)
  (with-point ((start (current-point))
               (end (current-point)))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (let ((result
           (slime-eval-from-string (format nil "(swank:fuzzy-completions ~S ~S)"
                                           (points-to-string start end)
                                           (current-package)))))
      (when result
        (destructuring-bind (completions timeout-p) result
          (declare (ignore timeout-p))
          (run-completion
           (mapcar (lambda (completion)
                     (make-completion-item
                      :label (first completion)
                      :detail (fourth completion)
                      :start start
                      :end end))
                   completions)
           :auto-insert nil
           :restart-function 'slime-completion-symbol-at-point)))))
  t)

(defvar *slime-apropos-mode-keymap* (make-keymap nil *slime-mode-keymap*))
(define-key *slime-apropos-mode-keymap* "q" 'quit-window)

(define-major-mode slime-apropos-mode ()
    (:name "slime-apropos"
     :keymap *slime-apropos-mode-keymap*
     :syntax-table lem-lisp-syntax.syntax-table:*syntax-table*))

(defun show-apropos (data)
  (let ((buffer (get-buffer-create "*slime-apropos*")))
    (switch-to-buffer buffer)
    (slime-apropos-mode)
    (erase-buffer buffer)
    (save-excursion
      (let ((point (current-point)))
        (loop :for plist :in data
              :do (let ((designator (cadr plist))
                        (plist1 (cddr plist)))
                    (insert-string point designator
                                   :attribute 'slime-headline-attribute)
                    (loop :for (k v) :on plist1 :by #'cddr
                          :do (insert-string point (format nil "~%  ~A: ~A" k v)))
                    (insert-character point #\newline 2)))))))

(defun slime-apropos-internal (string only-external-p package case-sensitive-p)
  (show-apropos (slime-eval
                 `(swank:apropos-list-for-emacs ,string
                                                ,only-external-p
                                                ,case-sensitive-p
                                                ,package))))

(define-command slime-apropos (&optional arg) ("P")
  (check-connection)
  (let ((string)
        (only-external-p t)
        (package nil)
        (case-sensitive-p nil))
    (if arg
        (setq string (prompt-for-string "SLIME Apropos: ")
              only-external-p (prompt-for-y-or-n-p "External symbols only? ")
              package (let ((name (read-package-name)))
                        (if (string= "" name)
                            nil
                            name))
              case-sensitive-p (prompt-for-y-or-n-p "Case-sensitive? "))
        (setq string (prompt-for-string "SLIME Apropos: ")))
    (slime-apropos-internal string only-external-p package case-sensitive-p)))

(define-command slime-apropos-all () ()
  (check-connection)
  (slime-apropos-internal (prompt-for-string "SLIME Apropos: ")
                          nil nil nil))

(define-command slime-apropos-package (internal) ("P")
  (check-connection)
  (let ((package (read-package-name)))
    (slime-apropos-internal ""
                            (not internal)
                            (if (string= package "")
                                (current-package)
                                package)
                            nil)))

(defun show-description (string)
  (let ((buffer (get-buffer-create "*slime description*")))
    (with-pop-up-typeout-window (stream buffer :erase t)
      (princ string stream))))

(define-command slime-describe-symbol () ()
  (check-connection)
  (let ((symbol-name
         (read-symbol-name "Describe symbol: "
                           (or (symbol-string-at-point (current-point)) ""))))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (show-description (slime-eval `(swank:describe-symbol ,symbol-name)))))

(define-command slime-documentation-lookup () ()
  (check-connection)
  (let ((symbol-name
         (read-symbol-name "Describe symbol: "
                           (or (symbol-string-at-point (current-point)) ""))))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (lem-slime.clhs:main symbol-name)))

(define-major-mode slime-repl-mode slime-mode
    (:name "slime-repl"
     :keymap *slime-repl-mode-keymap*
     :syntax-table lem-lisp-syntax.syntax-table:*syntax-table*)
  (repl-reset-input)
  (lem.listener-mode:listener-mode t))

(defvar *read-string-thread-stack* nil) ;(1)
(defvar *read-string-tag-stack* nil) ;(1)

(define-key *slime-repl-mode-keymap* "C-c C-c" 'slime-repl-interrupt)

(define-command slime-repl-interrupt () ()
  (swank-protocol:send-message-string *connection*
                                      (format nil "(:emacs-interrupt ~(~S~))"
                                              (or (car *read-string-thread-stack*)
                                                  :repl-thread))))

(defun repl-buffer (&optional force)
  (if force
      (get-buffer-create "*slime-repl*")
      (get-buffer "*slime-repl*")))

(defun repl-get-prompt ()
  (format nil "~A> " *slime-prompt-string*))

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (variable-value 'lem.listener-mode:listener-get-prompt-function :buffer buffer)
            'repl-get-prompt
            (variable-value 'lem.listener-mode:listener-check-confirm-function :buffer buffer)
            'lem.lisp-mode:lisp-repl-paren-correspond-p
            (variable-value 'lem.listener-mode:listener-confirm-function :buffer buffer)
            'repl-confirm))))

(defun repl-change-read-line-input ()
  (setf (variable-value 'lem.listener-mode:listener-get-prompt-function)
        (constantly "")
        (variable-value 'lem.listener-mode:listener-check-confirm-function)
        (constantly t)
        (variable-value 'lem.listener-mode:listener-confirm-function)
        'repl-read-line))

(defun repl-confirm (point string)
  (declare (ignore point))
  (check-connection)
  (let ((prev-write-string-function *write-string-function*))
    (swank-protocol:request-listener-eval
     *connection*
     string
     (lambda (value)
       (declare (ignore value))
       (repl-reset-input)
       (lem.listener-mode:listener-reset-prompt (repl-buffer))
       (redraw-display)
       (setf *write-string-function* prev-write-string-function)))
    (setf *write-string-function* 'write-string-to-repl)))

(defun repl-read-string (thread tag)
  (let ((buffer (repl-buffer)))
    (when buffer
      (push thread *read-string-thread-stack*)
      (push tag *read-string-tag-stack*)
      (let ((windows (get-buffer-windows buffer)))
        (setf (current-window)
              (if windows
                  (first windows)
                  (pop-to-buffer buffer))))
      (buffer-end (current-point))
      (lem.listener-mode:listener-update-point)
      (repl-change-read-line-input))))

(defun repl-abort-read (thread tag)
  (declare (ignore thread tag))
  (pop *read-string-thread-stack*)
  (pop *read-string-tag-stack*)
  (message "Read aborted"))

(defun repl-read-line (point string)
  (declare (ignore point))
  (let ((thread (pop *read-string-thread-stack*))
        (tag (pop *read-string-tag-stack*)))
    (swank-protocol:send-message-string
     *connection*
     (format nil "(:emacs-return-string ~A ~A ~S)"
             thread
             tag
             (concatenate 'string string (string #\newline))))))

(define-command slime-repl () ()
  (lem.listener-mode:listener-start "*slime-repl*" 'slime-repl-mode))

(defun write-string-to-repl (string)
  (let ((buffer (repl-buffer)))
    (when buffer
      (with-open-stream (stream (make-buffer-output-stream (lem::buffer-end-point buffer)))
        (princ string stream))
      (lem.listener-mode:listener-update-point (buffer-end-point buffer))
      (when (eq buffer (current-buffer))
        (buffer-end (current-point)))
      (redraw-display))))

(defparameter *fresh-output-buffer-p* t)

(defun write-string-to-output-buffer (string)
  (with-pop-up-typeout-window (stream (get-buffer-create "*slime-output*"))
    (when *fresh-output-buffer-p*
      (setq *fresh-output-buffer-p* nil)
      (fresh-line stream)
      (terpri stream)
      (princ '*** stream)
      (terpri stream))
    (princ string stream)))

(defun refresh-output-buffer ()
  (setq *fresh-output-buffer-p* t))

(defun start-thread ()
  (bt:make-thread (lambda ()
                    (loop
                      (unless (connected-p)
                        (return))
                      (when (swank-protocol:message-waiting-p *connection* :timeout 3)
                        (let ((barrior t))
                          (send-event (lambda ()
                                        (unwind-protect (progn (pull-events)
                                                               (redraw-display))
                                          (setq barrior nil))))
                          (loop
                            (unless (connected-p)
                              (return))
                            (unless barrior
                              (return))
                            (sleep 0.1))))))
                  :name "slime-wait-message"))

(define-command slime-connect (hostname port)
    ((list (prompt-for-string "Hostname: " "localhost")
           (parse-integer (prompt-for-string "Port: " (princ-to-string *default-port*)))))
  (setf *connection* (make-instance 'swank-protocol:connection :hostname hostname :port port))
  (message "Connecting...")
  (handler-case (swank-protocol:connect *connection*)
    (usocket:connection-refused-error (c) (editor-error "~A" c)))
  (message "Swank server running on ~A ~A"
           (swank-protocol:connection-implementation-name *connection*)
           (swank-protocol:connection-implementation-version *connection*))
  (setf *slime-prompt-string*
        (getf (getf (getf (getf (swank-protocol::connection-info *connection*)
                                :return)
                          :ok)
                    :package)
              :prompt))
  (setf lem-lisp-syntax.syntax-table:*get-features-function* 'features)
  (slime-repl)
  (start-thread))

(defun log-message (message)
  (let ((buffer (get-buffer-create "*slime-events*")))
    (with-open-stream (stream (make-buffer-output-stream (lem::buffer-end-point buffer)))
      (print message stream))))

(defvar *unknown-keywords* nil)
(defun pull-events ()
  (when (and (boundp '*connection*)
             (not (null *connection*)))
    (handler-bind ((disconnected (lambda (c)
                                   (declare (ignore c))
                                   (setq *connection* nil))))
      (loop :while (swank-protocol:message-waiting-p *connection*)
            :do (dispatch-message (swank-protocol:read-message *connection*))))))

(defun dispatch-message (message)
  (log-message message)
  (alexandria:destructuring-case message
    ((:write-string string &rest rest)
     (declare (ignore rest))
     (funcall *write-string-function* string))
    ((:read-string thread tag)
     (repl-read-string thread tag))
    ((:read-aborted thread tag)
     (repl-abort-read thread tag))
    ;; ((:open-dedicated-output-stream port coding-system)
    ;;  )
    ((:new-package name prompt-string)
     (new-package name prompt-string))
    ((:return value id)
     (swank-protocol:finish-evaluated *connection* value id))
    ((:debug-activate thread level &optional select)
     (swank-protocol:debugger-in *connection*)
     (active-debugger thread level select))
    ((:debug thread level condition restarts frames conts)
     (start-debugger thread level condition restarts frames conts))
    ((:debug-return thread level stepping)
     (swank-protocol:debugger-out *connection*)
     (exit-debugger thread level stepping))
    ;; ((:channel-send id msg)
    ;;  )
    ;; ((:emacs-channel-send id msg)
    ;;  )
    ;; ((:read-from-minibuffer thread tag prompt initial-value)
    ;;  )
    ;; ((:y-or-n-p thread tag question)
    ;;  )
    ;; ((:emacs-return-string thread tag string)
    ;;  )
    ((:new-features features)
     (setf (swank-protocol:connection-features *connection*)
           features))
    ((:indentation-update info)
     (indentation-update info))
    ;; ((:eval-no-wait form)
    ;;  )
    ;; ((:eval thread tag form-string)
    ;;  )
    ;; ((:emacs-return thread tag value)
    ;;  )
    ;; ((:ed what)
    ;;  )
    ;; ((:inspect what thread tag)
    ;;  )
    ;; ((:background-message message)
    ;;  )
    ;; ((:debug-condition thread message)
    ;;  )
    ((:ping thread tag)
     (swank-protocol:send-message-string
      *connection*
      (format nil "(:emacs-pong ~A ~A)" thread tag)))
    ;; ((:reader-error packet condition)
    ;;  )
    ;; ((:invalid-rpc id message)
    ;;  )
    ;; ((:emacs-skipped-packet _pkg))
    ;; ((:test-delay seconds)
    ;;  )
    ((t &rest args)
     (declare (ignore args))
     (pushnew (car message) *unknown-keywords*))))

(define-major-mode slime-debug-mode ()
    (:name "slime-debug"
     :keymap *slime-debugger-keymap*))

(define-key *slime-debugger-keymap* "q" 'slime-quit-debugger)
(define-key *slime-debugger-keymap* "c" 'slime-continue)
(define-key *slime-debugger-keymap* "a" 'slime-abort)
(define-key *slime-debugger-keymap* "0" 'slime-invoke-restart-0)
(define-key *slime-debugger-keymap* "1" 'slime-invoke-restart-1)
(define-key *slime-debugger-keymap* "2" 'slime-invoke-restart-2)
(define-key *slime-debugger-keymap* "3" 'slime-invoke-restart-3)
(define-key *slime-debugger-keymap* "4" 'slime-invoke-restart-4)
(define-key *slime-debugger-keymap* "5" 'slime-invoke-restart-5)
(define-key *slime-debugger-keymap* "6" 'slime-invoke-restart-6)
(define-key *slime-debugger-keymap* "7" 'slime-invoke-restart-7)
(define-key *slime-debugger-keymap* "8" 'slime-invoke-restart-8)
(define-key *slime-debugger-keymap* "9" 'slime-invoke-restart-9)

(define-command slime-quit-debugger () ()
  (when (swank-protocol:debuggerp *connection*)
    (swank-protocol:emacs-rex *connection* `(swank:throw-to-toplevel))))

(define-command slime-continue () ()
  (when (null (buffer-value (current-buffer) 'restarts))
    (error "slime-continue called outside of debug buffer"))
  (swank-protocol:emacs-rex *connection*
                            '(swank:sldb-continue)
                            :continuation (lambda (value)
                                            (alexandria:destructuring-case value
                                              ((:ok x)
                                               (error "sldb-quit returned [~A]" x))))))

(define-command slime-abort () ()
  (when (swank-protocol:debuggerp *connection*)
    (eval-async '(swank:sldb-abort)
                (lambda (v)
                  (message "Restart returned: ~A" v)))))

(defun slime-invoke-restart (n)
  (check-type n integer)
  (when (swank-protocol:debuggerp *connection*)
    (swank-protocol:emacs-rex *connection*
                              `(swank:invoke-nth-restart-for-emacs
                                ,(buffer-value (current-buffer) 'level -1)
                                ,n))))

(define-command slime-invoke-restart-0 () () (slime-invoke-restart 0))
(define-command slime-invoke-restart-1 () () (slime-invoke-restart 1))
(define-command slime-invoke-restart-2 () () (slime-invoke-restart 2))
(define-command slime-invoke-restart-3 () () (slime-invoke-restart 3))
(define-command slime-invoke-restart-4 () () (slime-invoke-restart 4))
(define-command slime-invoke-restart-5 () () (slime-invoke-restart 5))
(define-command slime-invoke-restart-6 () () (slime-invoke-restart 6))
(define-command slime-invoke-restart-7 () () (slime-invoke-restart 7))
(define-command slime-invoke-restart-8 () () (slime-invoke-restart 8))
(define-command slime-invoke-restart-9 () () (slime-invoke-restart 9))

(defun get-debug-buffer (thread)
  (dolist (buffer (buffer-list))
    (when (eql thread (buffer-value buffer 'thread))
      (return buffer))))

(defun get-debug-buffer-create (thread)
  (or (get-debug-buffer thread)
      (get-buffer-create (format nil "*slime-debugger ~D*" thread))))

(defun exit-debugger (thread level stepping)
  (declare (ignore level stepping))
  (let ((buffer (get-debug-buffer thread)))
    (when buffer
      (cond ((eq buffer (window-buffer (current-window)))
             (quit-window (current-window) t)
             (let* ((repl-buffer (repl-buffer))
                    (repl-window (when repl-buffer
                                   (first (get-buffer-windows repl-buffer)))))
               (when repl-window
                 (setf (current-window) repl-window))))
            (t
             (kill-buffer buffer))))))

(defun start-debugger (thread level condition restarts frames conts)
  (let ((buffer (get-debug-buffer-create thread)))
    (setf (current-window) (display-buffer buffer))
    (slime-debug-mode)
    (setf (swank-protocol:connection-thread *connection*) thread)
    (setf (buffer-value buffer 'thread)
          thread
          (buffer-value buffer 'level)
          level
          (buffer-value buffer 'condition)
          condition
          (buffer-value buffer 'restarts)
          restarts
          (buffer-value buffer 'continuations)
          conts)
    (erase-buffer buffer)
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
              (lambda (buffer)
                (declare (ignore buffer))
                (slime-quit-debugger)))
    (let ((point (current-point)))
      (dolist (c condition)
        (insert-string point
                       (if (stringp c)
                           c
                           (prin1-to-string c)))
        (insert-character point #\newline 1))
      (insert-string point (format nil "~%Restarts:~%"))
      (loop :for n :from 0
            :for (title description) :in restarts
            :do (insert-string point (format nil "~D: [~A] ~A~%" n title description)))
      (insert-string point (format nil "~%Backtrace:~%"))
      (loop :for (n form) :in frames
            :do (insert-string point (format nil "~D: ~A~%" n form))))))

(defun active-debugger (thread level select)
  (declare (ignore select))
  (let ((buffer (get-debug-buffer thread)))
    (cond ((and buffer
                (= level (buffer-value buffer 'level -1)))
           ;(when select (pop-to-buffer buffer))
           )
          (t
           (debugger-reinitialize thread level)))))

(defun debugger-reinitialize (thread level)
  (swank-protocol:emacs-rex
   *connection*
   (swank:debugger-info-for-emacs 0 10)
   :continuation (lambda (value)
                   (alexandria:destructuring-case value
                     ((:ok result)
                      (apply #'start-debugger thread level result))))
   :thread thread))


(defvar *process* nil)
(defparameter *impl-name* nil)

(define-command slime () ()
  (setf *process*
        (sb-ext:run-program "ros"
                            `(,@(if *impl-name* `("-L" ,*impl-name*))
                              "-s" "swank"
                              "-e" ,(format nil "(swank:create-server :port ~D :dont-close t)"
                                            *default-port*)
                              "wait")
                            :wait nil
                            :search t))
  (sleep 1)
  (slime-connect "localhost" *default-port*)
  (add-hook *exit-editor-hook*
            (lambda ()
              (slime-quit))))

(define-command slime-quit () ()
  (when (and *process* (sb-ext:process-alive-p *process*))
    (sb-ext:process-kill *process* 9)))

(define-command slime-restart () ()
  (slime-quit)
  (sleep 1)
  (slime))


(defun idle-timer-function ()
  (when (and (eq (buffer-major-mode (current-buffer)) 'slime-mode)
             (connected-p))
    (let ((package (lem.lisp-mode::scan-current-package #'identity)))
      (when package
        (slime-set-package package)))))

(defvar *idle-timer*)
(when (or (not (boundp '*idle-timer*))
          (not (timer-alive-p *idle-timer*)))
  (setf *idle-timer*
        (start-idle-timer "slime" 110 t 'idle-timer-function nil
                          (lambda (condition)
                            (stop-timer *idle-timer*)
                            (pop-up-backtrace condition)))))

(pushnew (cons ".lisp$" 'slime-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons ".asd$" 'slime-mode) *auto-mode-alist* :test #'equal)
