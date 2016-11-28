(in-package :cl-user)
(defpackage :lem-slime
  (:use :cl :lem)
  (:local-nicknames
   (:lime :lem-slime.lime)
   (:swank-protocol :lem-slime.swank-protocol)))
(in-package :lem-slime)

(defvar *slime-prompt-string*)
(defvar *connection* nil)
(defvar *eval-timer* nil)
(defvar *write-string-function* 'write-string-to-output-buffer)
(defvar *last-compilation-result* nil)
(defvar *indent-table* (make-hash-table :test 'equal))

(defvar *note-attribute* (make-attribute "red" nil :underline-p t))
(defvar *entry-attribute* (make-attribute "blue" nil :bold-p t))
(defvar *headline-attribute* (make-attribute nil nil :bold-p t))

(define-major-mode slime-mode lem.prog-mode:prog-mode
    (:name "slime"
     :keymap *slime-mode-keymap*
     :syntax-table lem.lisp-mode:*lisp-syntax-table*)
  (modeline-add-status-list (lambda (window)
                              (buffer-package (window-buffer window) "CL-USER"))
                            (current-buffer))
  (setf (get-bvar :enable-syntax-highlight) t)
  (setf (get-bvar :indent-tabs-mode) nil)
  (setf (get-bvar :calc-indent-function)
        'calc-indent)
  (setf (get-bvar :beginning-of-defun-function)
        'lem.lisp-mode:lisp-beginning-of-defun)
  (setf (get-bvar :end-of-defun-function)
        'lem.lisp-mode:lisp-end-of-defun))

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
(define-key *slime-mode-keymap* "C-M-i" 'slime-complete-symbol)
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

(defun indentation-update (info)
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

(defun calc-indent ()
  (let ((lem.lisp-mode:*indent-spec-function* #'indent-spec))
    (lem.lisp-mode::lisp-calc-indent)))

(defun point-to-offset (point)
  (save-excursion
    (point-set (point-min))
    (let ((end-linum (point-linum point))
          (end-charpos (point-charpos point))
          (offset 0))
      (loop :repeat (1- end-linum)
            :for linum :from 1
            :do (incf offset
                      (1+ (buffer-line-length (current-buffer)
                                              linum))))
      (+ offset end-charpos))))

(defun get-buffer-from-file (file)
  (dolist (buffer (buffer-list))
    (when (uiop:pathname-equal file (buffer-filename buffer))
      (return buffer))))

(macrolet ((m (emacs-rex x)
             `(let (result-value)
                (,emacs-rex
                 *connection*
                 ,x
                 :continuation (lambda (result)
                                 (alexandria:destructuring-case result
                                   ((:ok value)
                                    (setq result-value value))
                                   ((:abort condition)
                                    (declare (ignore condition))
                                    (editor-error "Synchronous Lisp Evaluation aborted"))))
                 :package package
                 :thread t)
                (cond ((swank-protocol:message-waiting-p *connection* :timeout 2)
                       (pull-events)
                       result-value)
                      (t
                       (editor-error "timeout"))))))
  (defun slime-eval-string-internal (string
                                     &optional (package
                                                (swank-protocol:connection-package
                                                 *connection*)))
    (m swank-protocol:emacs-rex-string string))
  (defun slime-eval-internal (sexp &optional (package (current-package)))
    (m swank-protocol:emacs-rex sexp)))

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
   :package (current-package))
  (start-eval-timer))

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
                               (slime-eval-internal
                                '(swank:list-all-package-names t)))))
    (string-upcase (minibuf-read-line
                    "Package: " ""
                    (lambda (str)
                      (completion str package-names))
                    (lambda (str)
                      (find str package-names :test #'string=))))))

(define-command slime-indent-sexp () ()
  (lem.prog-mode:indent-region (current-point)
                               (save-excursion
                                 (forward-sexp 1)
                                 (current-point))))

(define-command slime-set-package (package-name) ((list (read-package-name)))
  (check-connection)
  (cond ((string= package-name ""))
        ((eq (current-buffer) (repl-buffer))
         (destructuring-bind (name prompt-string)
             (slime-eval-internal `(swank:set-package ,package-name))
           (new-package name prompt-string)
           (lem.listener-mode:listener-reset-prompt (repl-buffer))))
        (t
         (setf (get-bvar :file-property-list)
               (acons "package" package-name
                      (remove "package" (get-bvar :file-property-list)
                              :test #'equal
                              :key #'car))))))

(define-command slime-eval-last-expression () ()
  (check-connection)
  (refresh-output-buffer)
  (save-excursion
    (interactive-eval (region-string (current-point)
                                     (progn (backward-sexp 1 t)
                                            (current-point))))))

(define-command slime-eval-defun () ()
  (check-connection)
  (save-excursion
    (top-of-defun)
    (let ((string
           (region-string (current-point)
                          (progn (forward-sexp 1)
                                 (current-point)))))
      (refresh-output-buffer)
      (if (ppcre:scan "^\\(defvar\\b" string)
          (re-eval-defvar string)
          (interactive-eval string)))))

(define-command slime-eval-region (start end) ("r")
  (check-connection)
  (refresh-output-buffer)
  (interactive-eval (region-string start end)))

(define-command slime-load-file (filename) ("fLoad File: ")
  (check-connection)
  (when (uiop:pathname-equal filename (buffer-directory))
    (setf filename (buffer-filename (current-buffer))))
  (when (and (cl-fad:file-exists-p filename)
             (not (cl-fad:directory-pathname-p filename)))
    (refresh-output-buffer)
    (eval-with-transcript `(swank:load-file ,filename))))

(defun get-operator-name ()
  (save-excursion
    (lem.lisp-mode::go-to-car)
    (symbol-string-at-point)))

(define-command slime-echo-arglist () ()
  (check-connection)
  (unless (eval-timer-alive-p)
    (let ((name (get-operator-name))
          (package (current-package)))
      (when name
        (eval-async `(swank:operator-arglist ,name ,package)
                    (lambda (arglist)
                      (stop-eval-timer)
                      (when arglist
                        (message "~A" arglist)))
                    t)
        (start-eval-timer)))))

(define-command slime-space (n) ("p")
  (insert-char #\space n)
  (slime-echo-arglist))

(defun check-parens ()
  (save-excursion
    (point-set (point-min))
    (loop :while (forward-sexp 1 t))
    (skip-whitespace-forward)
    (eobp)))

(defun compilation-finished (result)
  (stop-eval-timer)
  (setf *last-compilation-result* result)
  (if (not (check-parens))
      (message "unmatched paren")
      (destructuring-bind (notes successp duration loadp fastfile)
          (rest result)
        (show-compile-result notes duration
                             (if (not loadp)
                                 successp
                                 (and fastfile successp)))
        (highlight-notes notes)
        (when (and loadp fastfile successp)
          (eval-async `(swank:load-file ,fastfile) nil t)))))

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
  (save-excursion
    (set-buffer buffer nil)
    (goto-position pos)
    (skip-chars-backward #'syntax-symbol-char-p)
    (let ((start (current-point)))
      (forward-sexp 1 t)
      (make-overlay start (current-point) *note-attribute*))))

(defun append-note-entry (grep name pos message source-context jump-fun)
  (lem.grep:call-with-writer
   grep
   (lambda ()
     (insert-string-with-attribute name lem.grep::*attribute-1*)
     (insert-string ":")
     (insert-string-with-attribute (princ-to-string pos) lem.grep::*attribute-2*)
     (insert-string ":")
     (lem.grep:put-entry-property grep
                                  (beginning-of-line-point)
                                  (end-of-line-point)
                                  jump-fun)
     (insert-newline 1)
     (insert-string message)
     (insert-newline 1)
     (insert-string source-context))))

(defvar *note-overlays* nil)

(defun highlight-notes (notes)
  (slime-remove-notes)
  (let ((overlays '())
        (grep (lem.grep:make-grep "*slime-compilations*")))
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
                                   (goto-position pos))))
                             (lambda ()
                               (find-file file)
                               (goto-position pos)))))
           (append-note-entry grep
                              (or buffer-name file)
                              pos
                              message
                              source-context
                              jump-fun)
           (push (make-highlight-overlay pos
                                         (if buffer-name
                                             (get-buffer buffer-name)
                                             (get-buffer-from-file file)))
                 overlays)))))
    (when overlays
      (lem.grep:update grep)
      (setf *note-overlays* overlays))))

(define-command slime-remove-notes () ()
  (mapc #'delete-overlay *note-overlays*))

(define-command slime-compile-and-load-file () ()
  (check-connection)
  (when (buffer-modified-p (current-buffer))
    (when (minibuf-y-or-n-p "Save file")
      (save-file)))
  (let ((file (buffer-filename (current-buffer))))
    (refresh-output-buffer)
    (eval-async `(swank:compile-file-for-emacs ,file t)
                #'compilation-finished
                t
                (buffer-package (current-buffer)))
    (start-eval-timer)))

(define-command slime-compile-region (start end) ("r")
  (check-connection)
  (let ((string (region-string start end))
        (position `((:position ,(point-to-offset start))
                    (:line ,(current-linum) ,(current-charpos)))))
    (refresh-output-buffer)
    (eval-async `(swank:compile-string-for-emacs ,string
                                                 ,(buffer-name (current-buffer))
                                                 ',position
                                                 ,(buffer-filename (current-buffer))
                                                 nil)
                #'compilation-finished
                t
                (buffer-package (current-buffer)))
    (start-eval-timer)))

(define-command slime-compile-defun () ()
  (check-connection)
  (save-excursion
    (let* ((start (progn (top-of-defun) (current-point)))
           (end (progn (forward-sexp 1) (current-point))))
      (slime-compile-region start end))))

(defun form-string-at-point ()
  (region-string (save-excursion
                   (skip-chars-backward #'syntax-symbol-char-p)
                   (current-point))
                 (save-excursion
                   (forward-sexp 1)
                   (current-point))))

(defun macroexpand-internal (expander buffer-name)
  (let ((string (slime-eval-internal `(,expander ,(form-string-at-point)))))
    (info-popup (get-buffer-create buffer-name)
                (lambda (out) (princ string out))
                t)))

(define-command slime-macroexpand () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-1 "*slime-macroexpand*"))

(define-command slime-macroexpand-all () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-all "*slime-macroexpand-all*"))

(defun symbol-completion (str)
  (let ((result (slime-eval-string-internal
                 (format nil "(swank:fuzzy-completions ~S ~S)"
                         str
                         (current-package)))))
    (when result
      (destructuring-bind (completions timeout-p) result
        (declare (ignore timeout-p))
        (completion-hypheen str (mapcar #'first completions))))))

(defun read-symbol-name (prompt &optional (initial ""))
  (minibuf-read-line prompt initial #'symbol-completion nil))

(defvar *edit-definition-stack* nil)

(define-command slime-edit-definition () ()
  (check-connection)
  (let* ((name (read-symbol-name "Edit Definition of: "
                                 (or (symbol-string-at-point) "")))
         (definitions (slime-eval-internal `(swank:find-definitions-for-emacs ,name)))
         (grep (lem.grep:make-grep "*slime-definitions*"))
         (prev-file nil))
    (dolist (def definitions)
      (optima:match def
        ((list title
               (list :location
                     (list :file file)
                     (list :position offset)
                     (list :snippet _)))
         (lem.grep:call-with-writer
          grep
          (lambda ()
            (unless (and prev-file (string= prev-file file))
              (insert-string-with-attribute file
                                            *headline-attribute*)
              (insert-newline 1))
            (let ((start (current-point)))
              (insert-string-with-attribute (format nil "  ~A" title)
                                            *entry-attribute*)
              (lem.grep:put-entry-property grep
                                           start
                                           (end-of-line-point)
                                           (lambda ()
                                             (find-file file)
                                             (goto-position offset))))
            (insert-newline 1)))
         (setf prev-file file))))
    (when prev-file
      (lem.grep:update grep)
      (push (list (current-buffer) (current-point))
            *edit-definition-stack*))))

(define-command slime-pop-find-definition-stack () ()
  (let ((elt (pop *edit-definition-stack*)))
    (when elt
      (destructuring-bind (buffer point) elt
        (select-buffer buffer)
        (point-set point)))))

(define-command slime-edit-uses () ()
  (check-connection)
  (let* ((symbol (read-symbol-name "Edit uses of: " (or (symbol-string-at-point) "")))
         (result (slime-eval-internal `(swank:xrefs '(:calls :macroexpands :binds
                                                      :references :sets :specializes)
                                                    ,symbol)))
         (grep (lem.grep:make-grep "*slime-xrefs*")))
    (loop :for (type . definitions) :in result
          :do (lem.grep:call-with-writer
               grep
               (lambda ()
                 (insert-string-with-attribute (princ-to-string type) *headline-attribute*)
                 (insert-newline 1)))
          :do (dolist (def definitions)
                (optima:match def
                  ((list name
                         (list :location
                               (list :file file)
                               (list :position offset)
                               (list :snippet _)))
                   (lem.grep:call-with-writer
                    grep
                    (lambda ()
                      (insert-string-with-attribute (format nil "  ~A" name)
                                                    *entry-attribute*)
                      (lem.grep:put-entry-property grep
                                                   (beginning-of-line-point)
                                                   (end-of-line-point)
                                                   (lambda ()
                                                     (find-file file)
                                                     (goto-position offset)))
                      (insert-newline 1)))))))
    (lem.grep:update grep)
    (push (list (current-buffer) (current-point))
          *edit-definition-stack*)))

(define-command slime-complete-symbol () ()
  (check-connection)
  (start-completion #'symbol-completion
                    (lem.lisp-mode::lisp-preceding-symbol))
  t)

(defvar *slime-apropos-mode-keymap* (make-keymap nil *slime-mode-keymap*))
(define-key *slime-apropos-mode-keymap* "q" 'quit-window)

(define-major-mode slime-apropos-mode ()
    (:name "slime-apropos"
     :keymap *slime-apropos-mode-keymap*))

(defun show-apropos (data)
  (let ((buffer (get-buffer-create "*slime-apropos*")))
    (setf (current-window) (display-buffer buffer))
    (slime-apropos-mode)
    (buffer-erase buffer)
    (loop :for plist :in data
          :do (let ((designator (cadr plist))
                    (plist1 (cddr plist)))
                (insert-string-with-attribute designator *headline-attribute*)
                (loop :for (k v) :on plist1 :by #'cddr
                      :do (insert-string (format nil "~%  ~A: ~A" k v)))
                (insert-newline 2)))))

(defun slime-apropos-internal (string only-external-p package case-sensitive-p)
  (show-apropos (slime-eval-internal
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
        (setq string (minibuf-read-string "SLIME Apropos: ")
              only-external-p (minibuf-y-or-n-p "External symbols only? ")
              package (let ((name (read-package-name)))
                        (if (string= "" name)
                            nil
                            name))
              case-sensitive-p (minibuf-y-or-n-p "Case-sensitive? "))
        (setq string (minibuf-read-string "SLIME Apropos: ")))
    (slime-apropos-internal string only-external-p package case-sensitive-p)))

(define-command slime-apropos-all () ()
  (check-connection)
  (slime-apropos-internal (minibuf-read-string "SLIME Apropos: ")
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
    (info-popup buffer
                (lambda (stream)
                  (princ string stream))
                nil)))

(define-command slime-describe-symbol () ()
  (check-connection)
  (let ((symbol-name (read-symbol-name "Describe symbol: ")))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (show-description (slime-eval-internal `(swank:describe-symbol ,symbol-name)))))

(define-command slime-documentation-lookup () ()
  (check-connection)
  (let ((symbol-name (read-symbol-name "Describe symbol: ")))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (lem-slime.clhs:main symbol-name)))

(define-major-mode slime-repl-mode slime-mode
    (:name "slime-repl"
     :keymap *slime-repl-mode-keymap*
     :syntax-table lem.lisp-mode:*lisp-syntax-table*)
  (repl-reset-input)
  (lem.listener-mode:listener-mode t))

(defvar *read-string-thread-stack* nil)
(defvar *read-string-tag-stack* nil)

(define-key *slime-repl-mode-keymap* "C-c C-c" 'slime-repl-interrupt)

(define-command slime-repl-interrupt () ()
  (when (eval-timer-alive-p)
    (swank-protocol:send-message-string *connection*
                                        (format nil "(:emacs-interrupt ~(~S~))"
                                                (or (car *read-string-thread-stack*)
                                                    :repl-thread)))))

(defun repl-buffer (&optional force)
  (if force
      (get-buffer-create "*slime-repl*")
      (get-buffer "*slime-repl*")))

(defun repl-get-prompt ()
  (format nil "~A> " *slime-prompt-string*))

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (get-bvar :listener-get-prompt-function :buffer buffer)
            'repl-get-prompt
            (get-bvar :listener-check-confirm-function :buffer buffer)
            'repl-paren-correspond-p
            (get-bvar :listener-confirm-function :buffer buffer)
            'repl-confirm))))

(defun repl-paren-correspond-p ()
  (lem.lisp-mode:lisp-repl-paren-correspond-p))

(defun repl-change-read-line-input ()
  (setf (get-bvar :listener-get-prompt-function)
        (constantly "")
        (get-bvar :listener-check-confirm-function)
        (constantly t)
        (get-bvar :listener-confirm-function)
        'repl-read-line))

(defun eval-timer-alive-p ()
  (and (timer-p *eval-timer*)
       (timer-alive-p *eval-timer*)))

(defvar *modeline-eval-flag* "During-Evalation")

(defun start-eval-timer ()
  (unless (eval-timer-alive-p)
    (modeline-add-status-list *modeline-eval-flag*)
    (setf *eval-timer*
          (start-timer nil 20 t
                       (lambda ()
                         (setf (timer-ms *eval-timer*) 100)
                         (pull-events))
                       nil
                       (lambda (condition)
                         (popup-backtrace condition)
                         (stop-timer *eval-timer*))))))

(defun stop-eval-timer ()
  (when (eval-timer-alive-p)
    (modeline-remove-status-list *modeline-eval-flag*)
    (stop-timer *eval-timer*)))

(defun repl-confirm (string)
  (check-connection)
  (let ((prev-write-string-function *write-string-function*))
    (swank-protocol:request-listener-eval
     *connection*
     string
     (lambda (value)
       (declare (ignore value))
       (stop-eval-timer)
       (repl-reset-input)
       (lem.listener-mode:listener-reset-prompt (repl-buffer))
       (redraw-display)
       (setf *write-string-function* prev-write-string-function)))
    (start-eval-timer)
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
      (point-set (point-max))
      (lem.listener-mode::listener-update-marker)
      (repl-change-read-line-input))))

(defun repl-abort-read (thread tag)
  (declare (ignore thread tag))
  (pop *read-string-thread-stack*)
  (pop *read-string-tag-stack*)
  (message "Read aborted"))

(defun repl-read-line (string)
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
      (with-open-stream (stream (make-buffer-output-stream buffer
                                                           (point-max buffer)))
        (princ string stream))
      (lem.listener-mode::listener-update-marker (point-max buffer))
      (when (eq buffer (current-buffer))
        (point-set (point-max)))
      (redraw-display))))

(defparameter *fresh-output-buffer-p* t)

(defun write-string-to-output-buffer (string)
  (let ((buffer (get-buffer-create "*slime-output*")))
    (set-buffer-mode buffer (lambda () (info-mode t)))
    (with-open-stream (stream (make-buffer-output-stream buffer
                                                         (point-max buffer)
                                                         t))
      (when *fresh-output-buffer-p*
        (setq *fresh-output-buffer-p* nil)
        (fresh-line stream)
        (terpri stream)
        (princ '*** stream)
        (terpri stream))
      (princ string stream))
    (pop-to-buffer buffer)))

(defun refresh-output-buffer ()
  (setq *fresh-output-buffer-p* t))

(define-command slime-connect (hostname port)
    ((list (minibuf-read-string "Hostname: " "localhost")
           (parse-integer (minibuf-read-string "Port: " "10000"))))
  (setf *connection* (lime:make-connection hostname port))
  (message "Connecting...")
  (handler-case (lime:connect *connection*)
    (usocket:connection-refused-error (c) (editor-error "~A" c)))
  (message "Swank server running on ~A ~A"
           (lime:connection-implementation-name *connection*)
           (lime:connection-implementation-version *connection*))
  (setf *slime-prompt-string*
        (getf (getf (getf (getf (lime::connection-info *connection*)
                                :return)
                          :ok)
                    :package)
              :prompt))
  (slime-repl))

(defun log-message (message)
  (let ((buffer (get-buffer-create "*slime-events*")))
    (with-open-stream (stream (make-buffer-output-stream buffer
                                                         (point-max buffer)))
      (print message stream))))

(defvar *unknown-keywords* nil)
(defun pull-events ()
  (when (and (boundp '*connection*)
             (not (null *connection*)))
    (let (messages)
      (handler-case (setq messages (lime:pull-all-events *connection*))
        (swank-protocol:disconnected
         (c)
         (declare (ignore c))
         (stop-eval-timer)
         (setq *connection* nil)))
      (loop :for message :in messages
            :do (log-message message)
            :do (alexandria:destructuring-case message
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
                   (active-debugger thread level select))
                  ((:debug thread level condition restarts frames conts)
                   (stop-eval-timer)
                   (start-debugger thread level condition restarts frames conts))
                  ((:debug-return thread level stepping)
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
                  ;; ((:new-features features)
                  ;;  )
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
                   (pushnew (car message) *unknown-keywords*)))))))

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
  (when (lime:debuggerp *connection*)
    (swank-protocol:emacs-rex *connection* `(swank:throw-to-toplevel))
    (start-eval-timer)))

(define-command slime-continue () ()
  (when (null (get-bvar 'restarts))
    (error "slime-continue called outside of debug buffer"))
  (swank-protocol:emacs-rex *connection*
                            '(swank:sldb-continue)
                            :continuation (lambda (value)
                                            (alexandria:destructuring-case value
                                              ((:ok x)
                                               (error "sldb-quit returned [~A]" x)))))
  (start-eval-timer))

(define-command slime-abort () ()
  (when (lime:debuggerp *connection*)
    (eval-async '(swank:sldb-abort)
                (lambda (v)
                  (message "Restart returned: ~A" v)))
    (start-eval-timer)))

(defun slime-invoke-restart (n)
  (check-type n integer)
  (when (lime:debuggerp *connection*)
    (swank-protocol:emacs-rex *connection*
                              `(swank:invoke-nth-restart-for-emacs
                                ,(get-bvar 'level :default -1)
                                ,n))
    (start-eval-timer)))

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
    (when (eql thread (get-bvar 'thread :buffer buffer))
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
    (setf (get-bvar 'thread :buffer buffer)
          thread
          (get-bvar 'level :buffer buffer)
          level
          (get-bvar 'condition :buffer buffer)
          condition
          (get-bvar 'restarts :buffer buffer)
          restarts
          (get-bvar 'continuations :buffer buffer)
          conts)
    (buffer-erase buffer)
    (buffer-add-delete-hook buffer 'slime-quit-debugger)
    (dolist (str condition)
      (insert-string str)
      (insert-newline 1))
    (insert-string (format nil "~%Restarts:~%"))
    (loop :for n :from 0
          :for (title description) :in restarts
          :do (insert-string (format nil "~D: [~A] ~A~%" n title description)))
    (insert-string (format nil "~%Backtrace:~%"))
    (loop :for (n form) :in frames
          :do (insert-string (format nil "~D: ~A~%" n form)))))

(defun active-debugger (thread level select)
  (declare (ignore select))
  (let ((buffer (get-debug-buffer thread)))
    (cond ((and buffer
                (= level (get-bvar 'level :buffer buffer :default -1)))
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


(defun idle-timer-function ()
  (when (and (eq (major-mode) 'slime-mode)
             (connected-p))
    (let ((package (lem.lisp-mode::scan-current-package #'identity)))
      (when package
        (slime-set-package package)))))

(defvar *idle-timer*)
(when (or (not (boundp '*idle-timer*))
          (not (timer-alive-p *idle-timer*)))
  (setf *idle-timer*
        (start-idle-timer "slime" 200 t 'idle-timer-function nil
                          (lambda (condition)
                            (popup-backtrace condition)
                            (stop-timer *idle-timer*)))))

(setq *auto-mode-alist*
      (append '((".lisp$" . slime-mode)
                (".asd$" . slime-mode))
              *auto-mode-alist*))
