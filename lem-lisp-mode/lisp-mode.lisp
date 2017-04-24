(in-package :lem-lisp-mode)

(defparameter *default-port* 4005)

(defvar *connection-list* '())
(defvar *connection* nil)
(defvar *event-hooks* '())
(defvar *write-string-function* 'write-string-to-output-buffer)
(defvar *last-compilation-result* nil)
(defvar *indent-table* (make-hash-table :test 'equal))

(define-attribute compiler-note-attribute
  (t :foreground "red" :underline-p t))

(define-attribute apropos-headline-attribute
  (t :bold-p t))

(define-major-mode lisp-mode language-mode
    (:name "lisp"
     :keymap *lisp-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*)
  (modeline-add-status-list (lambda (window)
                              (buffer-package (window-buffer window) "CL-USER"))
                            (current-buffer))
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'calc-indent)
  (setf (variable-value 'line-comment) ";")
  (setf (variable-value 'insertion-line-comment) ";; ")
  (setf (variable-value 'find-definitions-function) 'find-definitions)
  (setf (variable-value 'find-references-function) 'find-references)
  (setf (variable-value 'completion-function) 'completion-symbol)
  (unless (connected-p) (self-connect)))

(define-key *lisp-mode-keymap* "C-M-a" 'lisp-beginning-of-defun)
(define-key *lisp-mode-keymap* "C-M-e" 'lisp-end-of-defun)
(define-key *lisp-mode-keymap* "C-M-q" 'lisp-indent-sexp)
(define-key *lisp-mode-keymap* "C-c M-p" 'lisp-set-package)
(define-key *global-keymap* "M-:" 'lisp-eval-string)
(define-key *lisp-mode-keymap* "C-c C-e" 'lisp-eval-last-expression)
(define-key *lisp-mode-keymap* "C-M-x" 'lisp-eval-defun)
(define-key *lisp-mode-keymap* "C-c C-r" 'lisp-eval-region)
(define-key *lisp-mode-keymap* "C-c C-l" 'lisp-load-file)
(define-key *lisp-mode-keymap* "Spc" 'lisp-space)
(define-key *lisp-mode-keymap* "C-c M-c" 'lisp-remove-notes)
(define-key *lisp-mode-keymap* "C-c C-k" 'lisp-compile-and-load-file)
(define-key *lisp-mode-keymap* "C-c C-c" 'lisp-compile-defun)
(define-key *lisp-mode-keymap* "C-c C-m" 'lisp-macroexpand)
(define-key *lisp-mode-keymap* "C-c M-m" 'lisp-macroexpand-all)
(define-key *lisp-mode-keymap* "C-c C-d C-a" 'lisp-echo-arglist)
(define-key *lisp-mode-keymap* "C-c C-d a" 'lisp-apropos)
(define-key *lisp-mode-keymap* "C-c C-d z" 'lisp-apropos-all)
(define-key *lisp-mode-keymap* "C-c C-d p" 'lisp-apropos-package)
(define-key *lisp-mode-keymap* "C-c C-d d" 'lisp-describe-symbol)
(define-key *lisp-mode-keymap* "C-c C-z" 'lisp-switch-to-repl-buffer)
(define-key *lisp-mode-keymap* "C-c z" 'lisp-switch-to-repl-buffer)

(defun connected-p ()
  (not (null *connection*)))

(defun add-connection (conn)
  (push conn *connection-list*)
  (setf *connection* conn))

(defun remove-connection (conn)
  (setf *connection-list* (delete conn *connection-list*))
  (setf *connection* (car *connection-list*))
  *connection*)

(define-command lisp-connection-list () ()
  (let ((menu (make-instance 'lem.menu-mode:menu
                             :buffer-name "*lisp-connections*"
                             :columns '(" " "hostname" "port" "pid" "name" "version"))))
    (dolist (c *connection-list*)
      (let ((item (make-instance 'lem.menu-mode:menu-item
                                 :select-function (let ((c c))
                                                    (lambda ()
                                                      (setf *connection* c)
                                                      (lisp-connection-list))))))
        (lem.menu-mode:append-menu-item item (if (eq c *connection*) "*" " "))
        (lem.menu-mode:append-menu-item item (swank-protocol:connection-hostname c))
        (lem.menu-mode:append-menu-item item (swank-protocol:connection-port c))
        (lem.menu-mode:append-menu-item item (swank-protocol:connection-pid c))
        (lem.menu-mode:append-menu-item item (swank-protocol:connection-implementation-name c))
        (lem.menu-mode:append-menu-item item (swank-protocol:connection-implementation-version c))
        (lem.menu-mode:append-menu menu item)))
    (lem.menu-mode:display-menu menu)))

(defun self-connect ()
  (prog (port)
   :START
    (setf port (+ 10000 (random 10000)))
    (handler-case (let ((swank::*swank-debug-p* nil))
                    (swank:create-server :port port))
      (error ()
        (go :START)))
    (slime-connect "localhost" port nil)
    (update-buffer-package)))

(defun check-connection ()
  (unless (connected-p)
    (self-connect)))

(defun buffer-package (buffer &optional default)
  (let ((package-name (buffer-value buffer "package")))
    (if package-name
        (string-upcase package-name)
        default)))

(defvar *current-package* nil)

(defun current-package ()
  (or *current-package*
      (buffer-package (current-buffer))
      (swank-protocol:connection-package *connection*)))

(defun current-swank-thread ()
  (or (buffer-value (current-buffer) 'thread)
      t))

(defun features ()
  (when (connected-p)
    (swank-protocol:connection-features *connection*)))

(defun indentation-update (info)
  (loop :for (name indent packages) :in info :do
    (lem-lisp-syntax:set-indentation name indent))
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
  (let ((lem-lisp-syntax:*get-method-function* #'indent-spec))
    (lem-lisp-syntax:calc-indent point)))

(defun lisp-rex (form &key
                      continuation
                      (thread (current-swank-thread))
                      (package (current-package)))
  (swank-protocol:emacs-rex *connection*
                            form
                            :continuation continuation
                            :thread thread
                            :package package))

(defun lisp-eval-internal (emacs-rex-fun rex-arg package)
  (let ((tag (gensym)))
    (catch tag
      (funcall emacs-rex-fun
               *connection*
               rex-arg
               :continuation (lambda (result)
                               (alexandria:destructuring-ecase result
                                 ((:ok value)
                                  (throw tag value))
                                 ((:abort condition)
                                  (declare (ignore condition))
                                  (editor-error "Synchronous Lisp Evaluation aborted"))))
               :package package
               :thread (current-swank-thread))
      (loop (sit-for 10 nil)))))

(defun lisp-eval-from-string (string &optional (package (current-package)))
  (lisp-eval-internal 'swank-protocol:emacs-rex-string string package))

(defun lisp-eval (sexp &optional (package (current-package)))
  (lisp-eval-internal 'swank-protocol:emacs-rex sexp package))

(defun lisp-eval-async (form &optional cont (package (current-package)))
  (let ((buffer (current-buffer)))
    (lisp-rex form
              :continuation (lambda (value)
                              (alexandria:destructuring-ecase value
                                ((:ok result)
                                 (when cont
                                   (setf (current-buffer) buffer)
                                   (funcall cont result)))
                                ((:abort condition)
                                 (message "Evaluation aborted on ~A." condition))))
              :thread (current-swank-thread)
              :package package)))

(defun lisp-eval-describe (form)
  (lisp-eval-async form #'show-description))

(defun eval-with-transcript (form)
  (lisp-rex form
            :continuation (lambda (value)
                            (alexandria:destructuring-ecase value
                              ((:ok x)
                               (message "~A" x))
                              ((:abort condition)
                               (message "Evaluation aborted on ~A." condition))))
            :package (current-package)))

(defun re-eval-defvar (string)
  (eval-with-transcript `(swank:re-evaluate-defvar ,string)))

(defun interactive-eval (string)
  (eval-with-transcript `(swank:interactive-eval ,string)))

(defun new-package (name prompt-string)
  (setf (swank-protocol:connection-package *connection*) name)
  (setf (swank-protocol:connection-prompt-string *connection*) prompt-string)
  t)

(defun read-package-name ()
  (check-connection)
  (let ((package-names (mapcar #'string-downcase
                               (lisp-eval
                                '(swank:list-all-package-names t)))))
    (string-upcase (prompt-for-line
                    "Package: " ""
                    (lambda (str)
                      (completion str package-names))
                    (lambda (str)
                      (find str package-names :test #'string=))
                    'mh-lisp-package))))

(define-command lisp-beginning-of-defun (n) ("p")
  (lem-lisp-syntax:beginning-of-defun (current-point) (- n)))

(define-command lisp-end-of-defun (n) ("p")
  (if (minusp n)
      (lisp-beginning-of-defun (- n))
      (let ((point (current-point)))
        (dotimes (_ n)
          (with-point ((p point))
            (cond ((and (lem-lisp-syntax:beginning-of-defun p -1)
                        (point<= p point)
                        (or (form-offset p 1)
                            (progn
                              (move-point point p)
                              (return)))
                        (point< point p))
                   (move-point point p)
                   (skip-whitespace-forward point t)
                   (when (end-line-p point)
                     (character-offset point 1)))
                  (t
                   (form-offset point 1)
                   (skip-whitespace-forward point t)
                   (when (end-line-p point)
                     (character-offset point 1)))))))))

(define-command lisp-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-region (current-point) end))))

(define-command lisp-set-package (package-name) ((list (read-package-name)))
  (check-connection)
  (cond ((string= package-name ""))
        ((eq (current-buffer) (repl-buffer))
         (destructuring-bind (name prompt-string)
             (lisp-eval `(swank:set-package ,package-name))
           (new-package name prompt-string)
           (lem.listener-mode:listener-reset-prompt (repl-buffer))))
        (t
         (setf (buffer-value (current-buffer) "package") package-name))))

(defun prompt-for-sexp (string &optional initial)
  (prompt-for-line string
                   initial
                   (lambda (str)
                     (declare (ignore str))
                     (completion-symbol))
                   nil
                   'mh-sexp))

(define-command lisp-eval-string (string)
    ((list (prompt-for-sexp "Lisp Eval: ")))
  (check-connection)
  (interactive-eval string))

(define-command lisp-eval-last-expression () ()
  (check-connection)
  (refresh-output-buffer)
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (interactive-eval (points-to-string start end))))

(define-command lisp-eval-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem-lisp-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (let ((string (points-to-string start end)))
        (refresh-output-buffer)
        (if (ppcre:scan "^\\(defvar\\b" string)
            (re-eval-defvar string)
            (interactive-eval string))))))

(define-command lisp-eval-region (start end) ("r")
  (check-connection)
  (refresh-output-buffer)
  (interactive-eval (points-to-string start end)))

(define-command lisp-load-file (filename) ("fLoad File: ")
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

(define-command lisp-echo-arglist () ()
  (check-connection)
  (let ((name (get-operator-name))
        (package (current-package)))
    (when name
      (lisp-eval-async `(swank:operator-arglist ,name ,package)
                       (lambda (arglist)
                         (when arglist
                           (message "~A" (ppcre:regex-replace-all "\\s+" arglist " "))))))))

(define-command lisp-space (n) ("p")
  (insert-character (current-point) #\space n)
  (lisp-echo-arglist))

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
      (lisp-eval-async `(swank:load-file ,fastfile)))))

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
                  'compiler-note-attribute)))

(defvar *note-overlays* nil)

(defun highlight-notes (notes)
  (lisp-remove-notes)
  (when notes
    (let ((overlays '()))
      (lem.sourcelist:with-sourcelist (sourcelist "*lisp-compilations*")
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
                                                 (get-file-buffer file)))
                     overlays))))))
      (when overlays
        (setf *note-overlays* overlays)))))

(define-command lisp-remove-notes () ()
  (mapc #'delete-overlay *note-overlays*))

(define-command lisp-compile-and-load-file () ()
  (check-connection)
  (when (buffer-modified-p (current-buffer))
    (when (prompt-for-y-or-n-p "Save file")
      (save-buffer)))
  (let ((file (buffer-filename (current-buffer))))
    (refresh-output-buffer)
    (lisp-eval-async `(swank:compile-file-for-emacs ,file t)
                     #'compilation-finished)))

(define-command lisp-compile-region (start end) ("r")
  (check-connection)
  (let ((string (points-to-string start end))
        (position `((:position ,(position-at-point start))
                    (:line
                     ,(line-number-at-point (current-point))
                     ,(point-charpos (current-point))))))
    (refresh-output-buffer)
    (lisp-eval-async `(swank:compile-string-for-emacs ,string
                                                      ,(buffer-name (current-buffer))
                                                      ',position
                                                      ,(buffer-filename (current-buffer))
                                                      nil)
                     #'compilation-finished)))

(define-command lisp-compile-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem-lisp-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (form-offset end 1)
      (lisp-compile-region start end))))

(defun form-string-at-point ()
  (with-point ((point (current-point)))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (with-point ((start point)
                 (end point))
      (form-offset end 1)
      (points-to-string start end))))

(defun macroexpand-internal (expander buffer-name)
  (let ((string (lisp-eval `(,expander ,(form-string-at-point)))))
    (with-pop-up-typeout-window (out (get-buffer-create buffer-name) :focus t :erase t)
      (princ string out))))

(define-command lisp-macroexpand () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-1 "*lisp-macroexpand*"))

(define-command lisp-macroexpand-all () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-all "*lisp-macroexpand-all*"))

(defun symbol-completion (str &optional (package (current-package)))
  (let ((result (lisp-eval-from-string
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

(defun definition-to-location (definition)
  (optima:match definition
    ((list title
           (list :location
                 (list :file file)
                 (list :position position)
                 (list :snippet _)))
     (return-from definition-to-location
       (make-xref-location :title title
                           :file file
                           :position (1+ position))))
    ((list :location
           (list :file file)
           (list :position position)
           (list :snippet _))
     (return-from definition-to-location
       (make-xref-location :title ""
                           :file file
                           :position (1+ position))))))

(defun definitions-to-locations (definitions)
  (loop :for def :in definitions
        :for xref := (definition-to-location def)
        :when xref
        :collect xref))

(defun find-definitions ()
  (check-connection)
  (let ((name (or (symbol-string-at-point (current-point))
                  (read-symbol-name "Edit Definition of: "))))
    (let ((point (lem-lisp-syntax:search-local-definition (current-point) name)))
      (when point
        (return-from find-definitions
          (list (make-xref-location :file (buffer-filename (current-buffer))
                                    :position (position-at-point point))))))
    (let ((definitions (lisp-eval `(swank:find-definitions-for-emacs ,name))))
      (definitions-to-locations definitions))))

(defun find-references ()
  (check-connection)
  (let* ((name (or (symbol-string-at-point (current-point))
                   (read-symbol-name "Edit uses of: ")))
         (data (lisp-eval `(swank:xrefs '(:calls :macroexpands :binds
                                           :references :sets :specializes)
                                         ,name))))
    (loop
      :for (type . definitions) :in data
      :for defs := (definitions-to-locations definitions)
      :collect (make-xref-references :type type
                                     :locations defs))))

(defun completion-symbol ()
  (check-connection)
  (with-point ((start (current-point))
               (end (current-point)))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (when (point< start end)
      (let ((result
             (lisp-eval-from-string (format nil "(swank:fuzzy-completions ~S ~S)"
                                             (points-to-string start end)
                                             (current-package)))))
        (when result
          (destructuring-bind (completions timeout-p) result
            (declare (ignore timeout-p))
            (mapcar (lambda (completion)
                      (make-completion-item
                       :label (first completion)
                       :detail (fourth completion)
                       :start start
                       :end end))
                    completions)))))))

(defvar *lisp-apropos-mode-keymap* (make-keymap nil *lisp-mode-keymap*))
(define-key *lisp-apropos-mode-keymap* "q" 'quit-window)

(define-major-mode lisp-apropos-mode ()
    (:name "lisp-apropos"
     :keymap *lisp-apropos-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*))

(defun show-apropos (data)
  (let ((buffer (get-buffer-create "*lisp-apropos*")))
    (switch-to-buffer buffer)
    (lisp-apropos-mode)
    (erase-buffer buffer)
    (save-excursion
      (let ((point (current-point)))
        (loop :for plist :in data
              :do (let ((designator (cadr plist))
                        (plist1 (cddr plist)))
                    (insert-string point designator
                                   :attribute 'apropos-headline-attribute)
                    (loop :for (k v) :on plist1 :by #'cddr
                          :do (insert-string point (format nil "~%  ~A: ~A" k v)))
                    (insert-character point #\newline 2)))))))

(defun lisp-apropos-internal (string only-external-p package case-sensitive-p)
  (show-apropos (lisp-eval
                 `(swank:apropos-list-for-emacs ,string
                                                ,only-external-p
                                                ,case-sensitive-p
                                                ,package))))

(define-command lisp-apropos (&optional arg) ("P")
  (check-connection)
  (let ((string)
        (only-external-p t)
        (package nil)
        (case-sensitive-p nil))
    (if arg
        (setq string (prompt-for-string "lisp Apropos: ")
              only-external-p (prompt-for-y-or-n-p "External symbols only? ")
              package (let ((name (read-package-name)))
                        (if (string= "" name)
                            nil
                            name))
              case-sensitive-p (prompt-for-y-or-n-p "Case-sensitive? "))
        (setq string (prompt-for-string "lisp Apropos: ")))
    (lisp-apropos-internal string only-external-p package case-sensitive-p)))

(define-command lisp-apropos-all () ()
  (check-connection)
  (lisp-apropos-internal (prompt-for-string "lisp Apropos: ")
                          nil nil nil))

(define-command lisp-apropos-package (internal) ("P")
  (check-connection)
  (let ((package (read-package-name)))
    (lisp-apropos-internal ""
                            (not internal)
                            (if (string= package "")
                                (current-package)
                                package)
                            nil)))

(defun show-description (string)
  (let ((buffer (get-buffer-create "*lisp-description*")))
    (change-buffer-mode buffer 'lisp-mode t)
    (with-pop-up-typeout-window (stream buffer :erase t)
      (princ string stream))))

(define-command lisp-describe-symbol () ()
  (check-connection)
  (let ((symbol-name
         (read-symbol-name "Describe symbol: "
                           (or (symbol-string-at-point (current-point)) ""))))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (show-description (lisp-eval `(swank:describe-symbol ,symbol-name)))))

(define-major-mode lisp-repl-mode lisp-mode
    (:name "lisp-repl"
     :keymap *lisp-repl-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*)
  (repl-reset-input)
  (lem.listener-mode:listener-mode t)
  (setf *write-string-function* 'write-string-to-repl))

(defun read-string-thread-stack ()
  (buffer-value (repl-buffer) 'read-string-thread-stack))

(defun (setf read-string-thread-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-thread-stack) val))

(defun read-string-tag-stack ()
  (buffer-value (repl-buffer) 'read-string-tag-stack))

(defun (setf read-string-tag-stack) (val)
  (setf (buffer-value (repl-buffer) 'read-string-tag-stack) val))

(define-key *lisp-repl-mode-keymap* "C-c C-c" 'lisp-repl-interrupt)

(define-command lisp-repl-interrupt () ()
  (swank-protocol:send-message-string *connection*
                                      (format nil "(:emacs-interrupt ~(~S~))"
                                              (or (car (read-string-thread-stack))
                                                  :repl-thread))))

(defun repl-buffer ()
  (get-buffer "*lisp-repl*"))

(defun repl-get-prompt ()
  (format nil "~A> " (swank-protocol:connection-prompt-string *connection*)))

(defun repl-paren-correspond-p (point)
  (loop :with count := 0
        :do
        (insert-character point #\))
        (incf count)
        (unless (form-offset (copy-point point :temporary) -1)
          (delete-character point (- count))
          (return (= 1 count)))))

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (variable-value 'lem.listener-mode:listener-get-prompt-function :buffer buffer)
            'repl-get-prompt
            (variable-value 'lem.listener-mode:listener-check-confirm-function :buffer buffer)
            'repl-paren-correspond-p
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
  (swank-protocol:request-listener-eval
   *connection*
   string
   (lambda (value)
     (declare (ignore value))
     (lem.listener-mode:listener-reset-prompt (repl-buffer))
     (redraw-display))))

(defun repl-read-string (thread tag)
  (unless (repl-buffer) (start-lisp-repl))
  (let ((buffer (repl-buffer)))
    (push thread (read-string-thread-stack))
    (push tag (read-string-tag-stack))
    (let ((windows (get-buffer-windows buffer)))
      (setf (current-window)
            (if windows
                (first windows)
                (pop-to-buffer buffer))))
    (buffer-end (current-point))
    (lem.listener-mode:listener-update-point)
    (repl-change-read-line-input)))

(defun repl-pop-stack ()
  (let ((thread (pop (read-string-thread-stack)))
        (tag (pop (read-string-tag-stack))))
    (when (null (read-string-thread-stack))
      (repl-reset-input))
    (values thread tag)))

(defun repl-abort-read (thread tag)
  (declare (ignore thread tag))
  (repl-pop-stack)
  (message "Read aborted"))

(defun repl-read-line (point string)
  (declare (ignore point))
  (multiple-value-bind (thread tag) (repl-pop-stack)
    (dispatch-message (list :emacs-return-string
                            thread
                            tag
                            (concatenate 'string
                                         string
                                         (string #\newline))))))

(define-command start-lisp-repl () ()
  (check-connection)
  (lem.listener-mode:listener-start "*lisp-repl*" 'lisp-repl-mode))

(define-command lisp-switch-to-repl-buffer () ()
  (let ((buffer (repl-buffer)))
    (if buffer
        (setf (current-window)
              (or (car (get-buffer-windows buffer))
                  (pop-to-buffer buffer)))
        (start-lisp-repl))))

(defun write-string-to-repl (string)
  (let ((buffer (repl-buffer)))
    (when buffer
      (with-open-stream (stream (make-buffer-output-stream (buffer-end-point buffer)))
        (princ string stream))
      (lem.listener-mode:listener-update-point (buffer-end-point buffer))
      (when (eq buffer (current-buffer))
        (buffer-end (current-point)))
      (redraw-display))))

(defparameter *fresh-output-buffer-p* t)

(defun write-string-to-output-buffer (string)
  (with-pop-up-typeout-window (stream (get-buffer-create "*lisp-output*"))
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
                      (when (swank-protocol:message-waiting-p *connection* :timeout 10)
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
                  :name "lisp-wait-message"))

(define-command slime-connect (hostname port &optional (start-repl t))
    ((list (prompt-for-string "Hostname: " "localhost")
           (parse-integer (prompt-for-string "Port: " (princ-to-string *default-port*)))
           t))
  (message "Connecting...")
  (let (connection)
    (handler-case (setf connection
                        (swank-protocol:new-connection hostname port))
      (usocket:connection-refused-error (c)
        (editor-error "~A" c)))
    (message "Swank server running on ~A ~A"
             (swank-protocol:connection-implementation-name connection)
             (swank-protocol:connection-implementation-version connection))
    (setf lem-lisp-syntax:*get-features-function* 'features)
    (add-connection connection)
    (when start-repl (start-lisp-repl))
    (start-thread)
    connection))

(defun log-message (message)
  (let ((buffer (get-buffer-create "*lisp-events*")))
    (with-open-stream (stream (make-buffer-output-stream (buffer-end-point buffer)))
      (print message stream))))

(defvar *unknown-keywords* nil)
(defun pull-events ()
  (when (and (boundp '*connection*)
             (not (null *connection*)))
    (handler-bind ((swank-protocol:disconnected
                    (lambda (c)
                      (declare (ignore c))
                      (remove-connection *connection*))))
      (loop :while (swank-protocol:message-waiting-p *connection*)
            :do (dispatch-message (swank-protocol:read-message *connection*))))))

(defun dispatch-message (message)
  (log-message message)
  (dolist (e *event-hooks*)
    (when (funcall e message)
      (return-from dispatch-message)))
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
    ;; ((:channel-send id msg)
    ;;  )
    ;; ((:emacs-channel-send id msg)
    ;;  )
    ((:read-from-minibuffer thread tag prompt initial-value)
     (read-from-minibuffer thread tag prompt initial-value))
    ((:y-or-n-p thread tag question)
     (dispatch-message `(:emacs-return ,thread ,tag ,(prompt-for-y-or-n-p question))))
    ((:emacs-return-string thread tag string)
     (swank-protocol:send-message-string
      *connection*
      (format nil "(:emacs-return-string ~A ~A ~S)"
              thread
              tag
              string)))
    ((:new-features features)
     (setf (swank-protocol:connection-features *connection*)
           features))
    ((:indentation-update info)
     (indentation-update info))
    ;; ((:eval-no-wait form)
    ;;  )
    ;; ((:eval thread tag form-string)
    ;;  )
    ((:emacs-return thread tag value)
     (swank-protocol:send-message-string
      *connection*
      (format nil "(:emacs-return ~A ~A ~S)" thread tag value)))
    ;; ((:ed what)
    ;;  )
    ;; ((:inspect what thread tag)
    ;;  )
    ;; ((:background-message message)
    ;;  )
    ((:debug-condition thread message)
     (assert thread)
     (message "~A" message))
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

(defun read-from-minibuffer (thread tag prompt initial-value)
  (let ((input (prompt-for-sexp prompt initial-value)))
    (dispatch-message `(:emacs-return ,thread ,tag ,input))))

(defun show-source-location (source-location)
  (alexandria:destructuring-case source-location
    ((:error message)
     (message "~A" message))
    ((t &rest _)
     (declare (ignore _))
     (go-to-location (definition-to-location source-location) t))))


(defvar *process* nil)
(defparameter *impl-name* nil)

(define-command slime () ()
  (let ((process
         (sb-ext:run-program "ros"
                             `(,@(if *impl-name* `("-L" ,*impl-name*))
                               "-s" "swank"
                               "-e" ,(format nil "(swank:create-server :port ~D :dont-close t)"
                                             *default-port*)
                               "wait")
                             :wait nil
                             :search t)))
    (sleep 1)
    (let ((connection (slime-connect "localhost" *default-port*)))
      (setf (swank-protocol:connection-process connection)
            process)))
  (add-hook *exit-editor-hook*
            (lambda ()
              (ignore-errors
               (slime-quit)))))

(define-command slime-quit () ()
  (let ((process (swank-protocol:connection-process *connection*)))
    (when (and process (sb-ext:process-alive-p process))
      (sb-ext:process-kill process 9)
      (remove-connection *connection*)
      t)))

(define-command slime-restart () ()
  (slime-quit)
  (sleep 1)
  (slime))


(defun scan-current-package (point)
  (with-point ((p point))
    (loop
      (multiple-value-bind (result groups)
          (looking-at (line-start p)
                      "^\\s*\\(in-package (?:#?:|')?([^\)]*)\\)")
        (when result
          (let ((package (aref groups 0)))
            (when package
              (return package))))
        (unless (line-offset p -1)
          (return))))))

(defun update-buffer-package ()
  (when (and (eq (buffer-major-mode (current-buffer)) 'lisp-mode)
             (connected-p))
    (let ((package (scan-current-package (current-point))))
      (when package
        (lisp-set-package package)))))

(defvar *idle-timer*)
(when (or (not (boundp '*idle-timer*))
          (not (timer-alive-p *idle-timer*)))
  (setf *idle-timer*
        (start-idle-timer 110 t 'update-buffer-package
                          (lambda (condition)
                            (stop-timer *idle-timer*)
                            (pop-up-backtrace condition)))))

(pushnew (cons ".lisp$" 'lisp-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons ".asd$" 'lisp-mode) *auto-mode-alist* :test #'equal)
