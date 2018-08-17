(in-package :lem-lisp-mode)

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
(define-key *lisp-repl-mode-keymap* "," 'lisp-repl-shortcut)

(define-command lisp-repl-interrupt () ()
  (send-message-string *connection*
                       (format nil "(:emacs-interrupt ~(~S~))"
                               (or (car (read-string-thread-stack))
                                   :repl-thread))))

(defvar *lisp-repl-shortcuts* '())

(defun prompt-for-shortcuts ()
  (let* ((*lisp-repl-shortcuts* *lisp-repl-shortcuts*)
         (names (mapcar #'car *lisp-repl-shortcuts*)))
    (cdr (assoc (prompt-for-line
                 "Command:"
                 ""
                 (lambda (x) (completion-strings x names))
                 (lambda (name) (member name names :test #'string=))
                 'mh-lisp-repl-shortcuts)
                *lisp-repl-shortcuts* :test #'equal))))

(define-command lisp-repl-shortcut (n) ("p")
  (with-point ((point (current-point)))
    (if (point>= (lem.listener-mode::listener-start-point (current-buffer)) point)
        (let ((fun (prompt-for-shortcuts)))
          (when fun
            (funcall fun n)))
        (let ((c (insertion-key-p (last-read-key-sequence))))
          (insert-character point c n)))))

(defmacro define-repl-shortcut (name lambda-list &body body)
  (if (symbolp lambda-list)
      `(progn
         (setf *lisp-repl-shortcuts* (remove ,(string-downcase name) *lisp-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',lambda-list) *lisp-repl-shortcuts*)
         ',name)
      `(progn
         (setf *lisp-repl-shortcuts* (remove ,(string-downcase name) *lisp-repl-shortcuts* :key 'first :test 'equal))
         (push (cons ,(string-downcase name) ',name) *lisp-repl-shortcuts*)
         (defun ,name ,lambda-list ,@body))))

(defun repl-buffer ()
  (get-buffer "*lisp-repl*"))

(defun repl-get-prompt ()
  (format nil "~A> " (connection-prompt-string *connection*)))

(defun repl-paren-correspond-p (point)
  (unless (eq (repl-buffer) (point-buffer point))
    (return-from repl-paren-correspond-p))
  (with-point ((start (lem.listener-mode::listener-start-point (repl-buffer))))
    (let ((state (parse-partial-sexp start point)))
      (>= 0 (pps-state-paren-depth state)))))

(defun repl-reset-input ()
  (let ((buffer (repl-buffer)))
    (when buffer
      (setf (variable-value 'lem.listener-mode:listener-get-prompt-function :buffer buffer)
            'repl-get-prompt
            (variable-value 'lem.listener-mode:listener-check-input-function :buffer buffer)
            'repl-paren-correspond-p
            (variable-value 'lem.listener-mode:listener-execute-function :buffer buffer)
            'repl-eval))))

(defun repl-change-read-line-input ()
  (setf (variable-value 'lem.listener-mode:listener-get-prompt-function)
        (constantly "")
        (variable-value 'lem.listener-mode:listener-check-input-function)
        (constantly t)
        (variable-value 'lem.listener-mode:listener-execute-function)
        'repl-read-line))

(defun clear-repl ()
  (lem.listener-mode:clear-listener (repl-buffer)))

(defun get-repl-window ()
  (let* ((buffer (repl-buffer)))
    (if (eq buffer (window-buffer (current-window)))
        (current-window)
        (first (get-buffer-windows buffer)))))

(defun repl-buffer-width ()
  (alexandria:when-let* ((window (get-repl-window))
                         (width (- (window-width window) 2)))
    width))

(defun repl-highlight-notes (notes)
  (let ((buffer (repl-buffer)))
    (dolist (note notes)
      (optima:match note
        ((and (optima:property :location location)
              (optima:property :message _))
         (let* ((xref-loc (source-location-to-xref-location location))
                (offset (xref-location-position xref-loc)))
           (with-point ((start (buffer-point buffer)))
             (move-point start (lem.listener-mode::listener-start-point buffer))
             (form-offset start -1)
             (character-offset start (if (plusp offset) (1- offset) offset))
             (with-point ((end start))
               (form-offset end 1)
               (put-text-property start end :attribute 'compiler-note-attribute)))))))))

(defvar *repl-compiler-check* nil)

(defvar *repl-temporary-file*
  (merge-pathnames "slime-repl.tmp" (uiop:temporary-directory)))

(defun repl-eval (point string)
  (declare (ignore point))
  (check-connection)
  (cond
    (*repl-compiler-check*
     (with-open-file (stream *repl-temporary-file*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
       (write-string string stream))
     (let ((result
             (let ((*write-string-function* (constantly nil)))
               (lisp-eval `(swank:compile-file-for-emacs *repl-temporary-file* nil)))))
       (destructuring-bind (notes successp duration loadp fastfile)
           (cdr result)
         (declare (ignore successp duration loadp fastfile))
         (repl-highlight-notes notes)
         (listener-eval string))))
    (t
     (listener-eval string))))

(defun listener-eval (string)
  (request-listener-eval
   *connection*
   string
   (lambda (value)
     (declare (ignore value))
     (lem.listener-mode:listener-reset-prompt (repl-buffer)))
   (repl-buffer-width)))

(defun repl-read-string (thread tag)
  (unless (repl-buffer) (start-lisp-repl))
  (let ((buffer (repl-buffer)))
    (push thread (read-string-thread-stack))
    (push tag (read-string-tag-stack))
    (setf (current-window) (pop-to-buffer buffer))
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
        (setf (current-window) (pop-to-buffer buffer))
        (start-lisp-repl))))

(defun write-string-to-repl (string)
  (let ((buffer (repl-buffer)))
    (unless buffer
      (start-lisp-repl)
      (setf buffer (repl-buffer)))
    (with-point ((start (buffer-end-point buffer) :left-inserting))
      (when (alexandria:starts-with-subseq "CL-USER>" (line-string start))
        (insert-character start #\newline))
      (setf (variable-value 'enable-syntax-highlight :buffer start) t)
      (insert-string (buffer-end-point buffer) string)
      (syntax-scan-region start (buffer-end-point buffer))
      (setf (variable-value 'enable-syntax-highlight :buffer start) nil))
    (lem.listener-mode:listener-update-point (buffer-end-point buffer))
    (buffer-end (buffer-point buffer))
    (alexandria:when-let ((window (get-repl-window)))
      (with-current-window window
        (buffer-end (buffer-point buffer))
        (window-see window)))))
