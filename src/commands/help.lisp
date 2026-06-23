(defpackage :lem-core/commands/help
  (:use :cl :lem-core)
  (:export :describe-key
           :describe-bindings
           :describe-mode
           :apropos-command
           :describe-command
           :describe-variable
           :lem-version
           :list-modes
           :describe-all-modes
           :*describe-output-type-override*)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/help)

(define-key *global-keymap* "C-x ?" 'describe-key)

(declaim (type (member nil :buffer :popup :message)
               *describe-output-type-override*))
(defvar *describe-output-type-override* nil
  "When non nil, describe commands will always
   send their information to this type of output.
   Acceptable values: (nil :buffer :popup :message) ")

(defun call-with-describe-output-stream (requested-output-type buffer-name function)
  (ecase (or *describe-output-type-override* requested-output-type)
    ((:message)
     (let ((stream (make-string-output-stream)))
       (unwind-protect
            (funcall function stream)
         (show-message (get-output-stream-string stream)))))
    ((:popup)
     (with-pop-up-typeout-window (stream (make-buffer buffer-name) :erase t)
       (funcall function stream)))
    ((:buffer)
     (let ((stream (make-string-output-stream)))
       (unwind-protect
            (funcall function stream)
         (let ((output-buffer (make-buffer buffer-name)))
           (erase-buffer output-buffer)
           (insert-string (buffer-point output-buffer) (get-output-stream-string stream))
           (pop-to-buffer output-buffer)))))))

(defmacro with-describe-output-stream
    ((var requested-output-type &optional (buffer-name "*Description*"))
     &body body)
  "Executes body in a lexical context where a documentation output stream called
   'var' has been established. The documentation output stream type will be either
   a :message, a :popup, or a :buffer. If *describe-output-type-override* is set,
   then its value will override the requested-output-type, allowing the user to,
   for example, always output documentation descriptions to a full buffer, rather
   then just a temporary message or popup"
  `(call-with-describe-output-stream ,requested-output-type ,buffer-name
                                     (lambda (,var)
                                       ,@body)))
  
(define-command describe-key () ()
  "Tell what is the command associated to a keybinding."
  (show-message "describe-key: ")
  (redraw-display)
  (let* ((kseq (read-key-sequence))
         (cmd (find-keybind kseq)))
    (with-describe-output-stream (s :message)
      (format s "describe-key: ~a ~(~a~)"
              (keyseq-to-string kseq)
              cmd))))

(defun describe-bindings-internal (s name keymap &optional first-p)
  (unless first-p
    (princ #\page s)
    (terpri s))
  (let ((column-width 16))
    (loop :while keymap
          :do (format s "~A (~(~A~))~%" name (keymap-description keymap))
              (format s "~va~a~%" column-width "key" "binding")
              (format s "~va~a~%" column-width "---" "-------")
              (traverse-keymap keymap
                               (lambda (kseq command)
                                 (unless (equal "UNDEFINED-KEY" (symbol-name command))
                                   (format s "~va~(~a~)~%"
                                           column-width
                                           (keyseq-to-string kseq)
                                           (symbol-name command)))))
              (setf keymap nil)
              (terpri s))))

(define-command describe-bindings () ()
  "Describe the bindings of the buffer's current major mode."
  (let ((buffer (current-buffer))
        (firstp t))
    (with-describe-output-stream (s :popup "*bindings*")
      (describe-bindings-internal s
                                  (mode-name (buffer-major-mode buffer))
                                  (setf firstp (mode-keymap (buffer-major-mode buffer)))
                                  t)
      (setf firstp (not firstp))
      (dolist (mode (buffer-minor-modes buffer))
        (describe-bindings-internal s
                                    (mode-name mode)
                                    (mode-keymap mode)
                                    firstp)
        (when (mode-keymap mode)
          (setf firstp t)))
      (describe-bindings-internal s
                                  "Global"
                                  *global-keymap*
                                  firstp))))

(define-command list-modes () ()
  "Output all available major and minor modes."
  (with-describe-output-stream (s :popup "*all-modes*")
    (let ((major-modes (major-modes))
          (minor-modes (minor-modes)))
      (labels ((is-technical-mode (mode)
                 (eql (elt (symbol-name mode) 0)
                      #\%))
               (print-modes (title modes)
                 (format s "~A:~2%" title)
                 ;; Remove technical modes where name starts with %
                 (let* ((modes (remove-if #'is-technical-mode modes))
                        (sorted-modes (sort modes #'string< :key #'string-downcase)))
                   (dolist (mode sorted-modes)
                     (format s "* ~A~@[ – ~A~]~%"
                             (mode-name mode)
                             (mode-description mode))))
                 (format s "~2%")))
        (print-modes "Major modes" major-modes)
        (print-modes "Minor modes" minor-modes)))))

(define-command describe-all-modes () ()
  "Alias for list-modes"
  (call-command 'list-modes nil))

(define-command describe-mode () ()
  "Show information about current major mode and enabled minor modes."
  (let* ((buffer (current-buffer))
         (major-mode (buffer-major-mode buffer))
         (minor-modes (buffer-minor-modes buffer))
         (global-minor-modes (loop :for mode :in (minor-modes)
                                   :when (and (mode-active-p buffer mode)
                                              (not (find mode minor-modes)))
                                   :collect mode)))
    (with-describe-output-stream (s :popup "*modes*")
      (format s "Major mode is: ~A~@[ – ~A~]~%"
              (mode-name major-mode)
              (mode-description major-mode))
      (when minor-modes
        (format s "~2&Minor modes enabled in this buffer:~2%")
        (dolist (mode minor-modes)
          (format s "* ~A~@[ – ~A~]~%"
                  (mode-name mode)
                  (mode-description mode))))
      (when global-minor-modes
        (format s "~2&Global minor modes:~2%")
        (dolist (mode global-minor-modes)
          (format s "* ~A~@[ – ~A~]~%"
                  (mode-name mode)
                  (mode-description mode)))))))

(define-command apropos-command () ()
  "Find all commands in the running Lisp image whose names match a given string."
  (let ((str (prompt-for-string
              "Apropos: "
              :completion-function
              (lambda (str) (completion str (all-command-names))))))
    (with-describe-output-stream (out :popup "*Apropos*")
      (dolist (name (all-command-names))
        (when (search str name)
          (describe (command-name (find-command name)) out))))))

(define-command describe-command () ()
  "Alias for apropos-command"
  (call-command 'apropos-command nil))

(defun lem-symbol-p (symbol)
  (let* ((pkg (symbol-package symbol))
         (name (package-name pkg)))
    (string-equal "LEM" (subseq name 0 3))))

(defun lem-variable-p (symbol)
  "Returns true if the symbol is a lem variable or an editor variable"
  (or (lem/common/var:editor-variable-p (get symbol 'editor-variable))
      (and (lem-symbol-p symbol)
           (boundp symbol)
           (not (constantp symbol)))))

(defun list-all-lem-variables ()
  "Returns a list of all strings that represent lem variables"
  (loop
    :for pkg :in (list-all-packages)
    :appending 
       (loop
         :for sym :being :the external-symbols :of (find-package pkg)
         :when (lem-variable-p sym)
         :collect (string-downcase
                   (format nil "~a:~a"
                           (package-name (find-package pkg))
                           (symbol-name sym))))
    :into syms
    :finally (return syms)))
  


(define-command describe-variable () ()
  "Describe a lem variable who's name matches a given string."
  (let* ((all-lem-variables (list-all-lem-variables))
         (str (prompt-for-string
              "Enter Variable: "
              :completion-function
              (lambda (str) (completion str all-lem-variables)))))
    (with-describe-output-stream (out :popup "*variable-description*")
      ;; TODO get a better description than just describe
      (let* ((sym (read-from-string str))
             (editor-variable (get sym 'editor-variable)))
        (cond
          ((lem/common/var:editor-variable-p editor-variable)
           (format
            out
            "~a is an EDITOR VARIABLE bound to the following value: ~%~%" sym)
           (describe editor-variable out))
          ((lem-variable-p sym)
           (describe sym out))
          (t 
           (format out "~a does not describe any variable" sym)))))))

(define-command lem-version () ()
  "Display Lem's version."
  (let ((version (get-version-string)))
    (with-describe-output-stream (s :message "*Version*")
      (format s "~a" version))))

(define-command help () ()
  "Show some help."
  (with-describe-output-stream (s :popup "*Help*")
    (format s "Welcome to Lem.~&")
    (format s "You are running ~a.~&" (get-version-string))
    (format s "~%")
    (format s "To open a file in Lem, use C-x C-f. Close it with C-x k.~&")
    (format s "Switch buffers with C-x b.~&")
    (format s "Quit Lem with C-x C-c.~&")
    (format s "You can use Vi keys: activate the mode with Alt-x vi-mode.~&")
    (format s "~%")
    (format s "To discover all available keybindings, type Alt-x documentation-describe-bindings.~&")
    (format s "You can check what command a given keybinding is bound to with Alt-x describe-key.~&")
    (format s "~%")
    (format s "Lem works out of the box for Common Lisp development. ~%")
    (format s "You can compile a function right away with C-c C-c,~%")
    (format s "and open a REPL with M-x start-lisp-repl.~%")
    (format s "(the notation M-x means Alt-x.)~%")
    (format s "~%")
    (format s "But Lem supports other programming languages thanks to its built-in LSP client.~%")
    (format s "Please consult our online documentation.~%")))
