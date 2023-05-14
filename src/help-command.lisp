(in-package :lem)

(define-command describe-key () ()
  (show-message "describe-key: ")
  (redraw-display)
  (let* ((kseq (read-key-sequence))
         (cmd (find-keybind kseq)))
    (show-message (format nil "describe-key: ~a ~(~a~)"
                          (keyseq-to-string kseq)
                          cmd))))

(defun describe-bindings-internal (s name keymap &optional first-p)
  (unless first-p
    (princ #\page s)
    (terpri s))
  (let ((column-width 16))
    (loop :while keymap
          :do (format s "~A (~(~A~))~%" name (keymap-name keymap))
              (format s "~va~a~%" column-width "key" "binding")
              (format s "~va~a~%" column-width "---" "-------")
              (traverse-keymap keymap
                               (lambda (kseq command)
                                 (unless (equal "UNDEFINED-KEY" (symbol-name command))
                                   (format s "~va~(~a~)~%"
                                           column-width
                                           (keyseq-to-string kseq)
                                           (symbol-name command)))))
              (setf keymap (keymap-parent keymap))
              (terpri s))))


(define-command describe-bindings () ()
  (let ((buffer (current-buffer))
        (firstp t))
    (with-pop-up-typeout-window (s (make-buffer "*bindings*") :erase t)
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
  "Outputs all available major and minor modes."
  (with-pop-up-typeout-window (s (make-buffer "*all-modes*") :erase t)
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

(define-command describe-mode () ()
  "Show information about current major mode and enabled minor modes."
  (let* ((buffer (current-buffer))
         (major-mode (buffer-major-mode buffer))
         (minor-modes (buffer-minor-modes buffer)))
    (with-pop-up-typeout-window (s (make-buffer "*modes*") :erase t)
      (format s "Major mode is: ~A~@[ – ~A~]~%"
              (mode-name major-mode)
              (mode-description major-mode))
      (when minor-modes
        (format s "~2&Minor modes:~2%")
        (dolist (mode minor-modes)
          (format s "* ~A~@[ – ~A~]~%"
                  (mode-name mode)
                  (mode-description mode)))))))

(define-command execute-command (arg) ("P")
  (let* ((name (prompt-for-string
                (if arg
                    (format nil "~D M-x " arg)
                    "M-x ")
                :completion-function (lambda (str)
                                       (if (find #\- str)
                                           (completion-hypheen str (all-command-names))
                                           (completion str (all-command-names))))
                :test-function 'exist-command-p
                :history-symbol 'mh-execute-command))
         (command (find-command name)))
    (if command
        (call-command command arg)
        (message "invalid command"))))

(define-command apropos-command (str) ("sApropos: ")
  (with-pop-up-typeout-window (out (make-buffer "*Apropos*") :erase t)
    (dolist (name (all-command-names))
      (when (search str name)
        (describe (command-name (find-command name)) out)))))

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
