(defpackage #:lem-trans
  (:use #:cl #:lem)
  (:export #:*trans-program*
           #:*trans-target-language*
           #:*trans-source-language*
           #:*trans-engine*
           #:*trans-extra-arguments*
           #:*trans-popup-timeout*
           #:*trans-popup-style*
           #:trans-translate-region
           #:trans-translate-symbol
           #:trans-translate-string
           #:trans-set-target-language
           #:trans-set-engine
           #:trans-help))
(in-package #:lem-trans)

(defvar *trans-program* "trans"
  "Name or path of the translate-shell executable.")

(defvar *trans-target-language* "en"
  "Target language code passed to `trans`, for example \"en\" or \"zh\".")

(defvar *trans-source-language* nil
  "Source language code passed to `trans`, or NIL to let `trans` detect it.")

(defvar *trans-engine* nil
  "Default translation engine passed to `trans`, for example \"bing\".
When NIL, `trans` chooses its default engine.")

(defvar *trans-extra-arguments* '("-b" "-no-ansi" "-no-theme")
  "Command line arguments always passed to `trans`.
Add proxy or other options here, for example (\"-x\" \"127.0.0.1:7890\").")

(defvar *trans-popup-timeout* 10
  "Seconds before a translation popup disappears.
When NIL, the popup stays visible until the next message is shown.")

(defvar *trans-popup-style* '(:gravity :cursor)
  "Style plist used to display translation popups.")

(defun trans--empty-string-p (text)
  (str:blankp text))

(defun trans--check-program ()
  "Signal a user-facing error when the translate-shell executable is missing."
  (unless (exist-program-p *trans-program*)
    (editor-error "translate-shell not found: ~A. Install translate-shell or set `lem-trans:*trans-program*'."
                  *trans-program*)))

(defun trans--command-line (target)
  (append (list *trans-program*)
          *trans-extra-arguments*
          (when *trans-engine*
            (list "-e" *trans-engine*))
          (when *trans-source-language*
            (list "-s" *trans-source-language*))
          (list "-t" target)))

(defun trans--execute (command-line text)
  "Run COMMAND-LINE, feed TEXT on standard input, and return its output.
Signal an error when `trans` exits with a non-zero status."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program command-line
                        :input (make-string-input-stream text)
                        :output :string
                        :error-output :string
                        :external-format :utf-8
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (let ((message (str:trim error-output)))
        (error "trans exited with code ~D~@[~A~]"
               exit-code
               (unless (string= message "")
                 (format nil ": ~A" message)))))
    (str:trim-right output)))

(defun trans--translate-async (text command-line on-success &key on-finalize)
  "Translate TEXT with COMMAND-LINE in the background and call ON-SUCCESS on the editor thread.
ON-FINALIZE is called on the editor thread after success or failure."
  (bt2:make-thread
   (lambda ()
     (handler-case
         (let ((result (trans--execute command-line text)))
           (send-event
            (lambda ()
              (unwind-protect
                   (funcall on-success result)
                (when on-finalize
                  (funcall on-finalize))))))
       (error (e)
         (let ((message (princ-to-string e)))
           (send-event
            (lambda ()
              (unwind-protect
                   (editor-error "trans: ~A" message)
                (when on-finalize
                  (funcall on-finalize)))))))))
   :name "trans"))

(defun trans--show-result (result)
  (show-message result
                :timeout *trans-popup-timeout*
                :style *trans-popup-style*))

(defun trans--translate (text &key (on-success #'trans--show-result) on-finalize)
  (when (trans--empty-string-p text)
    (editor-error "No text to translate"))
  (trans--check-program)
  (message-without-log "Translating...")
  (trans--translate-async
   text
   (trans--command-line *trans-target-language*)
   (lambda (result)
     (if (trans--empty-string-p result)
         (editor-error "trans returned an empty translation")
         (funcall on-success result)))
   :on-finalize on-finalize))

(defun trans--delete-point (point)
  (when (alive-point-p point)
    (delete-point point)))

(defun trans--region-intact-p (start end source)
  (let ((buffer (point-buffer start)))
    (and (not (deleted-buffer-p buffer))
         (eq buffer (point-buffer end))
         (alive-point-p start)
         (alive-point-p end)
         (string= source (points-to-string start end)))))

(defun trans--replace-region (start end source translation)
  (if (trans--region-intact-p start end source)
      (progn
        (buffer-undo-boundary (point-buffer start))
        (delete-between-points start end)
        (insert-string start translation)
        (message "Translated."))
      (trans--show-result translation)))

(define-command trans-translate-region (start end replace) (:region :universal-nil)
  "Translate the active region using translate-shell (`trans`).
With a prefix argument, replace the region with the translation instead of
showing it in a popup. If the region changed while waiting for the
translation, the popup is shown instead of replacing text."
  (let ((source (points-to-string start end)))
    (when (trans--empty-string-p source)
      (editor-error "No text to translate"))
    (if replace
        (let ((start-point (copy-point (point-min start end) :right-inserting))
              (end-point (copy-point (point-max start end) :left-inserting)))
          (trans--translate
           source
           :on-success (lambda (translation)
                         (trans--replace-region start-point end-point source translation))
           :on-finalize (lambda ()
                          (trans--delete-point start-point)
                          (trans--delete-point end-point))))
        (trans--translate source))))

(define-command trans-translate-symbol () ()
  "Translate the symbol at point using translate-shell (`trans`)."
  (let ((text (symbol-string-at-point (current-point))))
    (unless text
      (editor-error "No symbol at point"))
    (trans--translate text)))

(define-command trans-translate-string (text) ((:string "Text to translate: "))
  "Translate a string entered in the minibuffer using translate-shell (`trans`)."
  (trans--translate text))

(define-command trans-set-target-language () ()
  "Set the target language used by the trans commands for this session."
  (let ((language (prompt-for-string "Target language: "
                                     :initial-value *trans-target-language*
                                     :history-symbol 'trans-target-language)))
    (unless (trans--empty-string-p language)
      (setf *trans-target-language* language))))

(define-command trans-set-engine () ()
  "Set the translation engine used by the trans commands for this session.
An empty input restores the default engine selection."
  (let ((engine (prompt-for-string "Engine (empty for default): "
                                   :initial-value (or *trans-engine* "")
                                   :history-symbol 'trans-engine)))
    (setf *trans-engine* (unless (trans--empty-string-p engine)
                           engine))))

(define-command trans-help () ()
  "Show help for the trans commands and options."
  (with-pop-up-typeout-window (s (make-buffer "*trans-help*") :erase t)
    (format s "Translate text with translate-shell (`trans`).~&")
    (format s "~%")
    (format s "Requirements~&")
    (format s "  Install translate-shell and make sure `trans` is on PATH.~&")
    (format s "  If the executable has another name or path, set~&")
    (format s "  `lem-trans:*trans-program*`.~&")
    (format s "~%")
    (format s "Commands~&")
    (format s "  trans-translate-region    Translate the region. With a prefix~&")
    (format s "                            argument, replace the region with the~&")
    (format s "                            translation instead of showing it.~&")
    (format s "  trans-translate-symbol    Translate the symbol at point.~&")
    (format s "  trans-translate-string    Prompt for text and translate it.~&")
    (format s "  trans-set-target-language Set the target language for this session.~&")
    (format s "  trans-set-engine          Set the translation engine for this~&")
    (format s "                            session. An empty input restores the~&")
    (format s "                            default engine selection.~&")
    (format s "  trans-help                Show this help.~&")
    (format s "~%")
    (format s "Options (set with setf, for example in init.lisp)~&")
    (format s "  lem-trans:*trans-program*          Executable name or path.~&")
    (format s "  lem-trans:*trans-target-language*  Target language code (default \"en\").~&")
    (format s "  lem-trans:*trans-source-language*  Source language, or NIL to detect it.~&")
    (format s "  lem-trans:*trans-engine*           Engine name, or NIL to let trans~&")
    (format s "                                     choose its default engine.~&")
    (format s "  lem-trans:*trans-extra-arguments*  Extra command line arguments.~&")
    (format s "  lem-trans:*trans-popup-timeout*    Popup timeout in seconds, or NIL.~&")
    (format s "  lem-trans:*trans-popup-style*      Popup style plist.~&")
    (format s "~%")
    (format s "  Run `trans -list-engines` to see the engines supported by your~&")
    (format s "  translate-shell installation.~&")
    (format s "~%")
    (format s "Suggested key bindings~&")
    (format s "  (define-key *global-keymap* \"C-c t\" 'lem-trans:trans-translate-region)~&")
    (format s "  (define-key *global-keymap* \"C-c T\" 'lem-trans:trans-translate-symbol)~&")))
