(defpackage #:lem-trans
  (:use #:cl #:lem)
  (:export #:*trans-program*
           #:*trans-target-language*
           #:*trans-source-language*
           #:*trans-extra-arguments*
           #:*trans-popup-timeout*
           #:*trans-popup-style*
           #:trans-translate-region
           #:trans-translate-symbol
           #:trans-translate-string
           #:trans-set-target-language))
(in-package #:lem-trans)

(defvar *trans-program* "trans"
  "Name or path of the translate-shell executable.")

(defvar *trans-target-language* "en"
  "Target language code passed to `trans`, for example \"en\" or \"zh\".")

(defvar *trans-source-language* nil
  "Source language code passed to `trans`, or NIL to let `trans` detect it.")

(defvar *trans-extra-arguments* '("-b" "-no-ansi" "-no-theme")
  "Command line arguments always passed to `trans`.
Add engine or proxy options here, for example (\"-e\" \"bing\").")

(defvar *trans-popup-timeout* 10
  "Seconds before a translation popup disappears.
When NIL, the popup stays visible until the next message is shown.")

(defvar *trans-popup-style* '(:gravity :cursor)
  "Style plist used to display translation popups.")

(defun trans--empty-string-p (text)
  (string= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))

(defun trans--command-line (target)
  (append (list *trans-program*)
          *trans-extra-arguments*
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
      (let ((message (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  error-output)))
        (error "trans exited with code ~D~@[~A~]"
               exit-code
               (unless (string= message "")
                 (format nil ": ~A" message)))))
    (string-right-trim '(#\Newline #\Return) output)))

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
