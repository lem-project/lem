(defpackage :lem-clojure-mode/stacktrace
  (:use :cl :lem :lem/button)
  (:import-from :lem-clojure-mode/nrepl-client
                :check-nrepl-connection
                :nrepl-eval-sync
                :nrepl-response-value
                :nrepl-response-err)
  (:import-from :lem-clojure-mode
                :clojure-mode)
  (:export :clojure-stacktrace-mode
           :*clojure-stacktrace-mode-keymap*
           :clojure-show-last-exception
           :clojure-stacktrace-jump-to-source
           ;; Parsing utilities (for testing)
           :parse-stacktrace
           :make-stack-frame
           :stack-frame-class
           :stack-frame-method
           :stack-frame-file
           :stack-frame-line
           :stack-frame-clojure-p))

(in-package :lem-clojure-mode/stacktrace)

;;;; Attributes

(define-attribute stacktrace-header-attribute
  (t :foreground :base08 :bold t))

(define-attribute stacktrace-message-attribute
  (t :foreground :base09))

(define-attribute stacktrace-clojure-frame-attribute
  (t :foreground :base0D))

(define-attribute stacktrace-java-frame-attribute
  (t :foreground :base03))

(define-attribute stacktrace-file-attribute
  (t :foreground :base0B :underline t))

(define-attribute stacktrace-line-attribute
  (t :foreground :base0E))

;;;; Stacktrace Mode

(define-major-mode clojure-stacktrace-mode clojure-mode
    (:name "Clojure Stacktrace"
     :keymap *clojure-stacktrace-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *clojure-stacktrace-mode-keymap* "q" 'quit-active-window)
(define-key *clojure-stacktrace-mode-keymap* "Return" 'clojure-stacktrace-jump-to-source)
(define-key *clojure-stacktrace-mode-keymap* "n" 'clojure-stacktrace-next-frame)
(define-key *clojure-stacktrace-mode-keymap* "p" 'clojure-stacktrace-prev-frame)
(define-key *clojure-stacktrace-mode-keymap* "Tab" 'clojure-stacktrace-next-frame)
(define-key *clojure-stacktrace-mode-keymap* "Shift-Tab" 'clojure-stacktrace-prev-frame)
(define-key *clojure-stacktrace-mode-keymap* "j" 'clojure-stacktrace-toggle-java-frames)

;;;; Buffer Management

(defvar *stacktrace-buffer-name* "*clojure-stacktrace*")
(defvar *show-java-frames* nil)
(defvar *current-stacktrace* nil)

(defun stacktrace-buffer ()
  "Get or create the stacktrace buffer."
  (or (get-buffer *stacktrace-buffer-name*)
      (let ((buffer (make-buffer *stacktrace-buffer-name* :enable-undo-p nil)))
        (change-buffer-mode buffer 'clojure-stacktrace-mode)
        buffer)))

(defun show-stacktrace-buffer ()
  "Show the stacktrace buffer."
  (let ((buffer (stacktrace-buffer)))
    (pop-to-buffer buffer)))

;;;; Stacktrace Parsing

(defstruct stack-frame
  class
  method
  file
  line
  clojure-p)

(defun parse-stacktrace (stacktrace-str)
  "Parse a Java/Clojure stacktrace string into a list of frames."
  (let ((lines (uiop:split-string stacktrace-str :separator '(#\newline)))
        (frames nil)
        (exception-class nil)
        (exception-message nil))
    ;; First line is usually the exception
    (when lines
      (let ((first-line (first lines)))
        ;; Parse exception class and message
        (multiple-value-bind (match groups)
            (ppcre:scan-to-strings "^([a-zA-Z0-9_.]+(?:Exception|Error|Throwable)[^:]*):?\\s*(.*)" first-line)
          (when match
            (setf exception-class (aref groups 0)
                  exception-message (aref groups 1))))))
    ;; Parse stack frames
    (dolist (line (rest lines))
      (multiple-value-bind (match groups)
          (ppcre:scan-to-strings "^\\s+at\\s+([a-zA-Z0-9_.$]+)\\.([a-zA-Z0-9_$<>]+)\\(([^:]+):([0-9]+)\\)" line)
        (when match
          (push (make-stack-frame
                 :class (aref groups 0)
                 :method (aref groups 1)
                 :file (aref groups 2)
                 :line (parse-integer (aref groups 3) :junk-allowed t)
                 :clojure-p (or (search ".clj" (aref groups 2))
                               (search "$" (aref groups 0))))  ; Clojure classes have $
                frames))))
    (values (nreverse frames) exception-class exception-message)))

;;;; Stacktrace Display

(defun display-stacktrace (stacktrace-str)
  "Display a stacktrace in the stacktrace buffer."
  (multiple-value-bind (frames exception-class exception-message)
      (parse-stacktrace stacktrace-str)
    (setf *current-stacktrace* frames)
    (let ((buffer (stacktrace-buffer)))
      (with-buffer-read-only buffer nil
        (erase-buffer buffer)
        (with-point ((point (buffer-point buffer)))
          ;; Header
          (insert-string point "Clojure Exception" :attribute 'stacktrace-header-attribute)
          (insert-character point #\newline)
          (insert-string point (make-string 50 :initial-element #\=))
          (insert-character point #\newline)
          (insert-character point #\newline)
          ;; Exception info
          (when exception-class
            (insert-string point "Exception: " :attribute 'stacktrace-header-attribute)
            (insert-string point exception-class :attribute 'stacktrace-message-attribute)
            (insert-character point #\newline))
          (when (and exception-message (plusp (length exception-message)))
            (insert-string point "Message: " :attribute 'stacktrace-header-attribute)
            (insert-string point exception-message :attribute 'stacktrace-message-attribute)
            (insert-character point #\newline))
          (insert-character point #\newline)
          ;; Stack frames
          (insert-string point "Stack Trace:" :attribute 'stacktrace-header-attribute)
          (insert-character point #\newline)
          (insert-string point (make-string 50 :initial-element #\-))
          (insert-character point #\newline)
          (let ((frame-num 0))
            (dolist (frame frames)
              (when (or *show-java-frames* (stack-frame-clojure-p frame))
                (insert-frame point frame frame-num))
              (incf frame-num)))
          ;; Navigation hint
          (insert-character point #\newline)
          (insert-string point (make-string 50 :initial-element #\-))
          (insert-character point #\newline)
          (insert-string point "Press RET to jump to source, 'j' to toggle Java frames, 'q' to quit"
                         :attribute 'stacktrace-java-frame-attribute)))
      (buffer-start (buffer-point buffer))
      (show-stacktrace-buffer))))

(defun insert-frame (point frame frame-num)
  "Insert a stack frame at POINT."
  (let ((clojure-p (stack-frame-clojure-p frame)))
    ;; Frame number
    (insert-string point (format nil "~3D: " frame-num)
                   :attribute 'stacktrace-line-attribute)
    ;; Class and method
    (insert-string point (format nil "~A.~A"
                                 (stack-frame-class frame)
                                 (stack-frame-method frame))
                   :attribute (if clojure-p
                                  'stacktrace-clojure-frame-attribute
                                  'stacktrace-java-frame-attribute))
    ;; File and line
    (insert-string point " (")
    (let ((start (copy-point point :temporary)))
      (insert-string point (stack-frame-file frame)
                     :attribute 'stacktrace-file-attribute)
      (insert-string point ":")
      (insert-string point (format nil "~D" (stack-frame-line frame))
                     :attribute 'stacktrace-line-attribute)
      ;; Make it a button
      (apply-button-between-points
       start point
       (let ((file (stack-frame-file frame))
             (line (stack-frame-line frame)))
         (lambda (&rest args)
           (declare (ignore args))
           (jump-to-source file line)))))
    (insert-string point ")")
    (insert-character point #\newline)))

;;;; Source Navigation

(defun find-clojure-source-file (filename)
  "Try to find a Clojure source file in the project."
  ;; First, try the filename directly
  (when (probe-file filename)
    (return-from find-clojure-source-file filename))
  ;; Try to find in common source directories
  (let ((base-name (if (search "/" filename)
                       (subseq filename (1+ (position #\/ filename :from-end t)))
                       filename)))
    (dolist (dir '("src" "src/main/clojure" "src/clj" "test" "src/test/clojure"))
      (let* ((root (or (buffer-directory (current-buffer)) (uiop:getcwd)))
             (path (merge-pathnames (format nil "~A/~A" dir base-name) root)))
        (when (probe-file path)
          (return-from find-clojure-source-file (namestring path)))))
    ;; Try recursive search
    (let* ((root (or (buffer-directory (current-buffer)) (uiop:getcwd)))
           (pattern (format nil "**/**/~A" base-name)))
      (let ((files (directory (merge-pathnames pattern root))))
        (when files
          (return-from find-clojure-source-file (namestring (first files)))))))
  nil)

(defun jump-to-source (file line)
  "Jump to FILE at LINE."
  (let ((source-file (find-clojure-source-file file)))
    (if source-file
        (progn
          (find-file source-file)
          (move-to-line (current-point) line))
        (message "Could not find source file: ~A" file))))

;;;; Commands

(define-command clojure-show-last-exception () ()
  "Show the last exception stacktrace."
  (check-nrepl-connection)
  (let* ((code "(when-let [e *e]
                  (with-out-str
                    (clojure.stacktrace/print-cause-trace e)))")
         (responses (nrepl-eval-sync code :ns "user")))
    (let ((value (nrepl-response-value responses))
          (err (nrepl-response-err responses)))
      (cond
        (err (message "Error getting exception: ~A" err))
        ((and value (string/= value "nil"))
         ;; Remove surrounding quotes from the string
         (let ((stacktrace (string-trim '(#\") value)))
           ;; Unescape newlines
           (setf stacktrace (ppcre:regex-replace-all "\\\\n" stacktrace (string #\newline)))
           (display-stacktrace stacktrace)))
        (t (message "No exception to show"))))))

(define-command clojure-stacktrace-jump-to-source () ()
  "Jump to the source of the current stack frame."
  (let ((button (button-at (current-point))))
    (if button
        (button-action button)
        (message "No source location at point"))))

(define-command clojure-stacktrace-next-frame () ()
  "Move to the next stack frame."
  (or (search-forward-regexp (current-point) "^\\s*[0-9]+:")
      (message "No more frames")))

(define-command clojure-stacktrace-prev-frame () ()
  "Move to the previous stack frame."
  (line-start (current-point))
  (or (search-backward-regexp (current-point) "^\\s*[0-9]+:")
      (message "No previous frames")))

(define-command clojure-stacktrace-toggle-java-frames () ()
  "Toggle display of Java frames."
  (setf *show-java-frames* (not *show-java-frames*))
  (message "Java frames: ~A" (if *show-java-frames* "shown" "hidden"))
  ;; Redisplay if we have a stacktrace
  (when *current-stacktrace*
    ;; Re-fetch and display
    (clojure-show-last-exception)))

;;;; Add keybindings to clojure-mode

(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c C-x" 'clojure-show-last-exception)
