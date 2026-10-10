(defpackage :lem-prolog.run-prolog
  (:use :cl :lem :lem-process)
  (:export :prolog-dwim
           :prolog-toplevel
           :prolog-consult
           :prolog-kill-prolog
           :prolog-remove-interactions
           :prolog-localize
           :prolog-unlocalize))
(in-package :lem-prolog.run-prolog)

(define-editor-variable prolog-program "scryer-prolog"
  "Program name of the Scryer Prolog executable.")

(define-editor-variable prolog-program-switches nil
  "List of switches passed to the Scryer Prolog process, e.g. '(\"-f\").")

(define-editor-variable prolog-default-prefix "%@ "
  "String to prepend when inserting output from the Prolog process into the buffer.
Only used for queries written with %?- or %:-; queries without a % get no prefix.")

(define-editor-variable prolog-max-history 80000
  "Maximal size of the history buffer, or nil to never truncate it.")



(defstruct prolog-session
  (process nil)
  (process-buffer nil)
  (marker nil)
  (accum "")
  (seen-prompt nil)
  (read-term nil)
  (interrupted nil)
  (consulting-p nil)
  (indent-prefix "")
  (prefix "")
  (output "")
  (history-buffer nil)
  (buffer nil))

(defvar *default-prolog-session* (make-prolog-session)
  "The shared Prolog session used unless a buffer is localized.")

(defun prolog-session-for-buffer (&optional (buffer (current-buffer)))
  "Return BUFFER's local Prolog session, or the shared default session."
  (or (buffer-value buffer 'prolog-session)
      *default-prolog-session*))

(defun prolog-session-variable-value (session variable)
  "Return VARIABLE's value for SESSION, respecting local settings."
  (let ((buffer (prolog-session-buffer session)))
    (if buffer
        (variable-value variable :default buffer)
        (variable-value variable :global))))

(defvar *prolog-escape-scanner*
  (ppcre:create-scanner (concatenate 'string (string #\Esc)
                                      "\\[[0-9;?=]*[A-Za-z]"))
  "Matches ANSI CSI escape sequences.  The Prolog process runs on a pty,
so Scryer colorizes its prompt (e.g. \e[K?- \e[3C) and readline-based
processes emit terminal control sequences; strip them all.")

(defvar *prolog-partial-escape-scanner*
  (ppcre:create-scanner (concatenate 'string (string #\Esc)
                                      "\\[[0-9;?=]*(?=" (string #\Esc) ")"))
  "Matches an incomplete CSI sequence immediately followed by another
ESC, i.e. an abandoned partial sequence split across chunks.")

(defvar *prolog-escape-end-scanner*
  (ppcre:create-scanner (concatenate 'string (string #\Esc)
                                      "(?:\\[[0-9;?=?]*)$"))
  "Matches a string ending in a possibly-incomplete CSI sequence.")

(defun prolog-partial-escape-p (part)
  "True if PART ends with what may be an ANSI escape sequence that is
not yet complete (an escape sequence split across output chunks)."
  (and (find #\Esc part)
       (ppcre:scan *prolog-escape-end-scanner* part)))

(defun prolog-prompt ()
  "Prompt of the Scryer Prolog toplevel."
  "?- ")

(defun prolog-running-p (session)
  "True iff SESSION has a running Prolog process."
  (let ((process (prolog-session-process session)))
    (and process (process-alive-p process))))

(defun prolog-more-solutions-p (session)
  "True iff the process could still produce output for the current query."
  (not (prolog-session-seen-prompt session)))

(defun prolog-process-ready (session)
  "Signal an error if the previous query is still in progress."
  (when (and (prolog-session-interrupted session)
             (prolog-running-p session)
             (not (prolog-session-seen-prompt session)))
    (editor-error "Previous query still in progress; use `prolog-toplevel'"))
  (setf (prolog-session-interrupted session) nil))


(defun prolog-time-string ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defun prolog-log (session str)
  "Append STR to the *prolog-history* buffer of SESSION, truncating if too large."
  (let ((max (prolog-session-variable-value session 'prolog-max-history)))
    (when (and max (plusp (length str)))
      (unless (prolog-session-history-buffer session)
        (setf (prolog-session-history-buffer session)
              (make-buffer (if (prolog-session-buffer session)
                               (format nil "*prolog-history*: ~a"
                                       (buffer-name (prolog-session-buffer session)))
                               "*prolog-history*"))))
      (let ((history (prolog-session-history-buffer session)))
        (insert-string (buffer-end-point history) str)
        (let ((size (position-at-point (buffer-end-point history))))
          (when (> size max)
            (with-point ((p (buffer-start-point history)))
              (character-offset p (- size (truncate max 2)))
              (delete-between-points (buffer-start-point history) p))))))))


(defun prolog-insert-output (session str)
  "Insert STR at the marker of SESSION, preceding each line with indentation and prefix.

Output arrives in fragments (line editor redraws), so the prefix is only
added when a fragment starts a line: prefixing a mid-line fragment would
corrupt the buffer."
  (let ((marker (prolog-session-marker session)))
    (when (and marker (alive-point-p marker))
      (let* ((prefix (format nil "~A~A" (prolog-session-indent-prefix session)
                             (prolog-session-prefix session)))
             (start-of-line-p (start-line-p marker)))
        (when start-of-line-p
          (insert-string marker prefix))
        (insert-string marker
                       (ppcre:regex-replace-all (string #\Newline) str
                                                (format nil "~%~A" prefix)))))))

(defun prolog-filter (session process string)
  "Output callback of the Prolog process: SESSION receives the output.

Escape sequences are removed, since Scryer colorizes its prompt.  A trailing
partial escape sequence or a partial prompt is held back until the next
chunk, because it may be completed there.  Newlines right before the prompt
are dropped, since they would otherwise leave a bare prefix line; other
trailing newlines are held back until more output shows what follows them."
  ;; Ignore callbacks queued by a process that has since been killed or replaced.
  (unless (eq process (prolog-session-process session))
    (return-from prolog-filter))
  ;; Once the prompt has been seen, ignore stray input/echo from the process.
  (when (prolog-session-seen-prompt session)
    (prolog-log session string)
    (return-from prolog-filter))
  (with-accessors ((accum prolog-session-accum)
                   (process-buffer prolog-session-process-buffer)
                   (read-term prolog-session-read-term)
                   (seen-prompt prolog-session-seen-prompt))
      session
    (setf string (remove #\Return string))
    (setf accum (concatenate 'string accum string))
    (setf accum (ppcre:regex-replace-all *prolog-partial-escape-scanner* accum ""))
    (setf accum (ppcre:regex-replace-all *prolog-escape-scanner* accum ""))
    (when process-buffer
      (multiple-value-bind (start end) (ppcre:scan "(?m)^\\|: $" accum)
        (declare (ignore end))
        (when start
          ;; Keep the "|: " prompt in the output so it is visible in the buffer.
          (setf read-term t)))
      (multiple-value-bind (start end)
          (ppcre:scan (format nil "(?m)^~a$" (ppcre:quote-meta-chars (prolog-prompt)))
                      accum)
        (declare (ignore end))
        (when start
          (setf seen-prompt t)
          (setf accum (subseq accum 0 start))))
      (let* ((nl (position #\Newline accum :from-end t))
             (tail-start (if nl (1+ nl) 0))
             (part (subseq accum tail-start))
             (prompt (prolog-prompt))
             (hold-back-p
               (and (plusp (length part))
                    (or (and (<= (length part) (length prompt))
                             (string= part prompt :end2 (length part)))
                        (prolog-partial-escape-p part)))))
        (unless (and hold-back-p (zerop tail-start))
          (let* ((insertable (if hold-back-p
                                 (subseq accum 0 tail-start)
                                 accum))
                 (body (if read-term
                           insertable
                           (string-right-trim '(#\Newline) insertable)))
                 (pending (if (or seen-prompt read-term)
                              ""
                              (subseq insertable (length body)))))
            (when (plusp (length body))
              (setf (prolog-session-output session)
                    (concatenate 'string (prolog-session-output session) body))
              (unless (prolog-session-consulting-p session)
                (prolog-insert-output session body)))
            (setf accum (concatenate 'string pending (if hold-back-p part "")))))))
    (prolog-log session string)))

(defun prolog-wait-for-prompt (session)
  "Wait until the process prints its prompt, or the process exits.
Signals editor-abort if the user presses C-g.

C-g is checked explicitly: sit-for only treats the abort key specially when
it is bound to *abort-key*, and with vi-mode C-g is bound to
vi-keyboard-quit instead."
  (loop :while (and (not (prolog-session-seen-prompt session))
                    (prolog-running-p session))
        :do (let ((event (sit-for 0.1)))
              (when (key-p event)
                (let ((key (read-key)))
                  (when (match-key key :ctrl t :sym "g")
                    (error 'editor-abort)))))))

(defun prolog-interrupt (session)
  "Interrupt the Prolog process of SESSION (SIGINT), like C-c in a terminal.

lem-process does not export the process pointer or the pid, and SIGINT
has to be sent to the child, so these internal accessors are used
directly.  Replace them if lem-process ever exports process-pid."
  (handler-case
      (uiop:run-program (list "kill" "-INT"
                              (format nil "~d" (async-process::process-pid
                                                (lem-process::process-pointer
                                                 (prolog-session-process session)))))
                        :ignore-error-status t)
    (error () nil)))

(defun prolog-send-string (session str)
  "Send STR to the Prolog process of SESSION and log it."
  (prolog-log session str)
  (process-send-input (prolog-session-process session) str))

(defun prolog-run-prolog (session)
  "Start a Prolog process for SESSION and wait for its prompt.

The process runs with TERM=dumb: async-process runs the child on a pty, and
with a normal TERM the toplevel uses its line editor and floods the output
with redraws and ANSI sequences, and a redraw's \"?- \" prefix can be mistaken
for the real prompt.  TERM=dumb gives a clean stream of prompts and answers."
  (let* ((program (prolog-session-variable-value session 'prolog-program))
         (switches (or (prolog-session-variable-value session 'prolog-program-switches) '()))
         (proc (run-process (append (list "env" "TERM=dumb" program) switches)
                            :name "prolog"
                            :directory (or (buffer-directory (current-buffer))
                                           (uiop:getcwd))
                            :output-callback (lambda (process string)
                                               (prolog-filter session process string))
                            :output-callback-type :process-input)))
    (setf (prolog-session-process session) proc
          (prolog-session-process-buffer session) (current-buffer)
          (prolog-session-accum session) ""
          (prolog-session-output session) ""
          (prolog-session-seen-prompt session) nil
          (prolog-session-read-term session) nil
          (prolog-session-consulting-p session) nil)
    (prolog-log session (format nil "~a: starting: ~a~%"
                                (prolog-time-string)
                                (format nil "~{~a~^ ~}" (cons program switches))))
    (handler-case (prolog-wait-for-prompt session)
      (editor-abort () (setf (prolog-session-interrupted session) t)))
    (unless (prolog-session-seen-prompt session)
      (editor-error "No prompt from: ~a" program))))

(defun prolog-kill-process (session)
  "Kill the Prolog process of SESSION and clear its process state."
  (let ((process (prolog-session-process session))
        (marker (prolog-session-marker session)))
    (when (and process (process-alive-p process))
      (delete-process process))
    (when (and marker (alive-point-p marker))
      (delete-point marker))
    (setf (prolog-session-process session) nil
          (prolog-session-process-buffer session) nil
          (prolog-session-marker session) nil
          (prolog-session-accum session) ""
          (prolog-session-output session) ""
          (prolog-session-seen-prompt session) nil
          (prolog-session-read-term session) nil
          (prolog-session-interrupted session) nil
          (prolog-session-consulting-p session) nil)
    (when process
      (prolog-log session (format nil "~a: process killed.~%" (prolog-time-string))))))


(defun prolog-find-query-end (point)
  "Search forward from POINT for a `.' that ends a line (optionally
followed by whitespace and a % comment), and move POINT past it.
Return POINT on success, nil otherwise."
  (multiple-value-bind (end-point groups)
      (search-forward-regexp point "\\.([\\t ]*(%.*)?)$")
    (declare (ignore groups))
    end-point))

(defun prolog-interact (session query)
  "Send QUERY to the Prolog process of SESSION and interact as on a terminal."
  (unless (prolog-running-p session)
    (prolog-run-prolog session))
  (prolog-process-ready session)
  (setf (prolog-session-process-buffer session) (current-buffer)
        (prolog-session-accum session) ""
        (prolog-session-output session) ""
        (prolog-session-seen-prompt session) nil
        (prolog-session-read-term session) nil)
  (let ((marker (prolog-session-marker session)))
    (unless (and marker
                 (alive-point-p marker)
                 (eq (point-buffer marker) (prolog-session-process-buffer session)))
      (when (and marker (alive-point-p marker))
        (delete-point marker))
      (setf (prolog-session-marker session) (make-buffer-point (current-point)))))
  (prolog-send-string session (format nil "~a~%" query))
  (%prolog-toplevel session))

(defun prolog-set-query-prefix (session groups)
  "Set SESSION's indentation and output prefix from query-regexp GROUPS."
  (setf (prolog-session-indent-prefix session) (aref groups 0)
        (prolog-session-prefix session)
        (if (string= (aref groups 1) "")
            ""
            (prolog-session-variable-value session 'prolog-default-prefix))))

(defun prolog-query (session)
  "If point is on a query, send it to the process of SESSION and start interaction.
Return true if point was on a query."
  (unless (buffer-mark-p (current-buffer))
    (with-point ((p (current-point)))
      (line-start p)
      (multiple-value-bind (match groups)
          (looking-at p "([\\t ]*)(%*)[\\t ]*[:?]- *")
        (when match
          (prolog-set-query-prefix session groups)
          (character-offset p (length match))
          (let ((qstart (copy-point p :temporary)))
            (unless (prolog-find-query-end p)
              (editor-error "Missing `.' at the end of this query"))
            (let ((query (points-to-string qstart p)))
              (let ((point (current-point)))
                (line-end point)
                (insert-string point
                               (format nil "~%~A~A"
                                       (prolog-session-indent-prefix session)
                                       (prolog-session-prefix session)))
                (setf (prolog-session-marker session) (make-buffer-point point)))
              (prolog-interact session query)))
          t)))))

(defun %prolog-toplevel (session)
  "Start or resume Prolog toplevel interaction for SESSION in the buffer.

You can use this function if you have previously quit (with C-g) waiting
for a longer-running query and now want to resume interaction."
  (when (prolog-session-process session)
    (let ((process-buffer (prolog-session-process-buffer session)))
      (when (and process-buffer
                 (not (deleted-buffer-p process-buffer))
                 (not (eq process-buffer (current-buffer))))
        (switch-to-buffer process-buffer)))
    (handler-case
        (loop :while (and (not (prolog-session-seen-prompt session))
                          (prolog-running-p session))
              :do (if (prolog-session-read-term session)
                      (let ((input (prompt-for-string "Input: ")))
                        (setf (prolog-session-read-term session) nil)
                        (prolog-insert-output session (format nil "~a~%" input))
                        (prolog-send-string session (format nil "~a~%" input)))
                      (let ((event (sit-for 0.1)))
                        (cond ((or (null event) (eq event :timeout)) nil)
                              ((key-p event)
                               (let ((key (read-key)))
                                 (cond ((match-key key :ctrl t :sym "g")
                                        (error 'editor-abort))
                                       ((match-key key :ctrl t :sym "c")
                                        (prolog-interrupt session))
                                       ((insertion-key-p key)
                                        (process-send-input
                                         (prolog-session-process session)
                                         (string (if (char= (insertion-key-p key)
                                                            #\Return)
                                                     #\Newline
                                                     (insertion-key-p key)))))
                                       (t (message "Non-character key")))))))))
      (editor-abort () (setf (prolog-session-interrupted session) t)))))


(defun prolog-goto-first-error (session buffer source-start previous-point)
  "Move to the first consult error for SESSION, preserving PREVIOUS-POINT as mark.
SOURCE-START is the beginning of the whole buffer or consulted region."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "(?m)^ERROR.*?:([0-9]+)|(?m)^ *error\\(.*:([0-9]+)"
                             (prolog-session-output session))
    (declare (ignore match))
    (let ((line (and groups (find-if #'identity groups))))
      (when line
        (with-point ((target source-start))
          (let ((target-line (+ (line-number-at-point source-start)
                                (1- (parse-integer line)))))
            (when (move-to-line target target-line)
              (setf (buffer-mark buffer) previous-point)
              (move-point (buffer-point buffer) target))))))))

(defun prolog-consult-output-display-p (output)
  "True if OUTPUT contains non-comment output that is not plain success."
  (let* ((without-comments
           (ppcre:regex-replace-all "(?m)^[\\t ]*%.*(?:\\n|$)" output ""))
         (visible (string-trim '(#\Space #\Tab #\Newline) without-comments))
         (trimmed (string-trim '(#\Space #\Tab #\Newline) output)))
    (and (plusp (length visible))
         (not (ppcre:scan "^true\\." trimmed)))))

(defun %prolog-consult (session new-process)
  "Load the current buffer (or region) into the Prolog process of SESSION.
With NEW-PROCESS non-nil, start a new process first."
  (when new-process
    (prolog-kill-process session))
  (unless (prolog-running-p session)
    (prolog-run-prolog session))
  (prolog-process-ready session)
  (let* ((buffer (current-buffer))
         (region-p (buffer-mark-p buffer))
         (start (if region-p (region-beginning buffer) (buffer-start-point buffer)))
         (end (if region-p (region-end buffer) (buffer-end-point buffer))))
    (with-point ((source-start start)
                 (source-end end)
                 (previous-point (current-point)))
      (setf (prolog-session-process-buffer session) buffer
            (prolog-session-accum session) ""
            (prolog-session-output session) ""
            (prolog-session-seen-prompt session) nil
            (prolog-session-read-term session) nil)
      (uiop:with-temporary-file (:pathname temp :type "pl")
        (uiop:with-output-file (out temp :if-exists :supersede)
          (write-string (points-to-string source-start source-end) out))
        ;; Consult output belongs in the typeout buffer, not at a possibly
        ;; stale query marker in the source buffer.
        (setf (prolog-session-consulting-p session) t)
        (unwind-protect
             (progn
               (prolog-send-string
                session (format nil "['~a'].~%" (uiop:native-namestring temp)))
               (handler-case (prolog-wait-for-prompt session)
                 (editor-abort () (setf (prolog-session-interrupted session) t))))
          (setf (prolog-session-consulting-p session) nil))
        (when (prolog-session-seen-prompt session)
          (message "~a consulted." (if region-p "Region" "Buffer"))
          (let ((output (prolog-session-output session)))
            (when (prolog-consult-output-display-p output)
              (with-pop-up-typeout-window
                  (out (make-buffer (if (prolog-session-buffer session)
                                        (format nil "*prolog-consult*: ~a"
                                                (buffer-name (prolog-session-buffer session)))
                                        "*prolog-consult*"))
                   :erase t)
                (write-string output out))))
          (prolog-goto-first-error session buffer source-start previous-point))))))

(defun %prolog-remove-interactions (session)
  "Remove interaction lines in the active region, or in the whole buffer.
Refuses to run when SESSION's prefix is empty, since it would match every line."
  (when (string= (prolog-session-prefix session) "")
    (editor-error "Cannot remove Prolog interactions because the prefix is empty"))
  (let* ((buffer (current-buffer))
         (region-p (buffer-mark-p buffer))
         (start (if region-p (region-beginning buffer) (buffer-start-point buffer)))
         (end (if region-p (region-end buffer) (buffer-end-point buffer)))
         (regex (format nil "^[\t ]*~a"
                        (ppcre:quote-meta-chars (prolog-session-prefix session))))
         (lines '()))
    ;; Collect line numbers before editing; deleting from bottom to top keeps
    ;; the remaining line numbers stable.
    (with-point ((p start)
                 (limit end))
      (when (and region-p (plusp (point-charpos p)))
        (line-offset p 1))
      (loop :while (point< p limit)
            :do (progn
                  (when (ppcre:scan regex (line-string p))
                    (with-point ((line-start-point (line-start (copy-point p)))
                                 (line-end-point (line-end (copy-point p)))
                                 (after-line (copy-point p)))
                      (let ((has-next (line-offset after-line 1)))
                        (when (or (not region-p)
                                  (and (point<= start line-start-point)
                                       (point<= line-end-point limit)))
                          (push (list (line-number-at-point p)
                                      (and has-next
                                           (or (not region-p)
                                               (point<= after-line limit))))
                                lines)))))
                  (unless (line-offset p 1)
                    (return)))))
    (dolist (item lines)
      (destructuring-bind (line-number include-newline-p) item
        (with-point ((p (buffer-start-point buffer)))
          (when (move-to-line p line-number)
            (with-point ((line-start-point (line-start (copy-point p)))
                         (line-end-point (line-end (copy-point p)))
                         (after-line (copy-point p)))
              (cond ((and include-newline-p (line-offset after-line 1))
                     (delete-between-points line-start-point after-line))
                    ((and (not region-p) (not (line-offset after-line 1)))
                     (let ((previous-line (copy-point p :temporary)))
                       (if (line-offset previous-line -1)
                           (delete-between-points (line-end previous-line)
                                                  line-end-point)
                           (delete-between-points line-start-point line-end-point))))
                    (t
                     (delete-between-points line-start-point line-end-point))))))))
    (message "Interactions removed.")))

(defun prolog-kill-buffer-session (buffer)
  "Kill the localized Prolog process when BUFFER is killed."
  (let ((session (buffer-value buffer 'prolog-session)))
    (when session
      (prolog-kill-process session))))

(defun prolog-localize-buffer (buffer)
  "Give BUFFER a private Prolog session and detach it from the shared session."
  (let ((shared-session *default-prolog-session*))
    (when (and (prolog-running-p shared-session)
               (not (prolog-session-seen-prompt shared-session)))
      (editor-error "Cannot localize while the shared Prolog query is in progress"))
    (when (eq (prolog-session-process-buffer shared-session) buffer)
      (let ((marker (prolog-session-marker shared-session)))
        (when (and marker (alive-point-p marker))
          (delete-point marker)))
      (setf (prolog-session-process-buffer shared-session) nil
            (prolog-session-marker shared-session) nil))
    (let ((session (make-prolog-session :buffer buffer)))
      (setf (buffer-value buffer 'prolog-session) session)
      (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
                #'prolog-kill-buffer-session)
      session)))

(defun %prolog-dwim (session arg)
  "Dispatch on the prefix argument ARG of `prolog-dwim' for SESSION."
  (case arg
    ((nil)
     (unless (prolog-query session)
       (%prolog-consult session nil)))
    (0
     (unless (prolog-running-p session)
       (editor-error "No Prolog process running"))
     (prolog-kill-process session)
     (message "Prolog process killed."))
    (1 (%prolog-consult session nil))
    (2 (%prolog-consult session t))
    (7
     (unless (prolog-more-solutions-p session)
       (editor-error "No query in progress"))
     (%prolog-toplevel session))
    (4 (%prolog-consult session nil) (prolog-query session))
    (16 (%prolog-consult session t) (prolog-query session))
    (otherwise (%prolog-remove-interactions session))))

(define-command prolog-dwim (arg) (:universal-nil)
  "Load current buffer into Prolog or post query (Do What I Mean).

If invoked on a line starting with `:-' or `?-' (possibly preceded by
`%' and whitespace), send the query to the Prolog process and interact
as on a terminal. Otherwise, consult the buffer.

With prefix argument 0, kill the Prolog process. With prefix 1, always
consult the buffer. With prefix 2, consult the buffer with a new
process. With prefix 7, equivalent to `prolog-toplevel'. With just
C-u, first consult the buffer and then, if point is on a query,
evaluate it. Analogously, C-u C-u for consult with a new process.
With other prefix arguments, remove all interactions."
  (%prolog-dwim (prolog-session-for-buffer) arg))

(define-command prolog-toplevel () ()
  "Start or resume Prolog toplevel interaction in the buffer."
  (%prolog-toplevel (prolog-session-for-buffer)))

(define-command prolog-kill-prolog () ()
  "Kill the Prolog process."
  (unless (prolog-running-p (prolog-session-for-buffer))
    (editor-error "No Prolog process running"))
  (prolog-kill-process (prolog-session-for-buffer)))

(define-command prolog-remove-interactions () ()
  "Remove all lines starting with the prefix of the latest query from the buffer."
  (%prolog-remove-interactions (prolog-session-for-buffer)))

(define-command prolog-consult (&optional new-process) ()
  "Load current buffer (or region, if active) into the Prolog process.
With NEW-PROCESS non-nil, start a new process. In case of errors, point
is moved to the line of the first error."
  (%prolog-consult (prolog-session-for-buffer) new-process))

(defun prolog-clear-local-variable (buffer variable)
  "Remove VARIABLE's buffer-local value from BUFFER."
  (let ((editor-variable (get variable 'lem/common/var:editor-variable)))
    (when editor-variable
      (buffer-unbound
       buffer
       (lem/common/var:editor-variable-local-indicator editor-variable)))))

(define-command prolog-localize () ()
  "Give the current buffer its own Prolog process and interaction state.
Other buffers continue using the shared default Prolog session."
  (let ((buffer (current-buffer)))
    (when (buffer-value buffer 'prolog-session)
      (editor-error "This buffer already has a local Prolog session"))
    (prolog-localize-buffer buffer)
    (dolist (variable '(prolog-program
                        prolog-program-switches
                        prolog-default-prefix
                        prolog-max-history))
      (setf (variable-value variable :buffer buffer)
            (variable-value variable :default buffer)))
    (message "Prolog session localized to this buffer.")))

(define-command prolog-unlocalize () ()
  "Discard this buffer's local Prolog process and return to the shared session."
  (let* ((buffer (current-buffer))
         (session (buffer-value buffer 'prolog-session)))
    (if session
        (progn
          (prolog-kill-process session)
          (remove-hook (variable-value 'kill-buffer-hook :buffer buffer)
                       #'prolog-kill-buffer-session)
          (setf (buffer-value buffer 'prolog-session) nil)
          (dolist (variable '(prolog-program
                              prolog-program-switches
                              prolog-default-prefix
                              prolog-max-history))
            (prolog-clear-local-variable buffer variable))
          (message "Using the shared Prolog session again."))
        (message "This buffer already uses the shared Prolog session."))))
