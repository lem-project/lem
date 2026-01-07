(in-package :lem-core)

(define-editor-variable escape-as-meta-prefix nil
  "When non-nil, treat Escape key as Meta prefix without timeout.
Pressing Escape followed by another key produces Meta+key.
Escape Escape produces a single Escape.")

(defvar *input-hook* '())

(defvar *key-recording-p* nil)
(defvar *record-keys* nil)
(defvar *unread-keys* nil)

(let (last-read-key-sequence)
  (defun last-read-key-sequence ()
    last-read-key-sequence)
  (defun set-last-read-key-sequence (key-sequence)
    (setf last-read-key-sequence key-sequence)))

(let ((key-recording-status-name " Def"))
  (defun start-record-key ()
    (modeline-add-status-list key-recording-status-name)
    (setq *key-recording-p* t)
    (setq *record-keys* nil))
  (defun stop-record-key ()
    (when *key-recording-p*
      (modeline-remove-status-list key-recording-status-name)
      (setq *key-recording-p* nil)
      (nreverse *record-keys*))))

(defun key-recording-p ()
  *key-recording-p*)

(defun read-event-internal (&key (accept-key t) (accept-mouse t))
  (flet ((accept-event-p (event)
           (or (and accept-key (key-p event))
               (and accept-mouse (mouse-event-p event)))))
    (loop :for ms := (get-next-timer-timing-ms)
          :do (cond ((null ms)
                     (loop
                       (let ((event (receive-event nil)))
                         (when (accept-event-p event)
                           (return-from read-event-internal event)))))
                    ((<= ms 0)
                     (handler-bind ((timer-error
                                      (lambda (err)
                                        (show-message (princ-to-string err)))))
                       (update-idle-timers))
                     (redraw-display))
                    (t
                     (let ((event (receive-event (float (/ ms 1000)))))
                       (when (accept-event-p event)
                         (return event))))))))

(defun read-event-with-recording-and-run-hooks (&key accept-key accept-mouse)
  (let ((event (if (null *unread-keys*)
                   (read-event-internal :accept-key accept-key :accept-mouse accept-mouse)
                   (pop *unread-keys*))))
    (when (key-p event)
      (if *key-recording-p*
          (push event *record-keys*)
          (run-hooks *input-hook* event)))
    event))

(defun escape-key-p (key)
  "Return T if KEY is an unmodified Escape key."
  (and (key-p key)
       (match-key key :sym "Escape")))

(defun add-meta-modifier (key)
  "Return a new key with Meta modifier added."
  (make-key :ctrl (key-ctrl key)
            :meta t
            :super (key-super key)
            :hyper (key-hyper key)
            :shift (key-shift key)
            :sym (key-sym key)))

(defun maybe-convert-escape-to-meta (event read-next-fn)
  "If EVENT is Escape and escape-as-meta-prefix is enabled,
read the next key and add Meta modifier.
Escape Escape produces a single Escape."
  (if (and (variable-value 'escape-as-meta-prefix :global)
           (escape-key-p event))
      (let ((next-key (funcall read-next-fn)))
        (if (escape-key-p next-key)
            ;; Escape Escape -> Escape
            next-key
            ;; Escape + key -> M-key
            (add-meta-modifier next-key)))
      event))

(defun read-event ()
  (let ((event (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse t)))
    (maybe-convert-escape-to-meta
     event
     (lambda () (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))))

(defun read-key ()
  (let ((event (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))
    (maybe-convert-escape-to-meta
     event
     (lambda () (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))))

(defun unread-key (key)
  (when *key-recording-p*
    (pop *record-keys*))
  (pop *this-command-keys*)
  (push key *unread-keys*))

(defun read-command ()
  (let ((event (read-event)))
    (etypecase event
      (mouse-event
       (set-last-mouse-event event)
       (find-mouse-command event))
      (key
       (let* ((cmd (lookup-keybind event))
              (kseq (list event)))
         (loop
           (cond ((prefix-command-p cmd)
                  (let ((event (read-key)))
                    (setf kseq (nconc kseq (list event)))
                    (setf cmd (lookup-keybind kseq))))
                 (t
                  (set-last-read-key-sequence kseq)
                  (return cmd)))))))))

(defun read-key-sequence ()
  (read-command)
  (last-read-key-sequence))

(defun unread-key-sequence (kseq)
  (prog1 (setf *unread-keys* (nconc kseq *unread-keys*))
    (setf *this-command-keys*
          (nthcdr (length kseq) *this-command-keys*))))

(defun execute-key-sequence (key-sequence)
  (let ((*unread-keys* key-sequence))
    (do-command-loop (:interactive nil)
      (when (null *unread-keys*)
        (return))
      (let ((*this-command-keys* nil))
        (call-command (read-command) nil)))))

(defun sit-for (seconds &optional (update-window-p t) (force-update-p nil))
  (when update-window-p (redraw-display :force force-update-p))
  (let ((e (receive-event seconds)))
    (cond ((null e) :timeout)
          ((abort-key-p e) (error 'editor-abort))
          ((key-p e) (unread-key e) e)
          (t nil))))
