(in-package :lem)

(export '(last-read-key-sequence
          start-record-key
          stop-record-key
          key-recording-p
          read-event
          read-key
          unread-key
          read-command
          read-key-sequence
          unread-key-sequence
          execute-key-sequence
          sit-for))

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

(defun read-event (&optional timeout)
  (receive-event timeout))

(defun read-key-1 ()
  (loop
    (let ((ms (shortest-wait-timers)))
      (if (null ms)
          (loop :for obj := (read-event nil)
                :do (when (key-p obj)
                      (return-from read-key-1 obj)))
          (if (minusp ms)
              (update-timer)
              (let ((e (read-event (float (/ ms 1000)))))
                (if (key-p e)
                    (return e)
                    (update-timer))))))))

(defun read-key ()
  (let ((key (if (null *unread-keys*)
                 (read-key-1)
                 (pop *unread-keys*))))
    (when *key-recording-p*
      (push key *record-keys*))
    key))

(defun unread-key (key)
  (when *key-recording-p*
    (pop *record-keys*))
  (push key *unread-keys*))

(defun read-command ()
  (let* ((key (read-key))
         (cmd (lookup-keybind key))
         (kseq (list key)))
    (loop
      (cond ((hash-table-p cmd)
             (let ((key (read-key)))
               (setf kseq (nconc kseq (list key)))
               (setf cmd (lookup-keybind kseq))))
            (t
             (set-last-read-key-sequence kseq)
             (return cmd))))))

(defun read-key-sequence ()
  (read-command)
  (last-read-key-sequence))

(defun unread-key-sequence (kseq)
  (setf *unread-keys* (nconc *unread-keys* kseq)))

(defun execute-key-sequence (key-sequence)
  (handler-case
      (let ((*unread-keys* key-sequence))
        (do-command-loop (:interactive nil)
          (when (null *unread-keys*)
            (return-from execute-key-sequence t))
          (call-command (read-command) nil)))
    (editor-condition ())))

(defun sit-for (seconds &optional (update-window-p t))
  (when update-window-p (redraw-display))
  (let ((e (read-event seconds)))
    (cond ((null e) t)
          ((abort-key-p e) (error 'editor-abort))
          ((key-p e) (unread-key e))
          (t nil))))
