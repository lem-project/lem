(in-package :lem)

(export '(last-read-key-sequence
          start-record-key
          stop-record-key
          key-recording-p
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
    (setf last-read-key-sequence
          (if (kbd-p key-sequence)
              key-sequence
              (make-kbd key-sequence)))))

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

(defun read-key-1 ()
  (loop
    (let ((ms (shortest-wait-timers)))
      (if (null ms)
          (loop :for obj := (read-event nil)
                :do (when (characterp obj)
                      (return-from read-key-1 obj)))
          (if (minusp ms)
              (update-timer)
              (let ((e (read-event (float (/ ms 1000)))))
                (if (characterp e)
                    (return e)
                    (update-timer))))))))

(defun read-key ()
  (let ((kev (if (null *unread-keys*)
                 (read-key-1)
                 (pop *unread-keys*))))
    (when *key-recording-p*
      (push kev *record-keys*))
    kev))

(defun unread-key (key)
  (when *key-recording-p*
    (pop *record-keys*))
  (push key *unread-keys*))

(defun read-command ()
  (let* ((c (read-key))
         (cmd (lookup-keybind c))
         (list (list c)))
    (loop
      (cond ((hash-table-p cmd)
             (let ((c (read-key)))
               (setf list (nconc list (list c)))
               (setf cmd (lookup-keybind list))))
            (t
             (set-last-read-key-sequence list)
             (return cmd))))))

(defun read-key-sequence ()
  (read-command)
  (last-read-key-sequence))

(defun unread-key-sequence (key)
  (setf *unread-keys*
        (nconc *unread-keys*
               (if (listp key) key (kbd-list key)) ;!!!
               )))

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
          ((characterp e) (unread-key e))
          (t nil))))
