(in-package :lem)

(export '(*last-read-key-sequence*
          start-record-key
          stop-record-key
          key-recording-p
          read-key
          unread-key
          read-key-sequence
          unread-key-sequence
          execute-key-sequence
          sit-for))

(defvar *last-read-key-sequence*)

(defvar *key-recording-p* nil)
(defvar *temp-macro-chars* nil)

(defvar *unread-keys* nil)

(defun start-record-key ()
  (setq *key-recording-p* t)
  (setq *temp-macro-chars* nil))

(defun stop-record-key ()
  (setq *key-recording-p* nil)
  (nreverse *temp-macro-chars*))

(defun key-recording-p ()
  *key-recording-p*)

(defun read-key ()
  (let ((char (if (null *unread-keys*)
                  (get-char nil)
                  (pop *unread-keys*))))
    (when *key-recording-p*
      (push char *temp-macro-chars*))
    char))

(defun unread-key (char)
  (when *key-recording-p*
    (pop *temp-macro-chars*))
  (push char *unread-keys*))

(defun unread-key-sequence (key)
  (setf *unread-keys*
        (nconc *unread-keys*
               (if (listp key) key (kbd-list key))))) ;!!!

(defun read-key-sequence ()
  (let ((key
         (let ((c (read-key)))
           (if (or (char= c C-x)
                   (char= c escape))
               (let ((c2 (read-key)))
                 (if (char= c2 escape)
                     (kbd c c2 (read-key))
                     (kbd c c2)))
               (kbd c)))))
    (setq *last-read-key-sequence* key)))

(defun execute-key-sequence (key-sequence)
  (let ((prev-unread-keys-length (length *unread-keys*))
        (prev-unread-keys (copy-list *unread-keys*)))
    (unread-key-sequence key-sequence)
    (do ()
        ((>= prev-unread-keys-length
             (length *unread-keys*))
         t)
      (handler-case (funcall (find-keybind (read-key-sequence)) nil)
        (editor-condition ()
          (setf *unread-keys* prev-unread-keys)
          (return nil))))))

(defun sit-for (seconds &optional (update-window-p t))
  (when update-window-p (redraw-display))
  (multiple-value-bind (char timeout-p)
      (get-char (floor (* seconds 1000)))
    (cond (timeout-p t)
          ((char= char C-g) (error 'editor-abort))
          (t (unread-key char)
             nil))))
