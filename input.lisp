(in-package :lem)

(export '(*last-read-key-sequence*
          macro-running-p
          read-key
          unread-key
          begin-macro
          end-macro
          execute-macro
          apply-macro-to-region-lines
          read-key-sequence
          unread-key-sequence
          sit-for))

(defvar *last-read-key-sequence*)

(defvar *macro-recording-p* nil)
(defvar *macro-chars* nil)
(defvar *macro-running-p* nil)

(defun macro-running-p () *macro-running-p*)

(defvar *unread-keys* nil)

(defun read-key ()
  (let ((char (if (null *unread-keys*)
                  (get-char nil)
                  (pop *unread-keys*))))
    (when *macro-recording-p*
      (push char *macro-chars*))
    char))

(defun unread-key (char)
  (when *macro-recording-p*
    (pop *macro-chars*))
  (push char *unread-keys*))

(defun input-enqueue (c)
  (check-type c character)
  (setf *unread-keys*
        (nconc *unread-keys*
               (list c))))

(defun unread-key-sequence (key)
  (mapc 'input-enqueue (kbd-list key)))

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

(define-key *global-keymap* (kbd "C-x (") 'begin-macro)
(define-command begin-macro () ()
  (cond (*macro-recording-p*
         (message "Macro already active")
         nil)
        (t
         (message "Start macro")
         (setq *macro-recording-p* t)
         (setq *macro-chars* nil)
         t)))

(define-key *global-keymap* (kbd "C-x )") 'end-macro)
(define-command end-macro () ()
  (cond (*macro-running-p* t)
        ((not *macro-recording-p*)
         (message "Macro not active"))
        (t
         (setq *macro-recording-p* nil)
         (message "End macro")
         t)))

(define-key *global-keymap* (kbd "C-x e") 'execute-macro)
(define-command execute-macro (n) ("p")
  (cond (*macro-recording-p*
         (message "Macro already active")
         nil)
        (*macro-running-p*
         nil)
        (t
         (let ((*macro-running-p* t))
           (loop
             :named outer
             :repeat n
             :while *macro-running-p*
             :do (let ((length (length *unread-keys*)))
                   (mapc 'input-enqueue (reverse *macro-chars*))
                   (loop :while (< length (length *unread-keys*)) :do
                     (unless *macro-running-p*
                       (loop :while (< length (length *unread-keys*))
                             :do (read-key))
                       (return-from outer nil))
                     (handler-case (funcall (find-keybind (read-key-sequence)) nil)
                       (editor-condition ()
                         (setf *unread-keys* nil)
                         (return-from outer nil)))))
             :finally (return-from outer t))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      #'(lambda ()
                          (execute-macro 1)))
  t)

(defun sit-for (seconds &optional (update-window-p t))
  (when update-window-p (redraw-display))
  (multiple-value-bind (char timeout-p)
      (get-char (floor (* seconds 1000)))
    (cond (timeout-p t)
          ((char= char C-g) (error 'editor-abort))
          (t (unread-key char)
             nil))))
