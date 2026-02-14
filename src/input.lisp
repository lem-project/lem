(in-package :lem-core)

(define-editor-variable meta-prefix-keys nil
  "Keys to treat as Meta prefix without timeout.
When non-nil, pressing a prefix key followed by another key produces Meta+key.
Pressing the prefix key twice produces the prefix key itself.

Value can be:
- nil: disabled
- t: use Escape as Meta prefix (convenient shorthand)
- a key object: use that key as Meta prefix
- a list of keys: use any of those keys as Meta prefix

Examples:
  t                           ; Escape as Meta prefix
  (lem:make-key :sym \"Escape\") ; same as t
  (list (lem:make-key :sym \"Escape\")
        (lem:make-key :ctrl t :sym \"[\"))  ; both Escape and C-[ as Meta prefix")

(defvar *input-hook* '())

(defvar *key-recording-p* nil)
(defvar *record-keys* nil)
(defvar *unread-keys* nil)

(let (last-read-key-sequence)
  (defun last-read-key-sequence ()
    last-read-key-sequence)
  (defun set-last-read-key-sequence (key-sequence)
    (setf last-read-key-sequence key-sequence)))

(defmacro with-last-read-key-sequence (&body body)
  "execute BODY with `last-read-key-sequence' temporarily set to NIL, preserving its original value."
  (alexandria:with-gensyms (old-value)
    `(let ((,old-value (last-read-key-sequence)))
       (set-last-read-key-sequence nil)
       (unwind-protect (progn ,@body)
         (set-last-read-key-sequence ,old-value)))))

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

(defun keys-equal-p (key1 key2)
  "Return T if KEY1 and KEY2 are the same key."
  (and (key-p key1)
       (key-p key2)
       (equal (key-ctrl key1) (key-ctrl key2))
       (equal (key-meta key1) (key-meta key2))
       (equal (key-super key1) (key-super key2))
       (equal (key-hyper key1) (key-hyper key2))
       (equal (key-shift key1) (key-shift key2))
       (equal (key-sym key1) (key-sym key2))))

(defun meta-prefix-key-p (key prefix-keys)
  "Return T if KEY matches any of the PREFIX-KEYS.
PREFIX-KEYS can be t (Escape), a single key, or a list of keys."
  (and (key-p key)
       (cond ((eq prefix-keys t)
              (match-key key :sym "Escape"))
             ((key-p prefix-keys)
              (keys-equal-p key prefix-keys))
             ((listp prefix-keys)
              (some (lambda (pk) (keys-equal-p key pk)) prefix-keys))
             (t nil))))

(defun add-meta-modifier (key)
  "Return a new key with Meta modifier added."
  (make-key :ctrl (key-ctrl key)
            :meta t
            :super (key-super key)
            :hyper (key-hyper key)
            :shift (key-shift key)
            :sym (key-sym key)))

(defun maybe-convert-to-meta (event read-next-fn)
  "If EVENT is a meta-prefix key and meta-prefix-keys is set,
read the next key and add Meta modifier.
Pressing the same prefix key twice produces that key."
  (let ((prefix-keys (variable-value 'meta-prefix-keys :global)))
    (if (and prefix-keys
             (meta-prefix-key-p event prefix-keys))
        (let ((next-key (funcall read-next-fn)))
          (if (keys-equal-p next-key event)
              ;; prefix-key prefix-key -> prefix-key
              next-key
              ;; prefix-key + key -> M-key
              (add-meta-modifier next-key)))
        event)))

(defun read-event ()
  (let ((event (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse t)))
    (maybe-convert-to-meta
     event
     (lambda () (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))))

(defun read-key ()
  (let ((event (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))
    (maybe-convert-to-meta
     event
     (lambda () (read-event-with-recording-and-run-hooks :accept-key t :accept-mouse nil)))))

(defun unread-key (key)
  (when *key-recording-p*
    (pop *record-keys*))
  (pop *this-command-keys*)
  (push key *unread-keys*))

(defun count-intermediate-keys (keymap kseq)
  "count how many keys in KSEQ traversed through intermediate prefixes."
  (let ((count 0))
    (labels ((walk (binding keys)
               (when keys
                 (let ((matches (find-matching-prefixes binding (car keys))))
                   (dolist (match matches)
                     (when (prefix-intermediate-p match)
                       (incf count))
                     (walk (prefix-suffix match) (cdr keys)))))))
      (walk keymap kseq))
    count))

(defun read-command ()
  (let ((event (read-event)))
    (etypecase event
      (mouse-event
       (set-last-mouse-event event)
       (find-mouse-command event))
      (key
       (let ((result)
             (prefix)
             (suffix)
             (behavior)
             (kseq (list event)))
         (labels ((reset ()
                    (setf result (lookup-keybind kseq))
                    (setf suffix (car result))
                    (setf prefix (cdr result))
                    (when prefix
                      (setf behavior (prefix-behavior prefix)))))
           (loop
             (reset)
             (when prefix
               (prefix-invoke prefix))
             ;; if suffix was a function we call it and set to NIL so that we dont return it
             (when (functionp suffix)
               (funcall suffix)
               (setf suffix nil))
             (cond ((prefix-command-p suffix)
                    (when (typep suffix 'keymap)
                      (keymap-activate suffix))
                    (let ((event (read-key)))
                      (setf kseq (nconc kseq (list event)))
                      (reset)))
                   (t
                    (cond
                      ;; note: menu in these comments might mean keymaps, i used menu because
                      ;; this is mostly intended for transient keymaps (i.e. key menus).
                      ;; :drop removes the current key from kseq without changing "menus".
                      ;; used for "infix" keys (toggles, choices) that act in-place.
                      ;; also pops any intermediate prefix keys so the recorded
                      ;; sequence reflects only the menu-level key that was pressed.
                      ((eq behavior :drop)
                       (setf kseq (butlast kseq))
                       (dotimes (_ (count-intermediate-keys *root-keymap* kseq))
                         (setf kseq (butlast kseq)))
                       (set-last-read-key-sequence kseq)
                       (reset))
                      ;; :back removes the current key and the key that entered
                      ;; the current menu, navigating up one menu level.
                      ;; also pops any intermediate prefix keys in between.
                      ((eq behavior :back)
                       (setf kseq (butlast kseq))
                       (dotimes (_ (count-intermediate-keys *root-keymap* kseq))
                         (setf kseq (butlast kseq)))
                       ;; pop the key that entered the current "menu"
                       (setf kseq (butlast kseq))
                       (set-last-read-key-sequence kseq)
                       (reset))
                      ((eq behavior :cancel)
                       (setf kseq nil)
                       (set-last-read-key-sequence nil)
                       (keymap-activate *root-keymap*)
                       (return nil))
                      (t
                       (set-last-read-key-sequence kseq)
                       (keymap-activate *root-keymap*)
                       (return suffix))))))))))))

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
      (let* ((*this-command-keys* nil)
             (cmd (read-command)))
        (call-command cmd nil)))))

(defun sit-for (seconds &optional (update-window-p t) (force-update-p nil))
  (when update-window-p (redraw-display :force force-update-p))
  (let ((e (receive-event seconds)))
    (cond ((null e) :timeout)
          ((abort-key-p e) (error 'editor-abort))
          ((key-p e) (unread-key e) e)
          (t nil))))
