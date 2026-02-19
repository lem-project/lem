(in-package :lem-core)

(defclass prefix ()
  ((key
    :initarg :key
    :documentation "the key defined for the prefix. could be a function that returns a key.")
   (description
    :initarg :description
    :initform nil)
   (suffix
    :initarg :suffix
    :documentation "the suffix defined for the prefix, could be another prefix or a keymap or a function that returns one.")
   (active-p
    :initarg :active-p
    :documentation "whether a prefix is active."
    :initform t)
   ;; intermediate-p means a prefix is just a "continuation" of another and servers as an intermediate key
   (intermediate-p
    :initarg :intermediate-p
    :documentation "whether a prefix is an intermediary to another, this effects the :drop and :back behavior."
    :initform nil)
   (behavior
    :initarg :behavior
    :initform nil
    :documentation "should be one of `:drop', `:back', `:cancel', or NIL to decide the effect of the suffix on the key sequence.

:cancel to drop the current key sequence entirely without invoking a command.
:drop to avoid adding the current key to the key sequence, which makes the prefix act as an \"infix\" key.
:back to avoid adding the current key and to pop the last recorded key which has the effect of \"going back\" to parent menu in the transient popup.
NIL to append it to the key sequence normally.")
   (properties
    :initarg :properties
    :accessor prefix-properties
    :initform nil
    :documentation "extra metadata that a prefix may hold.")))

(defgeneric prefix-key (prefix)
  (:method ((prefix prefix))
   (slot-value prefix 'key)))

(defgeneric (setf prefix-key) (new-value prefix)
  (:method (new-value (prefix prefix))
    (setf (slot-value prefix 'key) new-value)))

(defgeneric prefix-suffix (prefix)
  (:method ((prefix prefix))
    (slot-value prefix 'suffix)))

(defgeneric (setf prefix-suffix) (new-value prefix)
  (:method (new-value (prefix prefix))
    (setf (slot-value prefix 'suffix) new-value)))

(defgeneric prefix-description (prefix)
  (:method ((prefix prefix))
    (slot-value prefix 'description)))

(defgeneric (setf prefix-description) (new-value prefix)
  (:method (new-value (prefix prefix))
    (setf (slot-value prefix 'description) new-value)))

(defgeneric prefix-active-p (prefix)
  (:method ((prefix prefix))
    (slot-value prefix 'active-p)))

(defgeneric (setf prefix-active-p) (new-value prefix)
  (:method (new-value (prefix prefix))
    (setf (slot-value prefix 'active-p) new-value)))

(defun make-prefix (&key key suffix description)
  (let ((prefix (make-instance
                 'prefix
                 :key key
                 :suffix suffix
                 :description description)))
    prefix))

(defclass keymap ()
  ((prefixes
    :initarg :prefixes
    :initform nil
    :documentation "prefix bindings owned by this keymap.")
   (children
    :initarg :children
    :initform nil
    :documentation "child keymaps.")
   (properties
    :initarg :properties
    :accessor keymap-properties
    :initform nil
    :documentation "additional metadata that a keymap holds.")
   (description
    :initarg :description
    :initform nil)
   (active-p
    :initarg :active-p
    :documentation "whether a prefix is active."
    :initform t)
   (base
    :initarg :base
    :accessor keymap-base
    :initform nil
    :documentation "the keymap that this keymap extends.")))

(defgeneric keymap-prefixes (keymap)
  (:method ((keymap keymap))
    (slot-value keymap 'prefixes)))

(defgeneric (setf keymap-prefixes) (new-value keymap)
  (:method (new-value (keymap keymap))
    (setf (slot-value keymap 'prefixes) new-value)))

(defgeneric keymap-children (keymap)
  (:method ((keymap keymap))
    (slot-value keymap 'children)))

(defgeneric (setf keymap-children) (new-value keymap)
  (:method (new-value (keymap keymap))
    (setf (slot-value keymap 'children) new-value)))

(defgeneric keymap-description (keymap)
  (:method ((keymap keymap))
    (slot-value keymap 'description)))

(defgeneric (setf keymap-description) (new-value keymap)
  (:method (new-value (keymap keymap))
    (setf (slot-value keymap 'description) new-value)))

(defgeneric keymap-active-p (keymap)
  (:method ((keymap keymap))
    (slot-value keymap 'active-p)))

(defgeneric (setf keymap-active-p) (new-value keymap)
  (:method (new-value (keymap keymap))
    (setf (slot-value keymap 'active-p) new-value)))

(defmethod keymap-add-prefix ((keymap keymap) (prefix prefix) &optional after)
  (unless (find prefix (keymap-prefixes keymap))
    (if after
        (setf (keymap-prefixes keymap) (append (slot-value keymap 'prefixes) (list prefix)))
        (push prefix (slot-value keymap 'prefixes)))))

(defmethod keymap-add-child ((keymap keymap) (keymap2 keymap) &optional after)
  (unless (find keymap2 (keymap-children keymap))
    (if after
        (setf (keymap-children keymap) (append (slot-value keymap 'children) (list keymap2)))
        (push keymap2 (slot-value keymap 'children)))))

(defgeneric prefix-p (keymap)
  (:documentation "check whether this is a prefix of another prefix.

a prefix is a prefix of another if its a keymap or if its suffix is a prefix."))

(defmethod prefix-p ((km keymap))
  t)

(defmethod prefix-p ((p prefix))
  (or (typep (prefix-suffix p) 'prefix)
      (typep (prefix-suffix p) 'keymap)))

(defmethod (setf prefix-behavior) (new-value (prefix prefix))
  (setf (slot-value prefix 'behavior) new-value))

(defmethod prefix-behavior ((prefix prefix))
  (slot-value prefix 'behavior))

(defmethod (setf prefix-intermediate-p) (new-value (prefix prefix))
  (setf (slot-value prefix 'intermediate-p) new-value))

(defmethod prefix-intermediate-p ((prefix prefix))
  (slot-value prefix 'intermediate-p))

(defgeneric keymap-activate (keymap)
  (:documentation "a hook for when a keymap is entered by some prefix.")
  ;; default keymap-activate does nothing
  (:method ((keymap t))
    nil))

(defgeneric prefix-invoke (prefix)
  (:documentation "a hook for when a prefix is reached.")
  (:method ((prefix t)) nil))

(defun find-prefix-matches (keymap key &key active-only)
  (loop for item in (keymap-prefixes keymap)
        when (and (equal (prefix-key item) key)
                  (or (not active-only)
                      (prefix-active-p item)))
          collect item))

(defun first-prefix-match (keymap key &key active-only)
  (loop for item in (keymap-prefixes keymap)
        when (and (equal (prefix-key item) key)
                  (or (not active-only)
                      (prefix-active-p item)))
          return item))

(defun search-with-base (keymap fn)
  (or (funcall fn keymap)
      (when (typep keymap 'keymap)
        (let ((base (keymap-base keymap)))
          (when base
            (search-with-base base fn))))))

(deftype key-sequence ()
  '(trivial-types:proper-list key))

(defun keyseq-to-string (key-sequence)
  (check-type key-sequence key-sequence)
  (format nil "~{~A~^ ~}" key-sequence))

;; this is for backwards compatibility for now
(defclass keymap* (keymap)
  ((undef-hook
    :initarg :undef-hook
    :accessor keymap-undef-hook
    :initform nil)
   (function-table
    :initarg :function-table
    :accessor keymap-function-table
    :initform (make-hash-table :test 'eq))))

;; *root-keymap* contains the full keymap hierarchy
(defvar *root-keymap* (make-instance 'keymap))

(defvar *special-keymap* nil)

(defmethod print-object ((object keymap) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-description object)
      (princ (keymap-description object) stream))))

(defun make-keymap (&key undef-hook prefixes children description base)
  (let ((keymap (make-instance 'keymap*
                               :undef-hook undef-hook
                               :prefixes prefixes
                               :children children
                               :description description
                               :base base)))
    keymap))

(defun prefix-command-p (command)
  (and (or (typep command 'keymap)
           (typep command 'prefix))
       (prefix-p command)))

(defmethod define-key ((keymap keymap) keyspec command-name)
  "Bind a command COMMAND-NAME to a KEYSPEC in a KEYMAP.

Global bindings use `*global-keymap*' as KEYMAP argument.

If KEYSPEC argument is a `string', valid prefixes are:
H (Hyper), S (Super), M (Meta), C (Ctrl), Shift

Example: (define-key *global-keymap* \"C-'\" 'list-modes)"
  (check-type keyspec (or symbol string))
  (check-type command-name (or symbol keymap))
  (typecase keyspec
    (symbol
     (setf (gethash keyspec (keymap-function-table keymap))
           command-name))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (define-key-internal keymap keys command-name))))
  (values))

(defmacro define-keys (keymap &body bindings)
  `(progn ,@(mapcar
             (lambda (binding)
               `(define-key ,keymap
                    ,(first binding)
                  ,(second binding)))
             bindings)))

(defun prefix-for-key (keymap key)
  "find a prefix matching KEY in KEYMAP, searching child keymaps recursively."
  (or (first-prefix-match keymap key)
      (loop for child in (keymap-children keymap)
            thereis (prefix-for-key child key))))

(defmethod define-key-internal ((keymap keymap) keys symbol)
  (let* ((rest (uiop:ensure-list keys))
         (k (car rest)))
    (if (null (cdr rest))
        ;; if theres no more keys in the sequence we simply bind the last key.
        (let ((prefix (prefix-for-key keymap k)))
          (if prefix
              (setf (prefix-suffix prefix) symbol)
              ;; if we didnt find a pre-existing prefix we insert one
              (keymap-add-prefix keymap (make-prefix :key k :suffix symbol))))
        ;; here we're creating intermediate keymaps to bind the keys in the sequence
        ;; one by one. which is the way emacs does it, and the way lem used to it.
        ;; but it should be possible to completely bind the sequence to prefixes that
        ;; lead to one another.
        (let* ((next-prefix (prefix-for-key keymap k))
               (next-keymap))
          ;; we expect the suffix of next-prefix to be a keymap, if next-prefix isnt yet
          ;; existent we create a prefixed keymap and work with it.
          (if next-prefix
              (let ((suffix (prefix-suffix next-prefix)))
                (if (typep suffix 'keymap)
                    (setf next-keymap suffix)
                    ;; suffix is a command, need to create intermediate keymap. but why would we get here?
                    (progn
                      (setf next-keymap (make-instance 'keymap))
                      (setf (prefix-suffix next-prefix) next-keymap))))
              (progn
                (setf next-keymap (make-instance 'keymap))
                (setf next-prefix
                      (make-prefix :suffix next-keymap
                                   :key k))
                (keymap-add-prefix keymap next-prefix)))
          (define-key-internal next-keymap (cdr rest) symbol)))))

(defun undefine-key (keymap keyspec)
  "Remove a binding for a KEYSPEC in a KEYMAP.

If KEYSPEC argument is a `string', valid prefixes are:
H (Hyper), S (Super), M (Meta), C (Ctrl), Shift

Example: (undefine-key *paredit-mode-keymap* \"C-k\")"
  (check-type keyspec (or symbol string))
  (typecase keyspec
    (symbol
     (remhash keyspec (keymap-function-table keymap)))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (undefine-key-internal keymap keys))))
  (values))

(defmacro undefine-keys (keymap &body bindings)
  `(progn ,@(mapcar
             (lambda (binding)
               `(undefine-key ,keymap
                              ,(first binding)))
             bindings)))

(defun undefine-key-internal (keymap keys)
  (labels ((search-tree (binding keys-to-find)
             (when (and keys-to-find (typep binding 'keymap))
               (let ((matches (find-prefix-matches binding (car keys-to-find) :active-only t)))
                 (loop for match in matches
                       for suffix = (prefix-suffix match)
                       do (if (cdr keys-to-find)
                              (search-tree suffix (cdr keys-to-find))
                              (setf (keymap-prefixes binding)
                                    (delete match (keymap-prefixes binding)))))))))
    (search-tree keymap keys)))

(defun parse-keyspec (string)
  (labels ((fail ()
             (editor-error "parse error: ~A" string))
           (parse (str)
             (loop :with ctrl :and meta :and super :and hyper :and shift
                   :do (cond
                         ((ppcre:scan "^[cmshCMSH]-" str)
                          (ecase (char-downcase (char str 0))
                            ((#\c) (setf ctrl t))
                            ((#\m) (setf meta t))
                            ((#\s) (setf super t))
                            ((#\h) (setf hyper t)))
                          (setf str (subseq str 2)))
                         ((ppcre:scan "^[sS]hift-" str)
                          (setf shift t)
                          (setf str (subseq str 6)))
                         ((string= str "")
                          (fail))
                         ((and (not (insertion-key-sym-p str))
                               (not (named-key-sym-p str)))
                          (fail))
                         (t
                          (return (make-key :ctrl ctrl
                                            :meta meta
                                            :super super
                                            :hyper hyper
                                            :shift shift
                                            :sym (or (named-key-sym-p str)
                                                     str))))))))
    (mapcar #'parse (uiop:split-string string :separator " "))))


(defun find-in-function-table (binding key)
  "search function-table of keymaps in hierarchy for KEY."
  (search-with-base
   binding
   (lambda (km)
     (cond ((typep km 'keymap*)
            (let ((result))
              (maphash (lambda (k v)
                         (when (and (null result) (equal k key))
                           (setf result (if (prefix-command-p v)
                                            v
                                            (make-prefix :key k :suffix v)))))
                       (keymap-function-table km))
              (or result
                  (loop for child in (keymap-children km)
                        thereis (or (find-in-function-table child key)
                                    (and (typep child 'keymap*)
                                         (keymap-undef-hook child)))))))
           ((typep km 'keymap)
            (loop for child in (keymap-children km)
                  thereis (find-in-function-table child key)))))))

(defmethod keymap-find ((keymap keymap) key)
  "finds key sequence in keymap, returns the matched prefix or nil."
  (let ((keyseq (etypecase key
                  (key (list key))
                  (list key))))
    (when (keymap-active-p keymap)
      ;; collect prefix matches from the prefixes slot
      (let ((prefix-matches
              (loop for item in (keymap-prefixes keymap)
                    when (and (prefix-active-p item)
                              (equal (prefix-key item) (car keyseq)))
                      collect item))
            (found))
        ;; search nested keymaps
        (loop for child in (keymap-children keymap)
              when (keymap-active-p child)
                do (let ((r (keymap-find child keyseq)))
                     (when r
                       (setf found r)
                       (return)))
                   ;; when we find an undef-hook, stop searching further to make find-undef-hook
                   ;; find the keymap instead
              when (and (typep child 'keymap*)
                        (keymap-undef-hook child))
                do (return))
        (or found
            ;; try collected prefix matches
            (loop for match in prefix-matches
                  for suffix = (prefix-suffix match)
                  for result = (cond
                                 ;; last key, return the matched prefix.
                                 ((null (cdr keyseq))
                                  match)
                                 ;; more keys, suffix is a keymap, recurse through keymap-find.
                                 ((typep suffix 'keymap)
                                  (keymap-find suffix (cdr keyseq)))
                                 (t nil))
                  when result
                    return result)
            (let ((base (keymap-base keymap)))
              (when base
                (keymap-find base keyseq))))))))

;; this is currently here for backwards compatibility
;; im not yet sure whether 'cmd' or function-table lookup is necessary (i think so but im not sure how to get rid of it.)
(defmethod keymap-find ((keymap keymap*) key)
  "finds key sequence in keymap, returns the matched prefix or nil."
  (or (call-next-method)
      (let ((keyseq (etypecase key
                      (key (list key))
                      (list key))))
        (let ((result (find-in-function-table keymap (car keyseq))))
          (when result
            (if (typep result 'prefix)
                result
                (make-prefix :key (car keyseq) :suffix result)))))))

(defun insertion-key-p (key)
  (let* ((key (typecase key
                (list (first key))
                (otherwise key)))
         (sym (key-sym key)))
    (cond ((match-key key :sym "Return") #\Return)
          ((match-key key :sym "Tab") #\Tab)
          ((match-key key :sym "Space") #\Space)
          ((and (insertion-key-sym-p sym)
                (match-key key :sym sym))
           (char sym 0)))))

(defgeneric compute-keymaps (global-mode)
  (:method ((mode global-mode)) nil))

(defun other-keymaps ()
  (let ((keymaps))
    ;; this one collects active modes. local shadows global.
    (dolist (mode (reverse (all-active-modes (current-buffer))))
      (alexandria:when-let ((keymap (mode-keymap mode)))
        (push keymap keymaps)))
    ;; major mode keymaps at point (context-specific).
    (alexandria:when-let* ((mode (major-mode-at-point (current-point)))
                           (keymap (mode-keymap mode)))
      (push keymap keymaps))
    ;; state keymaps (e.g. vi modes)
    (dolist (km (reverse (compute-keymaps (current-global-mode))))
      (push km keymaps))
    ;; special keymap (highest priority)
    (when *special-keymap*
      (push *special-keymap* keymaps))
    (delete-duplicates keymaps :from-end t)))

(defparameter *other-keymaps-root*
  (make-instance 'keymap :description '*other-keymaps-root*))

;; this is for some "other" keymaps that i need to inject into the root-keymap (atleast this way for now).
(defmethod keymap-children ((keymap (eql *other-keymaps-root*)))
  (other-keymaps))

(defmethod keymap-children ((keymap (eql *root-keymap*)))
  (cons *other-keymaps-root*
        (slot-value keymap 'children)))

(defun find-undef-hook ()
  (loop for km in (other-keymaps)
        when (and (typep km 'keymap*) (keymap-undef-hook km))
          return (keymap-undef-hook km)))

(defun lookup-keybind (key)
  (or (keymap-find *root-keymap* key)
      ;; find undef-hook in hierarchy (e.g. self-insert)
      (let ((hook (find-undef-hook)))
        (when hook
          (make-prefix :suffix hook)))))

(defun find-keybind (key)
  (let ((prefix (keymap-find *root-keymap* key)))
    (when prefix
      (prefix-suffix prefix))))

(defun traverse-keymap (keymap fun)
  (labels ((traverse-prefix (node prefix)
             (let ((key (prefix-key node))
                   (suffix (prefix-suffix node)))
               (cond ((or (typep suffix 'keymap)
                          (typep suffix 'prefix))
                      (traverse-node suffix (cons key prefix)))
                     (t
                      (funcall fun (reverse (cons key prefix)) suffix)))))
           (traverse-node (node prefix)
             (cond ((typep node 'keymap)
                    (mapc (lambda (p) (traverse-prefix p prefix))
                          (keymap-prefixes node))
                    (mapc (lambda (child) (traverse-node child prefix))
                          (keymap-children node)))
                   ((typep node 'prefix)
                    (traverse-prefix node prefix)))))
    (traverse-node keymap nil)))

(defun collect-command-keybindings (command keymap)
  (let ((bindings '()))
    (traverse-keymap keymap
                     (lambda (kseq cmd)
                       (when (eq cmd command)
                         (push kseq bindings))))
    (nreverse bindings)))

(defvar *abort-key*)

(defun abort-key-p (key)
  (and (key-p key)
       (let ((prefix (lookup-keybind key)))
         (and prefix (eq *abort-key* (prefix-suffix prefix))))))

(defmacro with-special-keymap ((keymap) &body body)
  `(let ((*special-keymap* (or ,keymap *special-keymap*)))
     ,@body))