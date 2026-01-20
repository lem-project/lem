(in-package :lem-core)

(defmacro defclass-dynamic (name direct-superclasses direct-slots &rest options)
  "defines a class with support for 'dynamic' slots.

slots with the :dynamic t option will have accessors that automatically handle values which are functions.
if a dynamic slot contains a function, the accessor will call it and return the result. otherwise,
it returns the value directly.
the underlying storage slot is renamed with a '*' suffix."
  (let ((dynamic-slots
          (loop :for slot :in direct-slots
                :when (getf (cdr slot) :dynamic)
                  :collect slot)))
    (setf direct-slots
          (loop :for slot :in direct-slots
                :collect (if (getf (cdr slot) :dynamic)
                             (let* ((slot-name (first slot))
                                    (accessor-name
                                      (intern (format nil "~A-~A" name slot-name)))
                                    (internal-accessor-name
                                      (intern (format nil "~A*" accessor-name)))
                                    (new-slot (copy-list slot)))
                               (remf (cdr new-slot) :dynamic)
                               (setf (getf (cdr new-slot) :accessor)
                                     internal-accessor-name)
                               new-slot)
                             slot)))
    `(progn
       (defclass ,name ,direct-superclasses
         ,direct-slots
         ,@options)
       ,@(loop :for slot :in dynamic-slots
               :for slot-name := (first slot)
               :for accessor := (intern (format nil "~A-~A" name slot-name))
               :for internal-accessor := (intern (format nil "~A*" accessor))
               :collect `(defmethod ,accessor ((object ,name))
                           (let ((value (,internal-accessor object)))
                             (if (functionp value)
                                 (funcall value)
                                 value)))
               :collect `(defmethod (setf ,accessor) (new-value (object ,name))
                           (setf (,internal-accessor object) new-value))))))

;; a non-suffix prefix cannot be a keymap, thats why keymap doesnt inherit from prefix. this makes sense because a "prefix keymap" is a keymap that shares a common prefix, but the root map for example may contain keybindings with no prefixes.
(defclass-dynamic prefix ()
  ((key
    :initarg :key
    :dynamic t
    :documentation "the key defined for the prefix. could be a function that returns a key.")
   (description
    :initarg :description
    :dynamic t
    :initform nil)
   (suffix
    :initarg :suffix
    :dynamic t
    :documentation "the suffix defined for the prefix, could be another prefix or a keymap or a function that returns one.")
   (active-p
    :initarg :active-p
    :dynamic t
    :documentation "whether a prefix is active."
    :initform t)
   (properties
    :initarg :properties
    :accessor prefix-properties
    :initform nil
    :documentation "extra metadata that a prefix may hold.")))

(defun make-prefix (&key key suffix description)
  (let ((prefix (make-instance
                 'prefix
                 :key key
                 :suffix suffix
                 :description description)))
    prefix))

(defclass-dynamic keymap ()
  ;; children could contain keymaps or prefixes.
  ((children
    :initarg :children
    :dynamic t
    :initform nil
    :documentation "the children of the keymap. could be a function that returns a list of children.")
   (properties
    :initarg :properties
    :accessor keymap-properties
    :initform nil
    :documentation "additional metadata that a keymap holds.")
   (description
    :initarg :description
    :dynamic t
    :initform nil)
   (active-p
    :initarg :active-p
    :dynamic t
    :documentation "whether a prefix is active."
    :initform t)))

(defmethod keymap-add-prefix ((keymap keymap) (prefix prefix) &optional after)
  (unless (listp (keymap-children* keymap))
    (error "trying to add key to a non-static keymap."))
  (if after
      (setf (keymap-children* keymap) (append (keymap-children* keymap) (list prefix)))
      (push prefix (keymap-children* keymap))))

(defmethod keymap-add-child ((keymap keymap) (keymap2 keymap) &optional after)
  (unless (listp (keymap-children* keymap))
    (error "trying to add nested keymap to a non-static keymap."))
  (if after
      (setf (keymap-children* keymap) (append (keymap-children* keymap) (list keymap2)))
      (push keymap2 (keymap-children* keymap))))

(defgeneric prefix-p (keymap)
  (:documentation "check whether this is a prefix of another prefix.

a prefix is a prefix of another if its a keymap or if its suffix is a prefix."))

(defmethod prefix-p ((km keymap))
  t)

(defmethod prefix-p ((p prefix))
  (or (typep (prefix-suffix p) 'prefix)
      (typep (prefix-suffix p) 'keymap)))

(defgeneric keymap-activate (keymap)
  (:documentation "a hook for when a keymap is entered by some prefix.")
  ;; default keymap-activate does nothing
  (:method ((keymap t))
    nil))

(defgeneric prefix-invoke (prefix)
  (:documentation "a hook for when a prefix is reached.")
  (:method ((prefix t)) nil))

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
    :initform (make-hash-table :test 'eq))
   (name
    :initarg :name
    :accessor keymap-name
    :initform nil)))

;; *root-keymap* contains all keymaps as (possibly nested, possibly "dynamic") children
(defvar *root-keymap* (make-instance 'keymap*))

(defvar *special-keymap* nil)

(defmethod print-object ((object keymap) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-name object)
      (princ (keymap-name object) stream))))

(defun make-keymap (&key undef-hook parent name)
  (let ((keymap (make-instance
                 'keymap*
                 :undef-hook undef-hook
                 :name name)))
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

;; this takes a single key and not a key sequence
;; i think this could be split into 2 defmethods but ill leave it for now
(defun prefix-for-key (binding key)
  "takes a keymap or a prefix, returns the prefix that corresponds to the given key (could be just BINDING)."
  (check-type binding (or prefix keymap))
  (cond ((typep binding 'prefix)
         (when (equal (prefix-key binding) key)
           binding))
        ((typep binding 'keymap)
         (loop for item in (keymap-children binding)
               for p = (prefix-for-key item key)
               do (when p
                    (return p))))))

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
                      (setf next-keymap (make-instance 'keymap*))
                      (setf (prefix-suffix next-prefix) next-keymap))))
              (progn
                (setf next-keymap (make-instance 'keymap*))
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
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (remhash k table))
                  (t
                   (let ((next (gethash k table)))
                     (when (prefix-command-p next)
                       (setf table next)))))))

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

(defun find-matching-prefixes (binding key)
  "find prefixes in children that match KEY."
  (cond ((typep binding 'prefix)
         (when (and (prefix-active-p binding)
                    (equal (prefix-key binding) key))
           (list binding)))
        ((typep binding 'keymap)
         (when (keymap-active-p binding)
           (loop for item in (keymap-children binding)
                 append (find-matching-prefixes item key))))))

(defun find-in-function-table (binding key)
  "search function-table of keymaps in hierarchy for KEY."
  (cond ((typep binding 'keymap*)
         (let ((result))
           (maphash (lambda (k v)
                      (when (and (null result) (equal k key))
                        (setf result (if (prefix-command-p v)
                                         v
                                         (make-prefix :key k :suffix v)))))
                    (keymap-function-table binding))
           ;; if found, return it; otherwise search children
           (or result
               (loop for child in (keymap-children binding)
                     thereis (find-in-function-table child key)))))
        ((typep binding 'keymap)
         (loop for child in (keymap-children binding)
               thereis (find-in-function-table child key)))
        ((typep binding 'prefix)
         (find-in-function-table (prefix-suffix binding) key))))

(defun find-undef-hook-in-hierarchy (binding)
  "find the first undef-hook from active keymaps."
  (declare (ignore binding))
  (loop for km in (all-keymaps)
        when (and (typep km 'keymap*) (keymap-undef-hook km))
          return (keymap-undef-hook km)))

(defmethod find-suffix ((keymap keymap) keyseq)
  "search KEYMAP tree for exact binding matching KEYSEQ. returns (suffix . prefix)"
  (labels ((search-tree (binding keys parent-prefix)
             (if (null keys)
                 (if (typep binding 'prefix)
                     (cons (prefix-suffix binding) binding)
                     (when binding
                       (cons binding parent-prefix)))
                 ;; try all matches and return first successful result
                 (loop for match in (find-matching-prefixes binding (car keys))
                       for result = (search-tree (prefix-suffix match) (cdr keys) match)
                       when result return result))))
    (search-tree keymap keyseq nil)))

;; this is currently here for backwards compatibility
;; im not yet sure whether 'cmd' or function-table lookup is necessary (i think so but im not sure how to get rid of it.)
(defmethod keymap-find-keybind ((keymap keymap) key cmd)
  "finds key sequence in keymap, returns (suffix . prefix)."
  (let ((keyseq (etypecase key
                  (key (list key))
                  (list key))))
    (or ;; search children prefixes
     (find-suffix keymap keyseq)
     (cons
      (or
       ;; search function-table in hierarchy
       (find-in-function-table keymap (car keyseq))
       ;; check function-table for cmd symbol
       (gethash cmd (keymap-function-table keymap))
       ;; find undef-hook in hierarchy (e.g. self-insert)
       (find-undef-hook-in-hierarchy keymap)
       ;; return cmd as fallback
       cmd)
      nil))))

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

(defun all-keymaps ()
  ;; build list in reverse priority order, then nreverse at end
  ;; lower priority first, higher priority last (before nreverse)
  (let* ((keymaps nil))
    ;; first add global/minor modes (lowest priority)
    (dolist (mode (all-active-modes (current-buffer)))
      (when (mode-keymap mode)
        (push (mode-keymap mode) keymaps)))
    ;; add major-mode keymap
    (alexandria:when-let* ((mode (major-mode-at-point (current-point)))
                           (keymap (mode-keymap mode)))
      (push keymap keymaps))
    ;; add state keymaps from compute-keymaps (highest priority)
    (dolist (km (compute-keymaps (current-global-mode)))
      (push km keymaps))
    ;; special keymap has highest priority
    (when *special-keymap*
      (push *special-keymap* keymaps))
    (delete-duplicates keymaps)))

;; this is for some "other" keymaps that i need to inject into the root-keymap (atleast this way for now).
;; we could make *root-keymap* itself have dynamic children and inject those into it but i dont want that,
;; so we create a second-level keymap as the root for all 'other-keymaps' and inject that keymap
;; into *root-keymap*
(defun other-keymaps ()
  (all-keymaps))
(defparameter *other-keymaps-root*
  (make-instance 'keymap*
                 :children #'other-keymaps))

(defun lookup-keybind (key)
  (unless (find *other-keymaps-root* (keymap-children *root-keymap*))
    (keymap-add-child *root-keymap* *other-keymaps-root*))
  (keymap-find-keybind *root-keymap* key nil))

(defun find-keybind (key)
  (let ((result (keymap-find-keybind *root-keymap* key nil)))
    (when result
      result)))

(defun traverse-keymap (keymap fun)
  (labels ((f (node prefix)
             (cond ((typep node 'keymap)
                    (mapc (lambda (child) (f child prefix))
                          (keymap-children node)))
                   ((typep node 'prefix)
                    (let ((key (prefix-key node))
                          (suffix (prefix-suffix node)))
                      (cond ((or (typep suffix 'keymap)
                                 (typep suffix 'prefix))
                             (f suffix (cons key prefix)))
                            (t
                             (funcall fun (reverse (cons key prefix)) suffix))))))))
    (f keymap nil)))

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
       (eq *abort-key* (lookup-keybind key))))

(defmacro with-special-keymap ((keymap) &body body)
  `(let ((*special-keymap* (or ,keymap *special-keymap*)))
     ,@body))