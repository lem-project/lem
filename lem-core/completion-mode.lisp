(defpackage :lem.completion-mode
  (:use :cl :lem)
  (:export :make-completion-spec
           :make-completion-item
           :run-completion)
  #+sbcl
  (:lock t))
(in-package :lem.completion-mode)

(defclass completion-spec ()
  ((function
    :initarg :function
    :reader spec-function)
   (prefix-search
    :initarg :prefix-search
    :reader spec-prefix-search)))

(defun make-completion-spec (function &key prefix-search)
  (make-instance 'completion-spec :function function :prefix-search prefix-search))

(defstruct completion-item
  (label "" :read-only t :type string)
  (detail "" :read-only t :type string)
  (start nil :read-only t :type (or null point))
  (end nil :read-only t :type (or null point))
  (focus-action nil :read-only t :type (or null function)))

(defvar *completion-mode-keymap* (make-keymap :name '*completion-mode-keymap*
                                              :undef-hook 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* 'next-line 'completion-next-line)
(define-key *completion-mode-keymap* "M-n"    'completion-next-line)
(define-key *completion-mode-keymap* "Tab"    'completion-narrowing-down-or-next-line)
(define-key *completion-mode-keymap* 'previous-line 'completion-previous-line)
(define-key *completion-mode-keymap* "M-p"    'completion-previous-line)
(define-key *completion-mode-keymap* 'move-to-end-of-buffer 'completion-end-of-buffer)
(define-key *completion-mode-keymap* 'move-to-beginning-of-buffer 'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "Return"    'completion-select)
(define-key *completion-mode-keymap* "Space"    'completion-insert-space-and-cancel)
(define-key *completion-mode-keymap* 'delete-previous-char 'completion-delete-prevous-char)

(define-attribute completion-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-completion-attribute
  (t :foreground "black" :background "gray"))

(defvar *current-completion-spec* nil)
(defvar *last-items* nil)
(defvar *initial-point* nil)

(defun completion-end ()
  (setf *last-items* nil)
  (when *initial-point* (delete-point *initial-point*))
  (completion-mode nil)
  (lem-if:popup-menu-quit (implementation)))

(defun completion-again ()
  (when *current-completion-spec*
    (run-completion-1 *current-completion-spec* t)))

(defun call-focus-action ()
  (alexandria:when-let* ((item (lem.popup-window::get-focus-item))
                         (fn (completion-item-focus-action item)))
    (funcall fn)))

(define-command completion-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c)
             (completion-again))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(define-command completion-delete-prevous-char (n) ("p")
  (delete-previous-char n)
  (completion-again))

(define-command completion-next-line () ()
  (lem-if:popup-menu-down (implementation))
  (call-focus-action))

(define-command completion-previous-line () ()
  (lem-if:popup-menu-up (implementation))
  (call-focus-action))

(define-command completion-end-of-buffer () ()
  (lem-if:popup-menu-last (implementation))
  (call-focus-action))

(define-command completion-beginning-of-buffer () ()
  (lem-if:popup-menu-first (implementation))
  (call-focus-action))

(define-command completion-select () ()
  (lem-if:popup-menu-select (implementation)))

(define-command completion-insert-space-and-cancel () ()
  (insert-character (current-point) #\space)
  (completion-end))

(defun partial-match (strings)
  (when strings
    (let ((n nil))
      (loop :for rest :on strings
            :do (loop :for rest2 :on (cdr rest)
                      :for mismatch := (mismatch (first rest) (first rest2))
                      :do (and mismatch
                               (setf n
                                     (if n
                                         (min n mismatch)
                                         mismatch)))))
      n)))

(define-command completion-narrowing-down-or-next-line () ()
  (when *last-items*
    (let ((n (partial-match (mapcar #'completion-item-label *last-items*))))
      (multiple-value-bind (start end)
          (completion-item-range (current-point) (first *last-items*))
        (cond ((and n (plusp n) (< (count-characters start end) n))
               (completion-insert (current-point)
                                  (first *last-items*)
                                  n)
               (completion-again))
              ((alexandria:length= *last-items* 1)
               (completion-insert (current-point)
                                  (first *last-items*))
               (completion-again))
              (t
               (completion-next-line)))))))

(defun start-completion-mode (completion-spec)
  (setf *current-completion-spec* completion-spec)
  (completion-mode t))

(defun completion-item-range (point item)
  (cond ((and (completion-item-start item)
              (completion-item-end item))
         (values (completion-item-start item)
                 (completion-item-end item)))
        (t
         (with-point ((start point))
           (skip-chars-backward start #'syntax-symbol-char-p)
           (values start point)))))

(defun completion-insert (point item &optional begin)
  (when item
    (multiple-value-bind (start end) (completion-item-range point item)
      (delete-between-points start end)
      (insert-string point (subseq (completion-item-label item) 0 begin)))))

(defun prefix-search (prefix-string items)
  (completion prefix-string
              items
              :test #'alexandria:starts-with-subseq
              :key #'completion-item-label))

(defun completion-items (completion-spec repeat)
  (cond ((and repeat (spec-prefix-search completion-spec))
         (prefix-search (points-to-string *initial-point* (current-point))
                        *last-items*))
        (t
         (let ((items (funcall (spec-function completion-spec) (current-point))))
           (when (spec-prefix-search completion-spec)
             (setf items
                   (prefix-search (points-to-string *initial-point* (current-point))
                                  items)))
           (setf *last-items* items)))))

(defun run-completion-1 (completion-spec repeat)
  (let ((items (completion-items completion-spec repeat)))
    (cond ((null items)
           (when repeat (completion-end)))
          ((and (not repeat) (null (rest items)))
           (completion-insert (current-point) (first items)))
          (repeat
           (lem-if:popup-menu-update (implementation) items))
          (t
           (lem-if:display-popup-menu (implementation)
                                      items
                                      :action-callback (lambda (item)
                                                         (completion-insert (current-point) item)
                                                         (completion-end))
                                      :print-function 'completion-item-label
                                      :focus-attribute 'completion-attribute
                                      :non-focus-attribute 'non-focus-completion-attribute)
           (start-completion-mode completion-spec)))))

(defun run-completion (completion)
  (let ((completion-spec
          (typecase completion
            (completion-spec
             completion)
            (otherwise
             (make-completion-spec (alexandria:ensure-function completion))))))
    (when (spec-prefix-search completion-spec)
      (setf *initial-point*
            (copy-point (current-point) :right-inserting))
      (skip-chars-backward *initial-point* #'syntax-symbol-char-p))
    (run-completion-1 completion-spec
                      nil)))

(defun minibuffer-completion (comp-f start)
  (run-completion
   (lambda (point)
     (with-point ((start start)
                  (end point))
       (let ((items (funcall comp-f
                             (points-to-string start
                                               (buffer-end-point (point-buffer end))))))
         (loop :for item? :in items
               :for item := (typecase item?
                              (string
                               (make-completion-item :label item?
                                                     :start start
                                                     :end end))
                              (completion-item
                               item?))
               :when item
               :collect item))))))

(setf *minibuffer-completion-function* 'minibuffer-completion)


(defun pathname-name* (pathname)
  (enough-namestring
   pathname
   (if (uiop:directory-pathname-p pathname)
       (uiop:pathname-parent-directory-pathname pathname)
       (uiop:pathname-directory-pathname pathname))))

(defun minibuffer-file-complete (str directory &key directory-only)
  (mapcar (lambda (filename)
            (let ((label (pathname-name* filename)))
              (with-point ((s (lem::minibuffer-start-point))
                           (e (lem::minibuffer-start-point)))
                (make-completion-item :label label
                                      :start (character-offset
                                              s
                                              (length (namestring (uiop:pathname-directory-pathname str))))
                                      :end (line-end e)))))
          (completion-file str directory :directory-only directory-only)))

(setf *minibuffer-file-complete-function* 'minibuffer-file-complete)
