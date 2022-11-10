(defpackage :lem.completion-mode
  (:use :cl :lem)
  (:export :make-completion-spec
           :make-completion-item
           :completion-item
           :completion-item-label
           :completion-item-detail
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

(defclass completion-item ()
  ((label
    :initarg :label
    :initform ""
    :reader completion-item-label
    :type string)
   (detail
    :initarg :detail
    :initform ""
    :reader completion-item-detail
    :type string)
   (start
    :initarg :start
    :initform nil
    :reader completion-item-start
    :type (or null point))
   (end
    :initarg :end
    :initform nil
    :reader completion-item-end
    :type (or null point))
   (focus-action
    :initarg :focus-action
    :initform nil
    :reader completion-item-focus-action
    :type (or null function))))

(defun make-completion-item (&rest initargs)
  (apply #'make-instance 'completion-item initargs))

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
(define-key *completion-mode-keymap* 'delete-previous-char 'completion-delete-previous-char)
(define-key *completion-mode-keymap* 'backward-delete-word 'completion-backward-delete-word)

(define-attribute completion-attribute
  (t :foreground "white" :background "RoyalBlue"))
(define-attribute non-focus-completion-attribute
  (:dark :foreground "white" :background "#444")
  (:light :foreground "black" :background "#DDD"))
(define-attribute detail-attribute
  (:dark :foreground "gray" :background "#444")
  (:light :foreground "#777" :background "#DDD"))

(defclass print-spec ()
  ((label-width
    :initarg :label-width
    :reader label-width)))

(defmethod lem.popup-window:apply-print-spec ((print-spec print-spec) point item)
  (insert-string point " ")
  (insert-string point (completion-item-label item))
  (move-to-column point (label-width print-spec) t)
  (line-end point)
  (insert-string point "  ")
  (unless (alexandria:emptyp (completion-item-detail item))
    (insert-string point (completion-item-detail item)
                   :attribute 'detail-attribute)
    (insert-string point " ")))

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
  (alexandria:when-let* ((item (lem.popup-window:get-focus-item))
                         (fn (completion-item-focus-action item)))
    (funcall fn)))

(define-command completion-self-insert () ()
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-character (current-point) c)
             (completion-again))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(define-command completion-delete-previous-char (n) ("p")
  (delete-previous-char n)
  (completion-again))

(define-command completion-backward-delete-word (n) ("p")
  (backward-delete-word n)
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

(defun narrowing-down (last-items)
  (when last-items
    (let ((n (partial-match (mapcar #'completion-item-label last-items))))
      (multiple-value-bind (start end)
          (completion-item-range (current-point) (first last-items))
        (cond ((and n (plusp n) (< (count-characters start end) n))
               (completion-insert (current-point)
                                  (first last-items)
                                  n)
               (completion-again)
               t)
              ((alexandria:length= last-items 1)
               (completion-insert (current-point)
                                  (first last-items))
               (completion-again)
               t)
              (t
               nil))))))

(define-command completion-narrowing-down-or-next-line () ()
  (when *last-items*
    (or (narrowing-down *last-items*)
        (completion-next-line))))

(defun start-completion-mode (completion-spec)
  (setf *current-completion-spec* completion-spec)
  (completion-mode t))

(defun completion-item-range (point item)
  (let ((start (or (completion-item-start item)
                   (with-point ((start point))
                     (skip-chars-backward start #'syntax-symbol-char-p)
                     start)))
        (end (or (completion-item-end item)
                 point)))
    (values start end)))

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

(defun compute-completion-items (completion-spec)
  (let ((items (funcall (spec-function completion-spec) (current-point))))
    (when (spec-prefix-search completion-spec)
      (setf items
            (prefix-search (points-to-string *initial-point* (current-point))
                           items)))
    items))

(defun completion-items (completion-spec repeat)
  (cond ((and repeat (spec-prefix-search completion-spec))
         (prefix-search (points-to-string *initial-point* (current-point))
                        *last-items*))
        (t
         (setf *last-items* (compute-completion-items completion-spec)))))

(defun run-completion-1 (completion-spec repeat)
  (let ((items (completion-items completion-spec repeat)))
    (cond ((null items)
           (when repeat (completion-end)))
          ((and (not repeat) (null (rest items)))
           (completion-insert (current-point) (first items)))
          (repeat
           (lem-if:popup-menu-update (implementation) items))
          (t
           (lem-if:display-popup-menu
            (implementation)
            items
            :action-callback
            (lambda (item)
              (completion-insert (current-point) item)
              (completion-end))
            :print-spec
            (make-instance 'print-spec
                           :label-width
                           (loop :for item :in items
                                 :maximize (1+ (length (completion-item-label item)))))
            :focus-attribute 'completion-attribute
            :non-focus-attribute 'non-focus-completion-attribute
            :style '(:use-border nil :offset-y 1))
           (start-completion-mode completion-spec)
           (unless repeat
             (narrowing-down *last-items*))))))

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
