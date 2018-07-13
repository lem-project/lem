(defpackage :lem.completion-mode
  (:use :cl :lem)
  (:export :make-completion-item
           :run-completion))
(in-package :lem.completion-mode)

(defstruct completion-item
  (label "" :read-only t :type string)
  (detail "" :read-only t :type string)
  (start nil :read-only t :type (or null point))
  (end nil :read-only t :type (or null point)))

(defvar *completion-mode-keymap* (make-keymap :name '*completion-mode-keymap*
                                              :undef-hook 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* 'next-line 'completion-next-line)
(define-key *completion-mode-keymap* "M-n"    'completion-next-line)
(define-key *completion-mode-keymap* "C-i"    'completion-narrowing-down-or-next-line)
(define-key *completion-mode-keymap* 'previous-line 'completion-previous-line)
(define-key *completion-mode-keymap* "M-p"    'completion-previous-line)
(define-key *completion-mode-keymap* 'move-to-end-of-buffer 'completion-end-of-buffer)
(define-key *completion-mode-keymap* 'move-to-beginning-of-buffer 'completion-beginning-of-buffer)
(define-key *completion-mode-keymap* "C-m"    'completion-select)
(define-key *completion-mode-keymap* "Space"    'completion-insert-space-and-cancel)
(define-key *completion-mode-keymap* 'delete-previous-char 'completion-delete-prevous-char)

(define-attribute completion-attribute
  (t :foreground "blue" :background "white" :reverse-p t))
(define-attribute non-focus-completion-attribute
  (t))

(defvar *completion-restart-function* nil)
(defvar *last-items* nil)

(defun completion-end ()
  (setf *last-items* nil)
  (completion-mode nil)
  (lem-if:popup-menu-quit (implementation)))

(defun completion-again ()
  (when *completion-restart-function*
    (run-completion-1 *completion-restart-function* t)))

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
  (lem-if:popup-menu-down (implementation)))

(define-command completion-previous-line () ()
  (lem-if:popup-menu-up (implementation)))

(define-command completion-end-of-buffer () ()
  (lem-if:popup-menu-last (implementation)))

(define-command completion-beginning-of-buffer () ()
  (lem-if:popup-menu-first (implementation)))

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
                      :do (setf n
                                (if n
                                    (min n mismatch)
                                    mismatch))))
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

(defun start-completion-mode (restart-function)
  (setf *completion-restart-function* restart-function)
  (completion-mode t))

(defun completion-item-range (point item)
  (cond ((and (completion-item-start item)
              (completion-item-end item))
         (values (completion-item-start item)
                 (completion-item-end item)))
        (t
         (values (skip-chars-backward (copy-point point :temporary)
                                      #'syntax-symbol-char-p)
                 point))))

(defun completion-insert (point item &optional begin)
  (when item
    (multiple-value-bind (start end) (completion-item-range point item)
      (delete-between-points start end)
      (insert-string point (subseq (completion-item-label item) 0 begin)))))

(defun run-completion-1 (function repeat)
  (let ((items (funcall function (current-point))))
    (setf *last-items* items)
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
           (start-completion-mode function)))))

(defun run-completion (function)
  (run-completion-1 function nil))

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
            (let* ((label (pathname-name* filename))
                   (n (- (length filename) (length label))))
              (with-point ((s (lem::minibuffer-start-point))
                           (e (lem::minibuffer-start-point)))
                (make-completion-item :label label
                                      :start (character-offset s n)
                                      :end (line-end e)))))
          (completion-file str directory :directory-only directory-only)))

(setf *minibuffer-file-complete-function* 'minibuffer-file-complete)
