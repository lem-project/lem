(defpackage :lem.completion-mode
  (:use :cl :lem)
  (:export :make-completion-item
           :run-completion))
(in-package :lem.completion-mode)

(defstruct completion-item
  (label "" :read-only t :type string)
  (detail "" :read-only t :type string)
  (start nil :read-only t :type (or null point))
  (end nil :read-only t :type (or null point))
  (apply-fn nil :read-only t :type (or null function)))

(defvar *completion-mode-keymap* (make-keymap :name '*completion-mode-keymap*
                                              :undef-hook 'completion-self-insert))
(define-minor-mode completion-mode
    (:name "completion"
     :keymap *completion-mode-keymap*))

(define-key *completion-mode-keymap* 'next-line 'completion-next-line)
(define-key *completion-mode-keymap* "M-n"    'completion-next-line)
(define-key *completion-mode-keymap* "C-i"    'completion-next-line)
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

(defun completion-end ()
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

(defun start-completion-mode (restart-function)
  (setf *completion-restart-function* restart-function)
  (completion-mode t))

(defun completion-insert (point item)
  (when item
    (cond ((completion-item-apply-fn item)
           (funcall (completion-item-apply-fn item)
                    point))
          ((and (completion-item-start item)
                (completion-item-end item))
           (move-point point (completion-item-start item))
           (delete-between-points (completion-item-start item)
                                  (completion-item-end item))
           (insert-string point (completion-item-label item)))
          (t
           (with-point ((start point))
             (skip-chars-backward start #'syntax-symbol-char-p)
             (delete-between-points start point)
             (insert-string start (completion-item-label item)))))))

(defun run-completion-1 (function repeat)
  (let ((items (funcall function (current-point))))
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
                                                         (completion-insert (current-point) item))
                                      :print-function 'completion-item-label)
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

(defun minibuffer-file-complete (str directory)
  (mapcar (lambda (filename)
            (make-completion-item :label (pathname-name* filename)
                                  :apply-fn (lambda (p)
                                              (move-point p (lem::minibuffer-start-point))
                                              (delete-between-points
                                               p (line-end (copy-point p :temporary)))
                                              (insert-string p filename))))
          (completion-file str directory)))

(setf *minibuffer-file-complete-function* 'minibuffer-file-complete)
