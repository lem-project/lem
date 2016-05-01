(in-package :lem)

(export '(completion
          popup-completion
          delete-completion-window))

(defvar *completion-window* nil)

(defun completion (name list)
  (let ((strings
         (remove-if-not #'(lambda (elt)
                            (and (<= (length name) (length elt))
                                 (string= name elt
                                          :end2 (length name))))
                        list)))
    (cond
     ((null strings) nil)
     ((null (cdr strings)) (car strings))
     (t
      (let* ((str (car strings))
             (len (length str)))
        (dolist (s (cdr strings))
          (let ((res (mismatch str s :end1 len)))
            (when res
              (setq len res))))
        (values (subseq str 0 len) strings))))))

(defvar *completion-mode-keymap* (make-keymap 'completion-self-insert))

(define-minor-mode completion-mode
  (:name "completion"
   :keymap *completion-mode-keymap*))

(defvar *completion-overlay* nil)
(defvar *completion-overlay-attribute* (make-attribute "blue" nil :reverse-p t))

(defun completion-update-overlay ()
  (when *completion-overlay*
    (delete-overlay *completion-overlay*))
  (setf *completion-overlay*
        (make-overlay (progn (beginning-of-line) (current-point))
                      (progn (end-of-line) (current-point))
                      *completion-overlay-attribute*)))

(define-key *completion-mode-keymap* (kbd "M-n") 'completion-next-line)
(define-command completion-next-line (n) ("p")
  (with-current-window *completion-window*
    (forward-line n)
    (completion-update-overlay)))

(define-key *completion-mode-keymap* (kbd "M-p") 'completion-previous-line)
(define-command completion-previous-line (n) ("p")
  (with-current-window *completion-window*
    (forward-line (- n))
    (completion-update-overlay)))

(defvar *completion-last-string* nil)
(defvar *completion-last-function* nil)

(defun completion-end ()
  (completion-mode nil)
  (delete-completion-window))

(define-key *completion-mode-keymap* (kbd "C-m") 'completion-select)
(define-key *completion-mode-keymap* (kbd "C-i") 'completion-select)
(define-command completion-select () ()
  (let (str)
    (with-current-window *completion-window*
      (setf str (current-line-string)))
    (delete-char (- (length *completion-last-string*)) nil)
    (insert-string str)
    (completion-end))
  t)

(define-key *completion-mode-keymap* (kbd "C-h") 'completion-delete-previous-char)
(define-key *completion-mode-keymap* (kbd "[backspace]") 'completion-delete-previous-char)
(define-command completion-delete-previous-char (n) ("p")
  (delete-char (- n) nil)
  (popup-completion *completion-last-function*
                    (subseq *completion-last-string* 0 (- (length *completion-last-string*) n))))

(define-command completion-self-insert (n) ("p")
  (let ((c (insertion-key-p (last-read-key-sequence))))
    (cond (c (insert-char c n)
             (popup-completion *completion-last-function*
                               (concatenate 'string
                                            *completion-last-string*
                                            (string c))))
          (t (unread-key-sequence (last-read-key-sequence))
             (completion-end)))))

(defun popup-completion (comp-f str)
  (setf *completion-last-function* comp-f)
  (setf *completion-last-string* str)
  (multiple-value-bind (result strings) (funcall comp-f str)
    (when (and result (null strings))
      (setf strings (list result)))
    (cond (strings
           (let ((buffer (get-buffer-create "*Completions*")))
             (setf *completion-window*
                   (info-popup buffer
                               (lambda (out)
                                 (format out "~{~A~%~}" strings))
                               nil))
             (setf (window-delete-hook *completion-window*)
                   (lambda () (setf *completion-window* nil)))
             (setf (get-bvar :completion-buffer-p :buffer buffer) t)
             (completion-mode t)
             (with-current-window *completion-window*
               (completion-update-overlay))))
          ((null result)
           (completion-end)))
    (if result
        (values result t)
        (values str nil))))

(defun delete-completion-window ()
  (when (and (window-p *completion-window*)
             (not (deleted-window-p *completion-window*)))
    (with-current-window *completion-window*
      (when (get-bvar :completion-buffer-p
                      :buffer (window-buffer *completion-window*))
        (quit-window)))
    (setf *completion-window* nil)))
