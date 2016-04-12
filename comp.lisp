;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(completion
          popup-completion
          delete-completion-window))

(defvar *comp-buffer-name* "*Completion*")
(defvar *completion-window* nil)
(defvar *completion-flag* nil)

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

(let ((prev-str))
  (defun popup-completion (comp-f str)
    (let ((comp-flag t))
      (when *completion-window*
        (when-continue-flag
         :completion
         (when (equal str prev-str)
           (setq comp-flag nil)
           (with-current-window *completion-window*
             (unless (scroll-down (1- (window-height)))
               (beginning-of-buffer))
             (window-update-all)))))
      (setq prev-str str)
      (if (null comp-flag)
          (values str nil)
          (multiple-value-bind (result strings) (funcall comp-f str)
            (cond (strings
                   (setq *completion-flag* t)
                   (let ((buffer (get-buffer-create *comp-buffer-name*)))
                     (setq *completion-window*
                           (info-popup buffer
                                       #'(lambda (out)
                                           (dolist (s strings)
                                             (format out "~a~%" s)))
                                       nil))
                     (setf (window-delete-hook *completion-window*)
                           #'(lambda ()
                               (setq *completion-window* nil)))
                     (setf (get-bvar :completion-buffer-p :buffer buffer) t)))
                  (t
                   (delete-completion-window)))
            (window-update-all)
            (if result
                (values result t)
                (values str nil)))))))

(defun delete-completion-window ()
  (when *completion-flag*
    (setq *completion-flag* nil)
    (unless (deleted-window-p *completion-window*)
      (let ((prev-window (current-window)))
        (setf (current-window) *completion-window*)
        (let ((buffer (window-buffer *completion-window*)))
          (when (get-bvar :completion-buffer-p :buffer buffer)
            (quit-window)))
        (unless (deleted-window-p prev-window)
          (setf (current-window) prev-window)))
      (setq *completion-window* nil))))
