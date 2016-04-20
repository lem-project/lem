;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(popup-completion
          delete-completion-window))

(defvar *comp-buffer-name* "*Completion*")
(defvar *completion-window* nil)
(defvar *completion-flag* nil)

(let ((prev-str))
  (defun popup-completion (comp-f str)
    (let ((comp-flag t))
      (when *completion-window*
        (when-continue-flag
         :completion
         (when (equal str prev-str)
           (setq comp-flag nil)
           (with-current-window *completion-window*
             (unless (scroll-down (1- (window-height (current-window))))
               (point-set (point-min)))
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
