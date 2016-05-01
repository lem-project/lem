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

(let ((state :first))
  (defun popup-completion (comp-f str)
    (unless (continue-flag :completion)
      (setf state :first))
    (case state
      (:first
       (setf state :rest)
       (multiple-value-bind (result strings) (funcall comp-f str)
         (cond (strings
                (let ((buffer (get-buffer-create "*Completions*")))
                  (setf *completion-window*
                        (info-popup buffer
                                    (lambda (out)
                                      (format out "~{~A~%~}" strings))
                                    nil))
                  (setf (window-delete-hook *completion-window*)
                        (lambda () (setf *completion-window* nil)))
                  (setf (get-bvar :completion-buffer-p :buffer buffer) t)))
               (t
                (delete-completion-window)))
         (if result
             (values result t)
             (values str nil))))
      (:rest
       (with-current-window *completion-window*
         (unless (scroll-down (1- (window-height (current-window))))
           (point-set (point-min)))
         (redraw-display))
       str)))

  (defun delete-completion-window ()
    (setf state :first)
    (when (and (window-p *completion-window*)
               (not (deleted-window-p *completion-window*)))
      (with-current-window *completion-window*
        (when (get-bvar :completion-buffer-p
                        :buffer (window-buffer *completion-window*))
          (quit-window)))
      (setf *completion-window* nil))))
