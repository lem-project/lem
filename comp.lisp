(in-package :lem)

(defvar *comp-buffer-name* "*Completion*")
(defvar *completion-window* nil)

(defun completion (name list)
  (let ((strings (remove-if-not (lambda (elt)
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

(defun popup-completion (comp-f str)
  (multiple-value-bind (result strings) (funcall comp-f str)
    (when strings
      (popup (get-buffer-create *comp-buffer-name*)
             (lambda ()
               (setq *completion-window* *current-window*)
               (dolist (s strings)
                 (buffer-append-line (window-buffer) s))))
      (window-update-all))
    (or result str)))

(defun delete-completion-window ()
  (dolist (win *window-list*)
    (when (and (eq win *completion-window*)
               (string= (buffer-name (window-buffer win))
                        *comp-buffer-name*))
      (delete-window-1 win)
      (return))))
