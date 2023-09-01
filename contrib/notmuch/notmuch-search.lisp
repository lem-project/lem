
(in-package :lem-notmuch)

(define-major-mode notmuch-search-mode ()
    (:name "notmuch-search"
     :keymap *notmuch-search-mode-keymap*))





(defun notmuch-run-saved-query (name query)
  (let ((buffer (make-buffer (format nil "*notmuch-saved-query-~A%" name)))
	(results (notmuch-search query :oldest-first t)))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (erase-buffer buffer)
    (let ((point (current-point)))
      (dolist (item results)
	(when item
	  (destructuring-bind (&key thread date_relative matched total authors query subject query tags &allow-other-keys)
	      item
	    (lem/button:insert-button
	     point
	     (format nil "~20@A [~A/~A] ~20@A ~A"
		     date_relative matched total authors subject)
	     (lambda () (notmuch-open-message subject query)))
	    (insert-character point #\newline 1)
	    ))
      )
    (setf (buffer-read-only-p buffer) t)
      )))


; todo
;(define-key *notmuch-search-mode-keymap* "Return" 'lem-notmuch:notmuch-search-show-thread)

(define-key *notmuch-search-mode-keymap* "q" 'quit-active-window)

