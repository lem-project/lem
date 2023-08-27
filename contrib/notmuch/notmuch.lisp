(in-package :lem-notmuch)

(defvar *notmuch-saved-searches*
  '((:name "inbox" :query "tag:inbox")
    (:name "unread" :query "tag:inbox and tag:unread")))


(define-attribute notmuch-saved-search-attribute
  (t :bold t))



(define-command notmuch () ()
  (let ((buffer (make-buffer "*notmuch-hello*")))
    (switch-to-buffer buffer)
    (notmuch-hello-mode)
    (erase-buffer buffer)
    (let ((point (current-point)))
      (insert-string point (format nil "Saved searches:~%~%"))
      (dolist (saved-search *notmuch-saved-searches*)
	(destructuring-bind (&key name query &allow-other-keys)
	    saved-search
	  (let ((count (notmuch-count query)))
	    
	    (lem/button:insert-button point name (lambda () (with-context (notmuch-run-saved-query name query)))
			   :attribute 'notmuch-saved-search-attribute)

	    (insert-string point (format nil ": ~A" count))
	    (insert-character point #\newline 1)
	    )
	  
	  
	  ))
      (setf (buffer-read-only-p buffer) t))))

