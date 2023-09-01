
(in-package :lem-notmuch)

(define-major-mode notmuch-hello-mode ()
    (:name "notmuch-hello"
     :keymap *notmuch-hello-mode-keymap*))

;; fixme/unused: deprecate for button
(defun find-saved-search (name-to-find)
  (dolist (item *notmuch-saved-searches*)
    (destructuring-bind (&key name query &allow-other-keys)
	item
      (when (string-equal name-to-find name)
	(return-from find-saved-search query)))))

(defun notmuch-hello (buffer)
  (let ((point (current-point)))
    (insert-string point (format nil "Saved searches:~%~%"))
    (dolist (saved-search *notmuch-saved-searches*)
      (destructuring-bind (&key name query &allow-other-keys)
	  saved-search
	(let ((count (notmuch-count query)))
	    
	  (lem/button:insert-button point
				    name
				    (lambda ()  (notmuch-run-saved-query name query))
				    :attribute 'notmuch-saved-search-attribute)

	  (insert-string point (format nil ": ~A" count))
	  (insert-character point #\newline 1)
	  )
	  
	  
	))
    (setf (buffer-read-only-p buffer) t)
    ))


;; fixme/unused don't use symbol-string-at-point, deprecate for button
(define-command notmuch-open-saved-query (&optional (point (current-point))) ()
  (let* ((name (string (symbol-string-at-point point)))
	 (query (find-saved-search name)))
    (notmuch-run-saved-query name (or query (editor-error "Could not find query for ~S" name)))))


;(define-key *notmuch-hello-mode-keymap* "Return" 'lem-notmuch:notmuch-open-saved-query)

(define-key *notmuch-hello-mode-keymap* "q" 'quit-active-window)

