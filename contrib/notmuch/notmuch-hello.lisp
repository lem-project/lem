
(in-package :lem-notmuch)

(define-major-mode notmuch-hello-mode ()
    (:name "notmuch-hello"
     :keymap *notmuch-hello-mode-keymap*))

(defun find-saved-search (name-to-find)
  (dolist (item *notmuch-saved-searches*)
    (destructuring-bind (&key name query &allow-other-keys)
	item
      (when (string-equal name-to-find name)
	(return-from find-saved-search query)))))


(defun notmuch-run-saved-query (name query)
  (let ((buffer (make-buffer (format nil "*notmuch-saved-query-~A%" name)))
	(results (notmuch-search query :oldest-first t)))
    (switch-to-buffer buffer)
    (notmuch-hello-mode)
    (erase-buffer buffer)
    (let ((point (current-point)))
      (dolist (item results)
	(destructuring-bind (&key thread date_relative matched total authors query subject query tags &allow-other-keys)
	    item
	    #+nil(lem/button:insert-button point (or thread "blah") (lambda () (with-context (notmuch-open-message thread query))))

	    (insert-string point (format nil "~20@A [~A/~A] ~20@A ~A" date_relative matched total authors subject))
	    (insert-character point #\newline 1)
	)
      )
    (setf (buffer-read-only-p buffer) t)
  )))


;; fixme don't use symbol-string-at-point
(define-command notmuch-open-saved-query (&optional (point (current-point))) ()
  (let* ((name (string (symbol-string-at-point point)))
	 (query (find-saved-search name)))
    (notmuch-run-saved-query name (or query (editor-error "Could not find query for ~S" name)))))


(define-key *notmuch-hello-mode-keymap* "Return" 'lem-notmuch:notmuch-open-saved-query)

(define-key *notmuch-hello-mode-keymap* "q" 'quit-active-window)

