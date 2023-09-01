
(in-package :lem-notmuch)

(define-major-mode notmuch-message-mode ()
    (:name "notmuch-message"
     :keymap *notmuch-message-mode-keymap*))

(defun print-message-body (point body)
  (dolist (item (alexandria:flatten body))
    (with-notmuch-props item
	((id "id")
	 (content-type "content-type")
	 (content "content"))
      (if (equal content-type "text/plain")
	  (progn
	    ;; fixme insert link to part
	    (insert-string point (format nil "~A~%" content)))
	  (progn
	    (insert-string point (format nil "[id: ~S, content type ~S]~%" id content-type))
	    )
	  )
      )
    )
  )

(defun notmuch-print-message (email)
  (with-notmuch-props email
      ((id "id")
       (content-type "content-type")
       (content "content")
       (filename "filename")
       (body "body")
       (headers "headers"))
    (with-notmuch-props headers
	((to "To")
	 (from "From")
	 (subject "Subject")
	 (date "Date"))
      (let ((point (current-point)))
	(insert-string point (format nil "From: ~A~%Subject: ~A~%To: ~A~%Date: ~A~%~%"
				     from
				     subject
				     to
				     date))
	(print-message-body point body)
	(insert-character point #\newline 1)
	)
      )))

(defun notmuch-open-message (subject query)
  (let ((buffer (make-buffer (format nil "*~A%*" subject)))
	;; (first query) is the query that matches the message
	(results (car (car (notmuch-show (first query) :decrypt t :entire-thread nil)))))
    (switch-to-buffer buffer)
    (notmuch-message-mode)
    (erase-buffer buffer)
    (dolist (email-message (alexandria:flatten results))
      (when email-message
	(notmuch-print-message email-message)))
    (setf (buffer-read-only-p buffer) t)
    ))


  (define-key *notmuch-message-mode-keymap* "q" 'quit-active-window)

