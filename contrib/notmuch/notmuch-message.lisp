
(in-package :lem-notmuch)

(define-major-mode notmuch-message-mode ()
    (:name "notmuch-message"
     :keymap *notmuch-message-mode-keymap*))

(defun notmuch-print-message (email)
  (destructuring-bind (&key id content-type content filename tags body crypto headers &allow-other-keys)
      email
    (destructuring-bind (&key from subject to date &allow-other-keys)
	headers
      (let ((point (current-point)))
	(insert-string point (format nil "From: ~A~%Subject: ~A~%To: ~A~%Date: ~A~%~%"
				     from
				     subject
				     to
				     date))
	(insert-string point (format nil "~A" body))
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
    (dolist (email-message results)
      (when email-message
	(notmuch-print-message email-message)))
    (setf (buffer-read-only-p buffer) t)
    ))


  (define-key *notmuch-message-mode-keymap* "q" 'quit-active-window)

