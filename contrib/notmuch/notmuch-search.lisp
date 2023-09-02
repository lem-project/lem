
(in-package :lem-notmuch)

(define-major-mode notmuch-search-mode ()
    (:name "notmuch-search"
     :keymap *notmuch-search-mode-keymap*))


(defun truncate-field (len str)
  (if (< len (length str))
      (concatenate 'string (subseq str 0 (- len 3)) "...")
      str))

(defun notmuch-run-saved-query (name query)
  (let ((buffer (make-buffer (format nil "*notmuch-saved-query-~A%" name)))
	(results (notmuch-search query :oldest-first t)))
    (switch-to-buffer buffer)
    (notmuch-search-mode)
    (erase-buffer buffer)
    (let ((point (current-point)))
      (dolist (item results)
	(when item
	  (with-notmuch-props item
	      ((thread "thread")
	       (date_relative "date_relative")
	       (matched "matched")
	       (total "total")
	       (authors "authors")
	       (query "query")
	       (subject "subject")
	       (tags "tags"))
	    (lem/button:insert-button
	     point
	     (format nil "~12@A ~8@A ~25@A ~A"
		     (truncate-field 12 date_relative)
		     (format nil "[~A/~A]" matched total)
		     (truncate-field 20 authors)
		     (truncate-field 60 subject))
	     (lambda () (notmuch-show-message subject query)))
	    (insert-character point #\newline 1)
	    ))
      )
    (setf (buffer-read-only-p buffer) t)
      )))


; todo
;(define-key *notmuch-search-mode-keymap* "Return" 'lem-notmuch:notmuch-search-show-thread)

(define-key *notmuch-search-mode-keymap* "q" 'quit-active-window)

