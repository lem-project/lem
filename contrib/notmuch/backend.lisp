(in-package :lem-notmuch)

(defvar *notmuch-executable* "notmuch")

(defun run-notmuch (query)
  (multiple-value-bind (str err retval)
      (uiop:run-program (format nil "~A ~A" *notmuch-executable* query)
			:output :string
			:error-output :string
			:ignore-error-status t)
    
    (if (= retval 0)
	str
	(editor-error "`notmuch ~A` returned ~A ~A ~A" query retval str err))))

;; fixme-- is this the right serialization format?
(defun parse-notmuch-search (result)
  (read-from-string result))

(defun parse-notmuch-search (result)
  (read-from-string result))

(defun parse-notmuch-show (result)
  (read-from-string result))

(defun notmuch-search (query &key oldest-first)
  (parse-notmuch-search
      (run-notmuch (format nil "search --format=sexp --format-version=5 --sort=~A ~S"
				(if oldest-first "oldest-first" "newest-first")
				query))))

(defun notmuch-show (query &key entire-thread decrypt)
  (parse-notmuch-show
      (run-notmuch (format nil "show --format=sexp --format-version=5 --entire-thread=~A --decrypt=auto ~S"
				(if entire-thread "true" "false")
				query))))


(defun notmuch-count (query)
  (parse-integer
   (run-notmuch (format nil "count ~S" query))))

