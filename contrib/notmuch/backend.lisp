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

(defun notmuch-search (query &key oldest-first)
  (yason:parse
      (run-notmuch (format nil "search --format=json --format-version=5 --sort=~A ~S"
				(if oldest-first "oldest-first" "newest-first")
				query))))

(defun notmuch-show (query &key entire-thread decrypt)
  (yason:parse
      (run-notmuch (format nil "show --format=json --format-version=5 --entire-thread=~A --decrypt=auto ~S"
				(if entire-thread "true" "false")
				query))))


(defun notmuch-count (query)
  (parse-integer
   (run-notmuch (format nil "count ~S" query))))

(defmacro with-notmuch-props (obj props &body body)
  (let ((sym (gensym)))
    `(let ((,sym ,obj))
       (let ,(loop for i in props
		    collect (list (first i) (list 'gethash (second i) sym))) ,@body))
    ))

; (with-notmuch-props ((a "hi") (b "bye")) (list 'x) (princ 5))

