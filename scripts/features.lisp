(ignore-errors
  (let* ((features (uiop:getenv "LISPFEATURES"))
	 (feature-list (read-from-string features)))
    (when (listp feature-list)
      (loop for feature in feature-list
	    do (pushnew feature *features*)))))
