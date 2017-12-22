(in-package :xcb)
;;=============================================================================
;; atom
;;
(defun easy-atom (c name &optional only-if-exists)
  (let ((atom-cookie
	 (xcb::intern-atom c (if only-if-exists 1 0)
			   (length name)
			   name)))
    (when atom-cookie
      (let ((reply (xcb::intern-atom-reply c atom-cookie
					   (null-pointer))))
	(prog1
	    (mem-ref reply :uint 8)
	  (foreign-free reply))))))

