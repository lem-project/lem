(in-package :lem)

(export '(*auto-mode-alist*))

(defvar *auto-mode-alist* nil)

(defun scan-line-property-list (buffer str)
  (ppcre:do-register-groups (var val)
      ("([a-zA-Z0-9-_]+)\\s*:\\s*([^ ;]+);?" str)
    (cond ((string= (string-downcase var) "mode")
           (let ((mode (find-mode-from-name val)))
             (when mode
               (change-buffer-mode buffer mode))))
          (t
           (setf (get-bvar :file-property-list :buffer buffer)
                 (cons (cons (string-downcase var) val)
                       (get-bvar :file-property-list :buffer buffer)))))))

(defun scan-file-property-list (buffer)
  (with-point ((cur-point (buffer-point buffer)))
    (buffer-start cur-point)
    (when (ppcre:scan "^#!" (line-string-at cur-point))
      (line-offset cur-point 1))
    (loop :until (end-line-p cur-point)
       :for string := (line-string-at cur-point)
       :do (ppcre:register-groups-bind (result)
	       ("-\\*-(.*)-\\*-" string)
	     (when result
	       (scan-line-property-list buffer result)
	       (return)))
       :do (if (string= "" (string-trim '(#\space #\tab) string))
	       (line-offset cur-point 1)
	       (return)))))

(defun prepare-auto-mode (buffer)
  (let* ((filename (file-namestring (buffer-filename buffer)))
         (elt (find-if (lambda (elt)
                         (ppcre:scan (car elt) filename))
                       *auto-mode-alist*)))
    (when elt
      (change-buffer-mode buffer (cdr elt)))))

(add-hook 'find-file-hook
          (lambda (buffer)
            (prepare-auto-mode buffer)
            (scan-file-property-list buffer)))

(add-hook 'before-save-hook
          (lambda (buffer)
            (scan-file-property-list buffer)))
