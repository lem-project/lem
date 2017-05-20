(in-package :lem)

(export '(*auto-mode-alist*))

(defvar *auto-mode-alist* nil)

(defun scan-line-property-list (buffer str)
  (ppcre:do-register-groups (var val)
      ("([a-zA-Z0-9-_]+)\\s*:\\s*([^ ;]+);?" str)
    (cond ((string-equal var "mode")
           (let ((mode (find-mode-from-name val)))
             (when mode
               (change-buffer-mode buffer mode))))
          (t
           (setf (buffer-value buffer (string-downcase var)) val)))))

(defun scan-file-property-list (buffer)
  (with-point ((cur-point (buffer-point buffer)))
    (buffer-start cur-point)
    (when (ppcre:scan "^#!" (line-string cur-point))
      (line-offset cur-point 1))
    (loop :until (end-line-p cur-point)
       :for string := (line-string cur-point)
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

(defun detect-external-format-from-file (pathname)
  (values (inq:detect-encoding (pathname pathname) :jp)
          (or (inq:detect-end-of-line (pathname pathname)) :lf)))

(setf *external-format-function* 'detect-external-format-from-file)
