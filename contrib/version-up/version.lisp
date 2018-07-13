(uiop/package:define-package :lem-version-up/version (:use :cl))
(in-package :lem-version-up/version)
;;;don't edit above

(defun path (target)
  (let ((path (ql:where-is-system target)))
    (and path
         (probe-file (merge-pathnames (format nil "~A.asd" target) path)))))

(defun get-version (target)
  (mapcar #'parse-integer
          (uiop:split-string
           (with-open-file (in (path target))
             (let ((*load-pathname* (path target)))
               (getf (cddr (read in)) :version)))
           :separator '(#\.))))

(defvar *candidates* '("roswell" "edit"))

(defun inclement-version (type target &optional (version (get-version target)))
  ;; [tbd] replace algorithm here?
  (cond ((equal type "roswell")
         (let ((d (multiple-value-list(decode-universal-time (get-universal-time)))))
           (setf (first version) (- (nth 5 d) 2000)
                 (second version) (nth 4 d)
                 (fourth version) (1+ (fourth version)))))
        (t version)))

(defun version-string (version)
  (format nil "~{~A~^.~}" version))

(defun rewrite.asd (version target)
  (let* ((path (path target))
         (lines (loop for line in (uiop:read-file-lines path)
                      collect (if (eql (ignore-errors (read-from-string line)) :version)
                                  (format nil "~A~S"
                                          (subseq line 0 (position #\" line))
                                           version)
                                  line))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for line in lines
            do (format out "~A~%" line)))))

(defun rewrite-package.json (version target)
  (let ((path (probe-file (make-pathname :defaults (path target) :type "json" :name "package"))))
    (when path
      (let ((lines (loop with done
                         for line in (uiop:read-file-lines path)
                         collect (if (and (not done)
                                          (equal (ignore-errors (read-from-string line)) "version"))
                                     (setq done (format nil "~A~S,"
                                                        (subseq line 0 (+ 2(position #\: line)))
                                                        version))
                                     line))))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (loop for line in lines
                do (format out "~A~%" line)))))))

(defun rewrite-configure.ac (version target)
  (let ((path (probe-file (make-pathname :defaults (path target) :type "ac" :name "configure"))))
    (when path
      (let ((lines (loop for line in (uiop:read-file-lines path)
                         for match- = (nth-value 1 (cl-ppcre:scan-to-strings "^AC_INIT\\([^,]*,\\[([^,]*)\\]" line))
                         collect (if match-
                                     (cl-ppcre:regex-replace "^(AC_INIT\\([^,]*,\\[)([^,]*)(\\].*)$" line
                                                             (format nil "\\{1}~A\\{3}" version))
                                     line))))
        (with-open-file (out path :direction :output :if-exists :supersede)
          (loop for line in lines
                do (format out "~A~%" line)))))))

(lem:define-command !version-up () ()
  (let* ((target (lem:prompt-for-string "target?:" "lem"))
         (old-version (version-string (get-version target)))
         (type (lem:prompt-for-line
                (format nil "~A version (~A):" target old-version)
                ""
                (lambda (str) (lem:completion-strings str *candidates*))
                (lambda (name)
                  (member name *candidates* :test #'string=))
                'lem-version)))
    (and type
         target
         (let ((version (inclement-version type target)))
           (setf version (lem:prompt-for-string (format nil "(was:~A)OK?:" old-version) (version-string version)))
           (rewrite.asd version target)
           (rewrite-package.json version target)
           (rewrite-configure.ac version target)
           (lem:message "~A updated to ~A" target version)))))
