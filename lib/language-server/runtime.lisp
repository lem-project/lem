(in-package :lem-language-server)

(defun asd-pathname-p (file)
  (equal "asd" (pathname-type file)))

(defun lisp-pathname-p (file)
  (equal "lisp" (pathname-type file)))

(defun root-directory-p (directory)
  (uiop:pathname-equal directory (user-homedir-pathname)))

(defun find-asd-files (pathname)
  (let ((directory (uiop:pathname-directory-pathname pathname)))
    (labels ((recursive (directory)
               (if (root-directory-p directory)
                   nil
                   (append (remove-if-not #'asd-pathname-p (uiop:directory-files directory))
                           (recursive (uiop:pathname-parent-directory-pathname directory))))))
      (recursive directory))))

(defun collect-defsystem-name (pathname)
  (with-open-file (in pathname)
    (let ((readtable *readtable*)
          (asdf-package (find-package :asdf-user))
          (print-pprint-dispatch *print-pprint-dispatch*))
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*readtable* readtable)
              (*print-pprint-dispatch* print-pprint-dispatch)
              (*package* asdf-package)
              (*default-pathname-defaults*
                (uiop:pathname-directory-pathname (uiop:physicalize-pathname pathname))))
          (uiop:with-safe-io-syntax (:package asdf-package)
            (loop :with eof-value := '#:eof-value
                  :for form := (read in nil eof-value)
                  :until (eq form eof-value)
                  :when (and (consp form)
                             (eq (first form) 'asdf:defsystem))
                  :collect (second form))))))))

(defun on-load (filename)
  (let ((asd-files (find-asd-files filename)))
    (cond (asd-files
           (let ((system-names (loop :for asd-file :in asd-files
                                     :append (collect-defsystem-name asd-file))))
             (log:info "load systems" system-names)
             (micros/client:remote-eval-sync (server-backend-connection *server*)
                                             `(micros/lsp-api:load-systems ',system-names)
                                             :package-name "CL-USER")))
          ((lisp-pathname-p filename)
           (log:info "compile and load file" filename)
           (micros/client:remote-eval-sync (server-backend-connection *server*)
                                           `(micros/lsp-api:compile-and-load-file ,filename)
                                           :package-name "CL-USER")))))
