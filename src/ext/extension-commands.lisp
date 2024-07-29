(defpackage :lem/extension-commands
  (:use :cl :lem-extension-manager
        :lem :ql)
  #+sbcl
  (:lock t)
  (:export))

(in-package :lem/extension-commands)

(defun %select-ql-package ()
  (let* ((packages (mapcar #'ql-dist:short-description
                           (system-list))))
    (prompt-for-string "Select package: "
                       :completion-function
                       (lambda (string)
                         (completion string packages)))))

(define-command extension-manager-test-ql-package () ()
  (alexandria:if-let ((lpackage (%select-ql-package)))
    (progn (package-test
            (make-instance 'simple-package
                           :name lpackage
                           :source (make-quicklisp :name lpackage))))
    (editor-error "There was an error loading ~a!" lpackage)))

(define-command extension-manager-install-ql-package () ()
  (let* ((lpackage (%select-ql-package)))

    (lem-use-package lpackage :source '(:type :quicklisp))
    (message "Package ~a installed!" lpackage)))

(define-command extension-manager-remove-package () ()
  (if *installed-packages*
      (let* ((packages (and *installed-packages*
                            (mapcar #'simple-package-name
                                    *installed-packages*)))
             (rpackage
               (prompt-for-string "Select package: "
                                  :completion-function
                                  (lambda (string)
                                    (completion string packages)))))
        (package-remove
         (find rpackage *installed-packages*
               :key #'simple-package-name
               :test #'string=))
        (message "Package remove from system!"))

      (message "No packages installed!")))


(define-command extension-manager-purge-packages () ()
  (let* ((plist (packages-list))
         (extra-packages
           (set-difference
            (mapcar (lambda (p)
                      (first (last (pathname-directory p))))
                    plist)
            (mapcar #'simple-package-name *installed-packages*)
            :test #'string=)))
    (loop for e in extra-packages
          for dir = (find e plist
                          :key (lambda (p) (first (last (pathname-directory p))))
                          :test #'string=)
          do (and (uiop:directory-exists-p dir)
                  (uiop:delete-directory-tree
                   (uiop:truename* dir)
                   :validate t)))))
