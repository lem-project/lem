(defpackage :lem/simple-package
  (:use :cl :lem)
  (:export :*installed-packages*
           :*packages-directory*
           :lem-use-package
           :load-packages))

(in-package :lem/simple-package)

(defvar *installed-packages* nil)

(defvar *packages-directory*
  (pathname (str:concat
             (directory-namestring (lem-home))
             "packages"
             (string  (uiop:directory-separator-for-host)))))

(defstruct source name)

(defstruct (local (:include source)))

(defgeneric download-source (source output-location)
  (:documentation "It downloads the SOURCE to the desired location."))

(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(defun run-git (arglist)
  (uiop:wait-process
   (uiop:launch-program (concatenate 'list *git-base-arglist* arglist)
                        :ignore-error-status t)))

(defstruct (git (:include source)) url branch commit)

(defmethod download-source ((source git) (output-location String))
  (let ((output-dir (str:concat
                     (namestring *packages-directory*) output-location)))
    (run-git (list "clone" (git-url source) output-dir))
    (when (git-branch source)
      (uiop:with-current-directory (output-dir)
        (run-git (list "checkout" "-b" (git-branch source)))))

    (when (git-commit source)
      (uiop:with-current-directory (output-dir)
        (run-git (list "checkout" (git-commit source)))))
    output-dir))

(defstruct (quicklisp (:include source)))

(defvar *quicklisp-system-list*
  (remove-duplicates
   (mapcar #'ql-dist:release (ql:system-list))))

(defmethod download-source ((source quicklisp) (output-location String))
  (let* ((output-dir (str:concat
                     (namestring *packages-directory*) output-location))
         (release (find (source-name source)
                       *quicklisp-system-list*
                       :key #'ql-dist:project-name
                       :test #'string=))
         (url (ql-dist:archive-url release))
         (name (source-name source))
         (tgzfile (str:concat name ".tgz"))
         (tarfile (str:concat name ".tar")))
    (if release
        (prog1 output-dir
          (uiop:with-current-directory (*packages-directory*)
            (quicklisp-client::maybe-fetch-gzipped url tgzfile
                                                   :quietly t)
            (ql-gunzipper:gunzip tgzfile tarfile)
            (ql-minitar:unpack-tarball tarfile)
            (delete-file tgzfile)
            (delete-file tarfile)
            (uiop/cl:rename-file (ql-dist:prefix release) output-location)))
        (editor-error "Package ~a not found!." (source-name source)))))

(defmethod download-source (source output-location)
  (editor-error "Source ~a not available." source))

(defclass simple-package ()
  ((name :initarg :name
         :accessor simple-package-name)
   (source :initarg :source
           :accessor simple-package-source)
   (directory :initarg :directory
              :accessor simple-package-directory)))

(defgeneric package-remove (package))

(defmethod package-remove ((package simple-package))
  (uiop:delete-directory-tree
   (uiop:truename* (simple-package-directory package)) :validate t)
  (delete package *installed-packages*))

(defgeneric package-test (package))

(defmethod package-test ((package simple-package))
  (let* ((*packages-directory* (uiop:temporary-directory))
         (ql:*local-project-directories* (list *packages-directory*))
         (name (simple-package-name package))
         (source (simple-package-source package)))
    (%download-package source name)
    (%register-maybe-quickload (simple-package-name package))))

(defun packages-list ()
  (remove-duplicates
   (mapcar (lambda (d) (pathname (directory-namestring d)))
           (directory (merge-pathnames "**/*.asd" *packages-directory*)))))

(defun insert-package (package)
  (pushnew package *installed-packages*
           :test (lambda (a b)
                   (string=
                    (simple-package-name a)
                    (simple-package-name b)))))

(defun define-source (source-list name)
  (let ((s (getf source-list :type)))
    (ecase s
      (:git
       (destructuring-bind (&key type url branch commit)
           source-list
         (declare (ignore type))
         (make-git :name name
                   :url url
                   :branch branch
                   :commit commit)))
      (:quicklisp
       (destructuring-bind (&key type)
           source-list
         (declare (ignore type))
         (make-quicklisp :name name)))
      (t (editor-error "Source ~a not available." s)))))

(defun %register-maybe-quickload (name)
  (uiop:symbol-call :quicklisp :register-local-projects)
  (maybe-quickload (alexandria:make-keyword name) :silent t))

(defun %download-package (source name)
  (message "Downloading ~a..." name)
  (download-source source name)
  (message "Done downloading ~a!" name))

;; git source (list :type type :url url :branch branch :commit commit)
(defmacro lem-use-package (name &key source after
                                 bind hooks force)
  (declare (ignore hooks bind after))
  (alexandria:with-gensyms (spackage rsource pdir)
    `(let* ((asdf:*central-registry*
                (union (packages-list)
                       asdf:*central-registry*
                       :test #'equal))
              (ql:*local-project-directories*
                (nconc (list *packages-directory*)
                       ql:*local-project-directories*))
              (,rsource (define-source ,source ,name))
              (,pdir (merge-pathnames *packages-directory* ,name))
              (,spackage (make-instance 'simple-package
                                        :name ,name
                                        :source ,rsource
                                        :directory ,pdir)))
         (when (or ,force
                   (not (uiop:directory-exists-p ,pdir)))
           (%download-package ,rsource ,name))
         (insert-package ,spackage)
       (and (%register-maybe-quickload ,name) t))))

;(lem-use-package "versioned-objects"
;                 :source '(:type :git
;                           :url "https://github.com/smithzvk/Versioned-Objects.git"
;                           :branch "advance-versioning"))

;(lem-use-package "fiveam" :source (:type :quicklisp))

;; Package util functions/commands


(defun load-packages ()
  (let ((ql:*local-project-directories* (list *packages-directory*)))
    (loop for dpackage in (directory (merge-pathnames "*/" *packages-directory*))
          for spackage = (car
                          (last
                           (pathname-directory
                            (uiop:directorize-pathname-host-device dpackage))))
          do (insert-package
              (make-instance 'simple-package
                             :name spackage
                             :source (make-local :name spackage)
                             :directory dpackage))
          do (maybe-quickload (alexandria:make-keyword spackage) :silent t))))

(defun %select-ql-package ()
  (let* ((packages (mapcar #'ql-dist:project-name
                           *quicklisp-system-list*)))
    (prompt-for-string "Select package: "
                       :completion-function
                       (lambda (string)
                         (completion string packages)))))

(define-command sp-test-ql-package () ()
  (alexandria:if-let ((lpackage (%select-ql-package)))
    (progn (package-test
            (make-instance 'simple-package
                           :name lpackage
                           :source (make-quicklisp :name lpackage))))
    (editor-error "There was an error loading ~a!" lpackage)))

(define-command sp-install-ql-package () ()
  (let* ((lpackage (%select-ql-package)))

    (lem-use-package lpackage :source '(:type :quicklisp))
    (message "Package ~a installed!" lpackage)))

(define-command sp-remove-package () ()
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


(define-command sp-purge-packages () ()
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
