(defpackage :lem/simple-package
  (:use :cl :lem))

(in-package :lem/simple-package)

(defparameter *installed-packages* nil)

(defparameter *packages-directory*
  (pathname (str:concat
             (directory-namestring (lem-home))
             "packages"
             (string  (uiop:directory-separator-for-host)))))

(defstruct source name)

(defgeneric download-source (source output-location)
  (:documentation "It downloads the SOURCE to the desired location."))

;; From porcelain.lisp
(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(defun run-git (arglist)
  (uiop:wait-process
   (uiop:launch-program (concatenate 'list *git-base-arglist* arglist)
                        :ignore-error-status t)))

(defstruct (git (:include source)) url commit)

(defmethod download-source ((source git) (output-location String))
  (run-git (list "clone" (git-url source)
                 (str:concat
                  (namestring *packages-directory*) output-location))))

(defmethod download-source (source output-location)
  (editor-error "Source ~a not available." source))

;; source (list type url commit)

(defclass simple-package ()
  ((name :initarg :name
         :accessor simple-package-name)
   (source :initarg :source
           :accessor simple-package-source)))

(defun packages-list ()
  (mapcar #'(lambda (d) (pathname (directory-namestring d)))
          (directory (merge-pathnames "**/*.asd" *packages-directory*))))

(defmacro lem-use-package (name &key source config
                                 after bind
                                 hooks force)
  (declare (ignore hooks bind after config ))
  (labels ((dfsource (source-list)
             (let ((s (car source-list)))
               (ecase s
                 (:git
                  (destructuring-bind (_ url commit)
                      source-list
                    (declare (ignore _ commit))
                      (make-git :name name
                                :url url)))
                 (t (editor-error "Source ~a not available." s))))))
    (alexandria:with-gensyms (spackage rsource pdir)
      `(let* ((asdf:*central-registry*
                (union (packages-list)
                       asdf:*central-registry*
                       :test #'equal))
              (,rsource ,(dfsource source))
              (,spackage (make-instance 'simple-package
                                        :name ,name
                                        :source ,rsource))
              (,pdir (merge-pathnames *packages-directory* ,name)))
         (when (or ,force
                   (not (uiop:directory-exists-p ,pdir)))
           (message "Downloading ~a..." ,name)
           (download-source ,rsource ,name)
           (message "Done downloading ~a!" ,name))
         (pushnew ,spackage *installed-packages*
                  :test #'(lambda (a b)
                            (string=
                             (simple-package-name a)
                             (simple-package-name b))))
         (maybe-quickload (alexandria:make-keyword ,name)
                          :silent t)))))

;;(lem-use-package "lem-pareto" :source (:git "https://github.com/40ants/lem-pareto.git" nil))

;;(lem-use-package "modf" :source (:git "https://github.com/smithzvk/modf.git" nil))
