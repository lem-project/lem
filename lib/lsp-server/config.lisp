(cl-lsp/defpackage:defpackage :cl-lsp/config
  (:use :cl)
  (:export :with-environment
           :config))
(in-package :cl-lsp/config)

(defconstant +user-package-name+ :cl-lsp/config-user)

(defparameter *config-pathname* (merge-pathnames ".cl-lsp.lisp" (user-homedir-pathname)))

(defvar *environment* nil)
(defvar *cache* nil)

(defstruct cache
  (pathname *config-pathname*)
  date
  value)

(defun alive-cache-p (&optional (cache *cache*))
  (and cache
       (let ((pathname (cache-pathname cache)))
         (if (not (uiop:file-exists-p pathname))
             nil
             (eql (cache-date cache)
                  (file-write-date pathname))))))

(defun update-cache (value date)
  (setf *cache* (make-cache :value value :date date)))

(defun get-cached-value ()
  (and *cache* (cache-value *cache*)))

(defun config-plist-without-cache ()
  (if (not (uiop:file-exists-p *config-pathname*))
      '()
      (let ((*package* (or (find-package +user-package-name+)
                           (make-package +user-package-name+ :use '(:common-lisp)))))
        (uiop:read-file-form *config-pathname*))))

(defun config-plist ()
  (when (uiop:file-exists-p *config-pathname*)
    (if (alive-cache-p)
        (get-cached-value)
        (let ((plist (config-plist-without-cache)))
          (update-cache plist (file-write-date *config-pathname*))
          plist))))

(defun config (&rest keys)
  (let ((plist (config-plist)))
    (when *environment*
      (setf plist (getf plist *environment*)))
    (dolist (key keys)
      (setf plist (getf plist key)))
    plist))

(defmacro with-environment (environment &body body)
  `(let* ((*environment* ,environment)
          (bt:*default-special-bindings*
            (acons '*environment*
                   *environment*
                   bt:*default-special-bindings*)))
     ,@body))
