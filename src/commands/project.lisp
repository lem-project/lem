(defpackage :lem-core/commands/project
  (:use :cl :lem-core :lem-core/commands/file)
  (:export :*root-directories*
           :*root-files*
           :root-p
           :find-root
           :project-find-file)
  (:documentation "Defines utilities to find a project root directory and the command project-find-file."))

(in-package :lem-core/commands/project)

(defvar *root-directories*
  (list
   ".git"
   ".hg"
   "_FOSSIL_"
   ".bzr"
   "_darcs"
   )
  "A list of directory names to be seen as project roots.
  Sans trailing slash.")

(defvar *root-files*
  (list
   ".project"
   ".projectile"
   ".fslckout"
   "TAGS"
   "configure.ac"
   "configure.in"
   ".ignore"
   "Makefile"
   )
  "A list of files to be seen as project roots.")

(defun parent-directory (pathname)
  "Return the parent directory (pathname)."
  (uiop:pathname-parent-directory-pathname pathname))

(defun add-directory-separator (pathname)
  "Add a directory separator, according to the host: a slash on Linux, something else on other popular platforms."
  ;; a little back and force from a pathname, to a string, back to a pathname...
  (pathname
   (str:concat pathname (string (uiop:directory-separator-for-host)))))

(defun list-file-names (pathname)
  "List files in this `pathname'.
  Return a list of file names, as strings, sans the full path.

  (list-file-names \"./\")
  => (README.md lem.asd ...)"
  (mapcar #'file-namestring
          (uiop:directory-files pathname)))

(defun list-subdirectories (pathname)
  "Return a list of subdirectories (pathnames)."
  (uiop:subdirectories pathname))

(defun root-p (pathname)
  "Return t if this directory contains any of the stop files or directories."
  (or (found-subdirectory-root-p pathname)
      (found-file-root-p pathname)))

(defun found-file-root-p (pathname)
  "Return t if this pathname contains any root markers from `*root-files*'."
  (when (intersection *root-files* (list-file-names pathname) :test #'equalp)
    t))

(defun found-subdirectory-root-p (pathname)
  "Return t if this pathname contains any directory markers from `*root-directories*'."
  ;; Here we work with pathnames, not strings.
  (loop for marker in *root-directories*
        with subdirectories = (list-subdirectories pathname)
        ;; To use merge-pathnames, our subdirectory must end with a trailing /
        for subpath = (uiop:merge-pathnames*
                       ;; (try to) handle non-unix platforms path separators.
                       (add-directory-separator marker) pathname)
        if (member subpath subdirectories :test #'equal)
          return t
        finally (return)))

(defvar *recurse* t)

(defun find-root (pathname &key (recurse *recurse*))
  "Search for a root directory up the file hierarchy.
  Return a pathname, or NIL.

  recurse: if non T, do not recurse in parent directories (testing only)."
  (cond
    ;; Stop at /
    ((or (equal (directory-namestring pathname)
                "/")
         ;; XXX: windows??
         (equal (directory-namestring pathname)
                (string (uiop:directory-separator-for-host))))
     nil)
    ;; Stop at HOME.
    ((equal (directory-namestring pathname)
            (directory-namestring (user-homedir-pathname)))
     nil)
    ;; Stop if found a directory root.
    ((found-subdirectory-root-p pathname)
     pathname)
    ;; Stop if found a file root.
    ((found-file-root-p pathname)
     pathname)
    ;; Go up to the parent directory.
    (t
     (when recurse
       (find-root (parent-directory pathname) :recurse recurse)))))

(define-command project-find-file (arg) ("p")
  "Open a file, from the list of all files in this project."
  ;; ARG is currently not used, use it when needed.
  (declare (ignorable arg))
  (let* ((cwd (buffer-directory))
         (project-root (find-root cwd))
         (root (or project-root cwd)))
    (uiop:with-current-directory (root)
      (let ((filename (prompt-for-files-recursively))
            buffer)
        (when filename
          (setf buffer (execute-find-file *find-file-executor*
                                          (get-file-mode filename)
                                          filename))
          (when buffer
            (switch-to-buffer buffer t nil)))))))
