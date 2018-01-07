(in-package :lem)

(defvar *site-init-name* "lem-site-init")
(defvar *site-init-comment ";; don't edit !!!")

(defun site-init-path ()
  (let ((path (merge-pathnames (format nil ".lem/~A.asd"
                                       *site-init-name*)
                               (user-homedir-pathname))))
    (with-open-file (out (ensure-directories-exist path)
                         :direction :output
                         :if-exists nil)
      (format out "~A~%~(~S~)~%"
              *site-init-comment
              `(asdf:defsystem ,*site-init-name*)))
    path))

(defun load-site-init ()
  (let* ((asdf:*central-registry*
           (union (mapcar #'pathname
                          (mapcar #'directory-namestring
                                  (directory
                                   (merge-pathnames "**/*.asd"
                                                    (merge-pathnames ".lem/"
                                                                     (user-homedir-pathname))))))
                  asdf:*central-registry*
                  :test #'equal))
         (system-name *site-init-name*)
         (key (intern (string-upcase system-name) :keyword)))
    (unless (find key *features*)
      (pushnew key *features*)
      (asdf:load-asd (site-init-path))
      (let ((*package* (find-package :lem-user)))
        #+quicklisp(ql:quickload system-name :silent t)))))

(defun site-init ()
  (with-open-file (i (site-init-path))
    (let ((*package* (find-package :lem-user)))
      (read i))))

(defun (setf site-init) (exp)
  (with-open-file (o (site-init-path) :direction :output :if-exists :supersede)
    (let ((*package* (find-package :lem-user)))
      (format o "~A~%~(~S~)" *site-init-comment exp))))

(define-command site-init-add-dependency (symbols) ("sPackages:")
  "Input system name and test it's loadable."
  (loop :with site-init := (site-init)
        :with depends-on := (getf (cddr site-init) :depends-on)
        :for s :in (loop :for str = (format nil "~a " symbols) then (subseq str (1+ pos))
                         :for pos := (position #\Space str)
                         :while pos
                         :collect (subseq str 0 pos))
        :for key := (read-from-string (format nil ":~A" s))
        :do (unless (find key depends-on)
              #+quicklisp(ql:quickload key :silent t)
              (push key depends-on))
        :finally (setf (getf (cddr site-init) :depends-on) depends-on)
                 (setf (site-init) site-init)
                 (message "~A" depends-on)))

(define-command site-init-remove-dependency (symbols) ("sPackage:")
  "Remove system name from site-init depends-on"
  ;;TBD prepare prompt-site-init-depends-on like function
  (loop :with site-init := (site-init)
        :with depends-on := (getf (cddr site-init) :depends-on)
        :for s :in (loop :for str = (format nil "~a " symbols) then (subseq str (1+ pos))
                         :for pos := (position #\Space str)
                         :while pos
                         :collect (subseq str 0 pos))
        :for key := (read-from-string (format nil ":~A" s))
        :do (when (find key depends-on)
              (setf depends-on (remove key depends-on)))
        :finally (setf (getf (cddr site-init) :depends-on) depends-on)
                 (setf (site-init) site-init)
                 (message "~A" depends-on)))

;; TBD prepare some commands to edit asd file.
;; M-x site-init-edit-dependency / prepare buffer one system in a line which are editable. test loadable when save and update asd.
;; M-x site-init-add / prepare a file which load.
;; M-x site-init-edit / open dired like interface "~/.lem/inits/"
