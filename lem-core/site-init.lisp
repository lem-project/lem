(in-package :lem)

(defun prepare-siteinit ()
  (with-open-file (out (ensure-directories-exist (merge-pathnames ".lem/site-init.asd" (user-homedir-pathname)))
                       :direction :output
                       :if-exists nil)
    (format out "~A~%~A~%"
            ";; don't edit !!!"
            '("defsystem" "\"lem.site.init\""))))

(let ((asdf:*central-registry*
       (union (mapcar #'pathname
                      (mapcar #'directory-namestring
                              (directory
                               (merge-pathnames "**/*.asd"
                                                (merge-pathnames ".lem/"
                                                                 (user-homedir-pathname))))))
              asdf:*central-registry*
              :test #'equal))
      (system-name "lem.site.init"))
  (unless (find :lem.siteinit *features*)
    (pushnew :lem.siteinit *features*)
    (cond ((asdf:find-system system-name nil)
           (let ((*package* (find-package :lem-user)))
             #-quicklisp(asdf:load-system system-name :verbose nil)
             #+quicklisp(ql:quickload system-name :silent t)))
          (t
           (prepare-siteinit)))))

;; TBD prepare some commands to edit asd file.
;; M-x site-add-dependency / input system name and test it's loadable.
;; M-x site-edit-dependency / prepare buffer one system in a line which are editable. test loadable when save and update asd.
;; M-x site-add-init / prepare a file which load.
;; M-x site-edit-init / open dired like interface "~/.lem/inits/"
