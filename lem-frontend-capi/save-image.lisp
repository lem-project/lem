(in-package :cl-user)

(load-all-patches)

(lw:set-default-character-element-type 'cl:character)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(namestring (probe-file "../")))
   :INHERIT-CONFIGURATION))

(uiop:symbol-call :ql :quickload :lem-capi)

(lem:add-hook lem:*exit-editor-hook*
              (lambda () (lw:quit)))

(defun run-lem ()
  (apply #'lem:lem (rest sys:*line-arguments-list*)))

(push '("lem" (:priority 60000000 :restart-action :continue) run-lem)
      mp:*initial-processes*)

(save-image "lem"
            :console t
            :multiprocessing t
            :environment nil)
