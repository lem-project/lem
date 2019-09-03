;; Build with `sbcl --load <this file>`.
(ql:quickload :lem-ncurses)

(unless (find-package :roswell)
  (ql:quickload :roswell)
  ;; If this script was run without Roswell, `*ros-opts*' won't be set properly,
  ;; so we need to set it here.
  (setf roswell::*ros-opts*
        `(("uname" ,(uiop:run-program "uname" :output '(:string :stripped t)))
          ("uname-m" ,(uiop:run-program '("uname" "-m") :output '(:string :stripped t))))))

(sb-ext:save-lisp-and-die (format nil "lem-ncurses-~A-~A" (roswell.util:uname) (roswell.util:uname-m))
                          :toplevel #'lem:main
                          :executable t
                          :compression t)
