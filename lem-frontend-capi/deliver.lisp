(in-package "CL-USER")

(lw:set-default-character-element-type 'cl:character)

(defun utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore buffer length))
  (if (assoc (pathname-type pathname)
             graphics-ports::*file-types-to-image-types*
             :test #'equalp)
      '(:latin-1)
      (system:merge-ef-specs ef-spec '(:utf-8 :use-replacement t))))

(setq system:*file-encoding-detection-algorithm* '(utf-8-file-encoding))

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(load-all-patches)
(uiop:symbol-call :ql :quickload :lem-capi)

(deliver (find-symbol "LEM" :lem)
         "lem"
         0
         :interface :capi)
