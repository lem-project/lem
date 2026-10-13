(in-package :lem-core)

;; The Windows executable is built as a GUI-subsystem application
;; (deploy's default on Windows), so a process launched from Explorer has
;; no console and the initial stdio handles are invalid; the first write
;; to *standard-output*/*error-output* would kill the writing thread.
;; Deploy repairs the streams only when it can attach to a parent console,
;; and its warm boot writes status output before any :boot hook runs, so
;; this must be an SBCL init hook that runs before the toplevel function.
#+sbcl
(progn
  (defun redirect-invalid-windows-stdio ()
    (when (and (deploy:deployed-p)
               (cffi:null-pointer-p
                (cffi:foreign-funcall "GetConsoleWindow" :pointer)))
      (let ((sink (or (ignore-errors
                        (open (merge-pathnames "lem-stdio.log"
                                               (uiop:temporary-directory))
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8))
                      (make-broadcast-stream))))
        (setf sb-sys:*stdout* sink
              sb-sys:*stderr* sink
              sb-sys:*stdin* (make-concatenated-stream)))))
  (pushnew 'redirect-invalid-windows-stdio sb-ext:*init-hooks*))

;; Tell the deploy library not to bundle OS-provided DLLs.  async-process
;; and winhttp declare them via cffi:define-foreign-library, which makes
;; deploy treat them as application libraries, but they are part of every
;; Windows installation and must be loaded from System32.
(unless (deploy:deployed-p)
  (loop :for (package-name symbol-name) :in '((:async-process "KERNEL32")
                                              (:winhttp "WINHTTP"))
        :for package := (find-package package-name)
        :for symbol := (and package (find-symbol symbol-name package))
        :when symbol
        :do (setf (deploy:library-dont-deploy-p (deploy:ensure-library symbol)) t)))

;; Tell the deploy library not to bundle tree-sitter native libraries.
;; They are optional and loaded at runtime only when available.
;; Only relevant while building; skip when running inside the deployed
;; binary so we don't mutate deploy state on every startup.
;; (Same workaround as in macosx.lisp.)
(unless (deploy:deployed-p)
  (when (find-package :tree-sitter/ffi)
    (let ((ts (find-symbol "TREE-SITTER" :tree-sitter/ffi))
          (tw (find-symbol "TS-WRAPPER" :tree-sitter/ffi)))
      (when ts
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library ts)) t))
      (when tw
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library tw)) t)))))
