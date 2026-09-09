;; Windows deployment build: produce bin/lem.exe (webview frontend, GUI
;; subsystem) plus all required DLLs via the deploy library (deploy-op
;; declared in lem.asd).
;;
;; Run through scripts/win-deploy.ps1, which first embeds the application
;; icon into a copy of the SBCL runtime.  The icon cannot be added to the
;; finished executable: rewriting PE resources afterwards corrupts the
;; Lisp core appended by save-lisp-and-die ("Can't find sbcl.core").
;;
;; Uses plain Quicklisp instead of qlot, which does not work on Windows.

;; SBCL bundles an old ASDF (no :local-nicknames in uiop:define-package);
;; load ASDF 3.3.7 first.  win-deploy.ps1 downloads it (checksum-verified)
;; into build/windows/.
(let* ((scripts-dir (make-pathname :name nil :type nil
                                   :defaults *load-truename*))
       (root (make-pathname :directory (butlast (pathname-directory scripts-dir))
                            :defaults scripts-dir)))
  (load (merge-pathnames "build/windows/asdf-3.3.7.lisp" root)))

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,root)
     :inherit-configuration)))

(ql:quickload :lem)

(lem:init-at-build-time)

;; deploy-op dumps the image (with :application-type :gui and
;; :save-runtime-options t) into bin/ next to lem.asd, together with all
;; foreign libraries loaded in the image (webview.dll, libasyncprocess.dll).
;; save-lisp-and-die exits the process, so this form never returns.
(asdf:make :lem)
