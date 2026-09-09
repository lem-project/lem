(in-package :lem-core)

(add-hook *after-init-hook*
          (lambda ()
            ;; PATH injection for macOS
            (setf (uiop:getenv "PATH")
                  (format nil "~{~A~^:~}" (exec-path)))
            ;; Prevent the startup directory from becoming root
            (when (deploy:deployed-p)
              (let ((dir (user-homedir-pathname)))
                (setq *default-pathname-defaults* dir)
                (uiop:chdir dir)))))

;; Tell the deploy library not to bundle tree-sitter native libraries.
;; They are optional and loaded at runtime only when available.
;; Only relevant while building; skip when running inside the deployed
;; binary so we don't mutate deploy state on every startup.
(unless (deploy:deployed-p)
  (when (find-package :tree-sitter/ffi)
    (let ((ts (find-symbol "TREE-SITTER" :tree-sitter/ffi))
          (tw (find-symbol "TS-WRAPPER" :tree-sitter/ffi)))
      (when ts
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library ts)) t))
      (when tw
        (setf (deploy:library-dont-deploy-p (deploy:ensure-library tw)) t)))))
