(setf lem:*find-directory-function* 'lem.dired:dired-buffer)

(defun load-init-file ()
  (flet ((test (path)
               (when (cl-fad:file-exists-p path)
                 (lem.lisp-mode:lisp-load-file path)
                 (lem:message "Load file: ~a" path)
                 t)))
    (or (test (merge-pathnames "lem.rc" (truename ".")))
        (test (merge-pathnames ".lemrc" (user-homedir-pathname))))))

(lem:add-hook 'lem:after-init-hook
              'load-init-file)

(lem:add-hook 'lem:find-file-hook
              (lambda ()
                (lem:syntax-scan-buffer (lem:current-buffer))))

;;; !!!
(progn
  (defvar *syntax-timer* nil)

  (lem:add-hook 'lem:post-command-hook
                (lambda ()
                  (lem::syntax-scan-lines (lem:current-window)
                                          (lem:current-linum)
                                          (1+ (lem:current-linum)))))

  (lem:add-hook 'lem:pre-command-hook
                (lambda ()
                  (when (lem::timer-p *syntax-timer*)
                    (lem:stop-timer *syntax-timer*))
                  (setq *syntax-timer*
                        (lem:start-timer 500
                                         nil
                                         (lambda ()
                                           (unless (lem:active-minibuffer-window)
                                             (lem:syntax-scan-window (lem:current-window))
                                             (lem:redraw-display)))))))
  )

#+sbcl
(push #'(lambda (x)
          (if x
              (lem:lem x)
              (lem:lem))
          t)
      sb-ext:*ed-functions*)
