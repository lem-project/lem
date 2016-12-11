(defpackage :lem.shell
  (:use :cl :lem :lem.listener-mode))
(in-package :lem.shell)

(defun map-path (fn)
  (dolist (path (uiop:split-string (uiop:getenv "PATH") :separator ":"))
    (dolist (file (directory (merge-pathnames "*.*" (uiop:ensure-directory-pathname path))))
      (funcall fn file))))

(defun path-files ()
  (let ((files))
    (map-path (lambda (file)
                (push (file-namestring file) files)))
    (nreverse files)))

(defvar *builtin-commands*
  '(("cd" . cd-command)))

(define-major-mode shell-mode nil
  (:name "shell"
   :keymap *shell-mode-keymap*)
  (setf (get-bvar :listener-get-prompt-function)
        'get-prompt)
  (setf (get-bvar :listener-check-confirm-function)
        'check-confirm)
  (setf (get-bvar :listener-confirm-function)
        'confirm)
  (listener-mode t))

(defun get-prompt ()
  (format nil "~A$ " (uiop:getcwd)))

(defun check-confirm ()
  t)

(defun confirm (string)
  (let* ((parts (uiop:split-string string))
         (found (assoc (car parts) *builtin-commands* :test #'equal)))
    (cond
      (found
       (funcall (cdr found) (cdr parts)))
      (t
       (insert-string
        (with-output-to-string (output)
          (uiop:run-program string
                            :ignore-error-status t
                            :output output
                            :error-output output))))))
  (listener-reset-prompt))

(defun command-point-p ()
  (save-excursion
    (backward-sexp 1 t)
    (skip-whitespace-backward)
    (and (point>= (listener-start-point) (current-point))
         (char= #\. (following-char)))))

(defun preceding-string ()
  (let* ((end-point (current-point))
         (start-point (save-excursion
                        (when (and (backward-sexp 1 t)
                                   (point<= (listener-start-point)
                                            (current-point)))
                          (current-point)))))
    (when start-point
      (region-string start-point end-point))))

(defun command-completion ()
  (let ((str (or (preceding-string) ""))
        (files (sort (path-files) #'< :key #'length)))
    (when str
      (start-completion (lambda (str)
                          (completion str files))
                        (preceding-string)))))

(defun filename-completion ()
  (let ((str (preceding-string)))
    (when str
      (start-completion #'completion-file str))))

(define-key *shell-mode-keymap* "C-i" 'shell-line-completion)
(define-command shell-line-completion () ()
  (if (command-point-p)
      (command-completion)
      (filename-completion)))

(define-command shell () ()
  (listener-start "*shell*" 'shell-mode))

(defun cd-command (args)
  (let* ((arg (first args))
         (dir (if (null arg)
                  (user-homedir-pathname)
                  (cl-fad:directory-exists-p (merge-pathnames arg (uiop:getcwd))))))
    (if dir
        (uiop:chdir dir)
        (insert-string (format nil "~A does not exist~%" arg)))))
