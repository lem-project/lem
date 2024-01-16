(in-package :lem-core)

(defparameter *default-prompt-gravity* :center)

(defvar *prompt-activate-hook* '())
(defvar *prompt-deactivate-hook* '())

(defvar *prompt-buffer-completion-function* nil)
(defvar *prompt-file-completion-function* nil)

(defgeneric caller-of-prompt-window (prompt))
(defgeneric prompt-active-p (prompt))
(defgeneric active-prompt-window ())
(defgeneric get-prompt-input-string (prompt))
(defgeneric %prompt-for-character (prompt &key gravity))
(defgeneric %prompt-for-line (prompt &key initial-value completion-function test-function
                                          history-symbol syntax-table gravity edit-callback
                                          special-keymap use-border))

(defun prompt-for-character (prompt &key (gravity *default-prompt-gravity*))
  (%prompt-for-character prompt :gravity gravity))

(defun prompt-for-y-or-n-p (prompt &key (gravity *default-prompt-gravity*))
  (loop :for c := (prompt-for-character (format nil "~A [y/n]? " prompt) :gravity gravity)
        :do (case c
              (#\y (return t))
              (#\n (return nil)))))

(defun prompt-for-string (prompt &rest args
                                 &key initial-value
                                      completion-function
                                      test-function
                                      (history-symbol nil)
                                      (syntax-table (current-syntax))
                                      (gravity *default-prompt-gravity*)
                                      edit-callback
                                      special-keymap
                                      use-border)
  (declare (ignore initial-value
                   completion-function
                   test-function
                   history-symbol
                   syntax-table
                   gravity
                   edit-callback
                   special-keymap
                   use-border))
  (apply #'%prompt-for-line prompt args))

(defun prompt-for-integer (prompt &key initial-value min max (gravity *default-prompt-gravity*))
  (check-type initial-value (or null integer))
  (parse-integer
   (prompt-for-string prompt
                      :initial-value (when initial-value (princ-to-string initial-value))
                      :test-function (lambda (str)
                                       (multiple-value-bind (n len)
                                           (parse-integer str :junk-allowed t)
                                         (and
                                          n
                                          (/= 0 (length str))
                                          (= (length str) len)
                                          (if min (<= min n) t)
                                          (if max (<= n max) t))))
                      :history-symbol 'prompt-for-integer
                      :gravity gravity)))

(defun prompt-for-buffer (prompt &key default existing (gravity *default-prompt-gravity*))
  (let ((result (prompt-for-string
                 (if default
                     (format nil "~a(~a) " prompt default)
                     prompt)
                 :completion-function *prompt-buffer-completion-function*
                 :test-function (and existing
                                     (lambda (name)
                                       (or (alexandria:emptyp name)
                                           (get-buffer name))))
                 :history-symbol 'prompt-for-buffer
                 :gravity gravity)))
    (if (string= result "")
        default
        result)))

(defun prompt-for-file (prompt &key directory (default (buffer-directory)) existing
                                    (gravity *default-prompt-gravity*))
  (let ((result
          (prompt-for-string (if default
                                 (format nil "~a(~a) " prompt default)
                                 prompt)
                             :initial-value (when directory (princ-to-string directory))
                             :completion-function
                             (when *prompt-file-completion-function*
                               (lambda (str)
                                 (funcall *prompt-file-completion-function*
                                          (if (alexandria:emptyp str)
                                              "./"
                                              str)
                                          (or directory
                                              (namestring (user-homedir-pathname))))))
                             :test-function (and existing #'virtual-probe-file)
                             :history-symbol 'prompt-for-file
                             :gravity gravity)))
    (if (string= result "")
        default
        result)))

(defun prompt-for-directory (prompt &rest args
                                    &key directory (default (buffer-directory)) existing
                                    &allow-other-keys)
  (let ((result
          (apply #'prompt-for-string
                 prompt
                 :initial-value directory
                 :completion-function
                 (when *prompt-file-completion-function*
                   (lambda (str)
                     (funcall *prompt-file-completion-function*
                              (if (alexandria:emptyp str)
                                  "./"
                                  str)
                              directory :directory-only t)))
                 :test-function (and existing #'virtual-probe-file)
                 :history-symbol 'prompt-for-directory
                 (alexandria:remove-from-plist args :directory :default :existing))))
    (if (string= result "")
        default
        result)))

(defun prompt-for-library (prompt &key history-symbol)
  (macrolet ((ql-symbol-value (symbol)
               `(symbol-value (uiop:find-symbol* ,symbol :quicklisp))))
    (let ((systems
            (append
             (mapcar (lambda (x) (pathname-name x))
                     (directory
                      (merge-pathnames "**/lem-*.asd"
                                       (asdf:system-source-directory :lem-contrib))))
             (set-difference
              (mapcar #'pathname-name
                      (loop for i in (ql-symbol-value :*local-project-directories*)
                            append (directory (merge-pathnames "**/lem-*.asd" i))))
              (mapcar #'pathname-name
                      (directory (merge-pathnames "**/lem-*.asd"
                                                  (asdf:system-source-directory :lem))))
              :test #'equal))))
      (setq systems (mapcar (lambda (x) (subseq x 4)) systems))
      (prompt-for-string prompt
                         :completion-function (lambda (str) (completion str systems))
                         :test-function (lambda (system) (find system systems :test #'string=))
                         :history-symbol history-symbol))))

(defun prompt-for-encodings (prompt &key history-symbol)
  (let ((encodings (encodings)))
    (let ((name (prompt-for-string
                 (format nil "~A(~(~A~))" prompt lem-base::*default-external-format*)
                 :completion-function (lambda (str) (completion str encodings))
                 :test-function (lambda (encoding) (or (equal encoding "")
                                                       (find encoding encodings :test #'string=)))
                 :history-symbol history-symbol)))
      (cond ((equal name "") lem-base::*default-external-format*)
            (t (read-from-string (format nil ":~A" name)))))))
