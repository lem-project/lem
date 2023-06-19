(defpackage :lem/legit
  (:use :cl
   :lem
   :lem/grep)
  (:export :legit)
  #+sbcl
  (:lock t))
(in-package :lem/legit)

(defun run-grep (string directory)
  (multiple-value-bind (output error-output status-code)
      (uiop:run-program string
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (cond ((eql status-code 0)
           output)
          ((eql status-code 1)
           "")
          (t
           (editor-error "~A"
                         (string-right-trim '(#\newline #\space)
                                            error-output))))))

(defun parse-grep-result (text)
  (let* ((text (string-right-trim '(#\newline) text))
         (lines (uiop:split-string text :separator '(#\newline)))
         (file-line-content-tuples
           (mapcar (lambda (line)
                     (destructuring-bind (file line-number content)
                         (ppcre:split ":" line :limit 3)
                       (list file
                             (parse-integer line-number)
                             content)))
                   lines)))
    file-line-content-tuples))

(defun move (file line-number)
  (let ((buffer (find-file-buffer (merge-pathnames file directory))))
    (move-to-line (buffer-point buffer) line-number)))

(defun make-move-function (file line-number)
  (lambda ()
    (move file line-number)))

(defun get-content-string (start)
  (with-point ((start start)
               (end start))
    (line-start start)
    (next-single-property-change start :content-start)
    (character-offset start 1)
    (line-end end)
    (points-to-string start end)))

(defun change-grep-buffer (start end old-len)
  (declare (ignore end old-len))
  (let ((string (get-content-string start))
        (move (lem/peek-legit:get-move-function start)))
    (with-point ((point (funcall move)))
      (with-point ((start point)
                   (end point))
        (line-start start)
        (line-end end)
        (buffer-undo-boundary (point-buffer start))
        (delete-between-points start end)
        (insert-string start string)
        (buffer-undo-boundary (point-buffer start)))))
  (lem/peek-legit:show-matched-line))

(defvar *last-query* "git grep -nH ")
(defvar *last-directory* nil)

(define-command grep (query &optional (directory (buffer-directory)))
  "Run an interactive grep."
    ((prompt-for-string "" :initial-value *last-query* :history-symbol 'grep)
     (princ-to-string (prompt-for-directory "Directory: "
                                            :directory (if *last-directory*
                                                           (princ-to-string *last-directory*)
                                                           (buffer-directory)))))
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (result (parse-grep-result (run-grep query directory))))
    (if (null result)
        (editor-error "No match")
        (lem/peek-legit:with-collecting-sources (collector :read-only nil)
          (loop :for (file line-number content) :in result
                :do (lem/peek-legit:with-appending-source
                        (point :move-function (make-move-function file line-number))
                      (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)
                      (insert-string point ":" :read-only t)
                      (insert-string point (princ-to-string line-number)
                                     :attribute 'lem/peek-legit:position-attribute
                                     :read-only t)
                      (insert-string point ":" :read-only t :content-start t)
                      (insert-string point content)))
          (add-hook (variable-value 'after-change-functions :buffer (lem/peek-legit:collector-buffer collector))
                    'change-grep-buffer)))
    (setf *last-query* query
          *last-directory* directory)))


;; (load "porcelain.lisp")
(load "src/ext/porcelain.lisp")

(define-command legit-status () ()
  "Show changes and untracked files."
  (multiple-value-bind (modified untracked)
      (porcelain-components)
    (declare (ignorable untracked))
    ;; (message "Modified files: ~S" modified)

    ;; big try!
    (lem/peek-legit:with-collecting-sources (collector :read-only nil)
      (loop :for file :in modified
            :for directory := ""
            :for i := 0 :then (incf i)
            :do (lem/peek-legit:with-appending-source
                    (point :move-function (make-move-function file i))

                  (insert-string point file :attribute 'lem/peek-legit:filename-attribute :read-only t)
                  ;; (insert-string point ":" :read-only t)
                  ;; (insert-string point (princ-to-string i)
                                 ;; :attribute 'lem/peek-legit:position-attribute
                                 ;; :read-only t)
                  ;; (insert-string point ":" :read-only t :content-start t)
                  ;; (insert-string point file)
                  ))
      (add-hook (variable-value 'after-change-functions :buffer (lem/peek-legit:collector-buffer collector))
                'change-grep-buffer))))
