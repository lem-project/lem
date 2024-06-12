(in-package :lem-core)

(defvar *file-completion-ignore-case* t)

(defun fuzzy-match-p (str elt &optional ignore-case)
  (loop :with start := 0
        :for c :across str
        :do (let ((pos (position c elt :start start :test (if ignore-case #'char-equal #'char=))))
              (if pos
                  (setf start pos)
                  (return nil)))
        :finally (return t)))

(defun completion-test (x y &optional (ignore-case nil))
  (and (<= (length x) (length y))
       (if ignore-case
           (string-equal x y :end2 (length x))
           (string= x y :end2 (length x)))))

(defun explode-string (string separator)
  (if (string= string "")
      '()
      (flet ((separatorp (char) (find char separator)))
        (loop :for start := 0 :then (1+ pos)
              :for pos := (position-if #'separatorp string :start start)
              :collect (subseq string start pos)
              :when pos :collect (string (char string pos))
              :while pos))))

(defun completion (name elements &key (test #'search) separator key)
  (labels ((apply-key (elt) (if key (funcall key elt) elt))
           (test-with-separator (elt)
             (let* ((elt (apply-key elt))
                    (parts1 (explode-string name separator))
                    (parts2 (explode-string elt separator)))
               (and (<= (length parts1) (length parts2))
                    (loop :for p1 :in parts1
                          :for p2 :in parts2
                          :always (funcall test p1 p2)))))
           (test-without-separator (elt)
             (funcall test name (apply-key elt))))
    (remove-if-not (if separator
                       #'test-with-separator
                       #'test-without-separator)
                   elements)))

(defun completion-hyphen (name elements &key key)
  (completion name elements :test #'completion-test :separator "-" :key key))

(defun completion-file (str directory &key (ignore-case *file-completion-ignore-case*) directory-only)
  (setf str (expand-file-name str directory))
  (let* ((input-directory
           (let ((dir (directory-namestring str)))
             (or (ignore-errors (virtual-probe-file dir)) dir)))
         (input-pathname (merge-pathnames (enough-namestring str (directory-namestring str))
                                          input-directory))
         (files (mapcar #'namestring (list-directory input-directory :directory-only directory-only)))
         (test-fn (alexandria:rcurry #'completion-test ignore-case)))
    (let ((strings
            (loop
              :for pathname :in (directory-files input-pathname)
              :for namestr := (namestring pathname)
              :append
                 (completion (let ((str (enough-namestring namestr input-directory)))
                               (if (uiop:directory-pathname-p str)
                                   (string-right-trim "/" str)
                                   str))
                             files
                             :test test-fn
                             :separator "-."
                             :key #'(lambda (path)
                                      (enough-namestring path input-directory))))))
      strings)))

(defun completion-strings (str strings &key key)
  (completion str strings :test #'fuzzy-match-p :key key))

(defun completion-buffer (str &optional (buffer-list (buffer-list)))
  (let ((candidates1
          (completion str buffer-list
                      :test (lambda (str buffer)
                              (or (search str (buffer-name buffer))
                                  (and (buffer-filename buffer)
                                       (search str (buffer-filename buffer)))))))
        (candidates2
          (completion str buffer-list
                      :test (lambda (str buffer)
                              (or (fuzzy-match-p str (buffer-name buffer))
                                  (and (buffer-filename buffer)
                                       (fuzzy-match-p str (buffer-filename buffer))))))))
    (dolist (c candidates1)
      (setf candidates2 (delete c candidates2)))
    (append candidates1 candidates2)))
