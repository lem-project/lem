;; -*- mode: lisp; package: repl -*-

(in-package :repl)

(defun common-prefix (items)
  (subseq (car items)
          0
          (apply #'min
                 (mapcar #'(lambda (item)
                             (or (mismatch (car items) item)
                                 (length item)))
                         (cdr items)))))

(defun package-prefix (str)
  (cond ((let ((pos (search "::" str)))
           (when pos
             (list (subseq str (+ pos 2)) (subseq str 0 pos) nil))))
        ((let ((pos (position #\: str)))
           (when pos
             (list (subseq str (+ pos 1))
                   (if (zerop pos)
                       "KEYWORD"
                       (subseq str 0 pos))
                   t))))
        (t
         (list str nil nil))))

(defun symbol-complete (text)
  (let ((text (string-upcase text))
        (els))
    (flet ((body (sym text prefix)
                 (let ((name (string sym)))
                   (when (eql 0 (search text name))
                     (push (format nil "~(~a~a~)" prefix name)
                           els)))))
      (destructuring-bind (symbol-name package external-p)
          (package-prefix text)
        (cond ((and package external-p)
               (do-external-symbols (sym package)
                 (body sym symbol-name
                       (if (equal (package-name :keyword)
                                  (package-name package))
                           ":"
                           (format nil "~a:" package)))))
              (package
               (do-symbols (sym package)
                 (body sym symbol-name (format nil "~a::" package))))
              (t
               (do-symbols (sym *package*)
                 (body sym symbol-name ""))
               (dolist (package (list-all-packages))
                 (body (format nil "~a:" (package-name package))
                       symbol-name "")
                 (dolist (package-name (package-nicknames package))
                   (body (format nil "~a:" package-name)
                         symbol-name "")))))))
    (if (cdr els)
        (cons (common-prefix els) els)
        els)))
