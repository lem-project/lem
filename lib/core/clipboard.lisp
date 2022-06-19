(in-package :lem)

(export '(*enable-clipboard-p*))

(defun sbcl-2.0.0-or-later-p ()
  (and (string-equal "sbcl" (lisp-implementation-type))
       (let ((version (mapcar #'parse-integer
                              (uiop:split-string (lisp-implementation-version)
                                                 :separator "."))))
         (trivia:match version
           ((cons major _)
            (<= 2 major))))))

(defparameter *enable-clipboard-p*
  (ignore-errors
    (or (progn #+darwin nil #-darwin t)
        (sbcl-2.0.0-or-later-p))))

(defun enable-clipboard-p ()
  *enable-clipboard-p*)

(defun copy-to-clipboard (string)
  (lem-if:clipboard-copy (implementation) string))

(defun get-clipboard-data ()
  (lem-if:clipboard-paste (implementation)))
