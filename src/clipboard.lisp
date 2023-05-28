(in-package :lem)

(defun wsl-p ()
  (zerop (nth-value 2 (uiop:run-program '("which" "clip.exe") :ignore-error-status t))))

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
    #+darwin (sbcl-2.0.0-or-later-p)
    #-darwin (not (wsl-p))))

(defun enable-clipboard ()
  (setf *enable-clipboard-p* t))

(defun disable-clipboard ()
  (setf *enable-clipboard-p* nil))

(defun enable-clipboard-p ()
  *enable-clipboard-p*)

(defun copy-to-clipboard (string)
  (lem-if:clipboard-copy (implementation) string))

(defun get-clipboard-data ()
  (lem-if:clipboard-paste (implementation)))
