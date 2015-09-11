(in-package :cl-user)
(defpackage inquisitor.eol
  (:use :cl)
  (:export :eol-available-p
           :lf-keyword
           :cr-keyword
           :crlf-keyword
           :eol-guess-from-vector))
(in-package :inquisitor.eol)


(defun eol-available-p ()
  ;; #+allegro
  ;; #+lispworks
  #+clisp t
  #+ecl t
  #+sbcl nil
  #+ccl t
  #+abcl t)


(defun lf-keyword ()
  ;; #+lispworks :lf
  #+clisp :unix
  ;; #+ecl :lf
  #+sbcl (values :lf ; sbcl cannot treat line-breaks
                 :cannot-treat)
  #+ccl :unix
  ;; #+abcl :lf
  #-(or clisp sbcl ccl) :lf)

(defun cr-keyword ()
  ;; #+lispworks :cr
  #+clisp :mac
  ;; #+ecl :cr
  #+sbcl (values :cr ; sbcl cannot treat line-breaks
                 :cannot-treat)
  #+ccl :macos
  ;; #+abcl :cr
  #-(or clisp sbcl ccl) :cr)

(defun crlf-keyword ()
  ;; #+lispworks :crlf
  #+clisp :dos
  ;; #+ecl :crlf
  #+sbcl (values :crlf ; sbcl cannot treat line-breaks
                 :cannot-treat)
  #+ccl :dos
  ;; #+abcl :crlf
  #-(or clisp sbcl ccl) :crlf)


(defun eol-guess-from-vector (buffer &aux (len (length buffer)))
  (loop for i of-type fixnum from 0 below len
     finally (return (lf-keyword))
     do (if (eq (aref buffer (the fixnum i))
                (the fixnum (char-code #\Return)))
            (if (and (< (1+ (the fixnum i)) len)
                     (eq (aref buffer (the fixnum (1+ i)))
                         (the fixnum (char-code #\Newline))))
                (return (crlf-keyword))
                (return (cr-keyword))))
        (if (eq (aref buffer (the fixnum i))
                (the fixnum (char-code #\Newline)))
            (return (lf-keyword)))))
