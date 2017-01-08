(in-package :cl-user)
(defpackage :lem.util
  (:use :cl)
  (:export
   :pdebug
   :utf8-bytes
   :split-string
   :join
   :replace-string
   :random-range
   :temp-file-name
   :safe-aref
   :make-history
   :last-history
   :add-history
   :prev-history
   :next-history
   :bests-if
   :max-if
   :min-if
   :mkstr
   :symb))
(in-package :lem.util)

(defun pdebug (x &optional (file "DEBUG"))
  (with-open-file (out file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (print x out)))

(defun utf8-bytes (c)
  (cond
    ((<= c #x7f) 1)
    ((<= #xc2 c #xdf) 2)
    ((<= #xe0 c #xef) 3)
    ((<= #xf0 c #xf4) 4)
    (t 1)))

(defun split-string (str delim)
  (labels ((f (str acc length)
	     (let ((i (position delim str)))
	       (if (null i)
		   (values (nreverse (cons str acc))
			   (1+ length))
		   (f (subseq str (1+ i))
		      (cons (subseq str 0 i) acc)
		      (1+ length))))))
    (f str nil 0)))

(defun join (str strings)
  (format nil "~{~A~}"
          (loop :for rest :on strings
	     :collect (car rest)
	     :if (cdr rest)
	     :collect str)))

(defun replace-string (before after string)
  (let ((i (search before string)))
    (if i
        (values (concatenate
                 'string
                 (subseq string 0 i)
                 after
                 (replace-string before after
                                 (subseq string (+ i (length before)))))
                t)
        (values string nil))))

(defun random-range (min max)
  (+ min (random (1+ (- max min)))))

(defun temp-file-name (prefix-name)
  (labels ((random-name ()
             (concatenate 'string
                          "/tmp/"
                          prefix-name
                          "-"
                          (coerce (loop repeat 8
				     collect (code-char
					      (random-range
					       (char-code #\a)
					       (char-code #\z))))
                                  'string))))
    (loop
       for name = (random-name)
       while (cl-fad:file-exists-p name)
       finally (return name))))

(defun safe-aref (seq i &optional default)
  (if (< -1 i (length seq))
      (aref seq i)
      default))

(defstruct (history (:constructor %make-history))
  data
  index
  novelty-check)

(defun history-default-novelty-check (x y)
  (not (equal x y)))

(defun make-history (&optional (novelty-check #'history-default-novelty-check))
  (%make-history
   :data (make-array 0 :fill-pointer 0 :adjustable t)
   :index 0
   :novelty-check novelty-check))

(defun last-history (history)
  (when (< 0 (length (history-data history)))
    (aref (history-data history)
          (1- (length (history-data history))))))

(defun add-history (history x)
  (when (funcall (history-novelty-check history)
                 x
                 (last-history history))
    (vector-push-extend x (history-data history)))
  (setf (history-index history)
        (length (history-data history)))
  x)

(defun prev-history (history)
  (when (< 0 (history-index history))
    (values (aref (history-data history)
                  (decf (history-index history)))
            t)))

(defun next-history (history)
  (when (< (history-index history)
           (1- (length (history-data history))))
    (values (aref (history-data history)
                  (incf (history-index history)))
            t)))

(defun bests-if (fn list test)
  (let ((best-value)
        (bests))
    (dolist (x list)
      (let ((score (funcall fn x)))
        (cond ((or (not best-value)
                   (funcall test score best-value))
               (setq best-value score)
               (setq bests (list x)))
              ((= best-value score)
               (push x bests)))))
    (values bests best-value)))

(defun max-if (fn list)
  (bests-if fn list #'>))

(defun min-if (fn list)
  (bests-if fn list #'<))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
