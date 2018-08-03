(in-package :lem-base)

(export '(tab-width
          tab-size
          wide-char-p
          char-width
          string-width
          wide-index
          *char-replacement*))

(define-editor-variable tab-width 8)

(defvar *tab-size* 8)
(defvar *char-replacement* (make-hash-table))

(setf (gethash (code-char 0) *char-replacement*) "^@")
(setf (gethash (code-char 1) *char-replacement*) "^A")
(setf (gethash (code-char 2) *char-replacement*) "^B")
(setf (gethash (code-char 3) *char-replacement*) "^C")
(setf (gethash (code-char 4) *char-replacement*) "^D")
(setf (gethash (code-char 5) *char-replacement*) "^E")
(setf (gethash (code-char 6) *char-replacement*) "^F")
(setf (gethash (code-char 7) *char-replacement*) "^G")
(setf (gethash (code-char 8) *char-replacement*) "^H")
(setf (gethash (code-char 9) *char-replacement*) "^I")
(setf (gethash (code-char 11) *char-replacement*) "^K")
(setf (gethash (code-char 12) *char-replacement*) "^L")
(setf (gethash (code-char 13) *char-replacement*) "^R")
(setf (gethash (code-char 14) *char-replacement*) "^N")
(setf (gethash (code-char 15) *char-replacement*) "^O")
(setf (gethash (code-char 16) *char-replacement*) "^P")
(setf (gethash (code-char 17) *char-replacement*) "^Q")
(setf (gethash (code-char 18) *char-replacement*) "^R")
(setf (gethash (code-char 19) *char-replacement*) "^S")
(setf (gethash (code-char 20) *char-replacement*) "^T")
(setf (gethash (code-char 21) *char-replacement*) "^U")
(setf (gethash (code-char 22) *char-replacement*) "^V")
(setf (gethash (code-char 23) *char-replacement*) "^W")
(setf (gethash (code-char 24) *char-replacement*) "^X")
(setf (gethash (code-char 25) *char-replacement*) "^Y")
(setf (gethash (code-char 26) *char-replacement*) "^Z")
(setf (gethash (code-char 27) *char-replacement*) "^[")
(setf (gethash (code-char 28) *char-replacement*) "^\\")
(setf (gethash (code-char 29) *char-replacement*) "^]")
(setf (gethash (code-char 30) *char-replacement*) "^^")
(setf (gethash (code-char 31) *char-replacement*) "^_")
(setf (gethash (code-char 127) *char-replacement*) "^?")

(defun tab-size ()
  *tab-size*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eastasian-full*
    (vector
     '(#x01100 #x0115f) '(#x02329 #x0232a) '(#x02e80 #x02e99) '(#x02e9b #x02ef3)
     '(#x02f00 #x02fd5) '(#x02ff0 #x02ffb) '(#x03000 #x0303e) '(#x03041 #x03096)
     '(#x03099 #x030ff) '(#x03105 #x0312d) '(#x03131 #x0318e) '(#x03190 #x031ba)
     '(#x031c0 #x031e3) '(#x031f0 #x0321e) '(#x03220 #x03247) '(#x03250 #x032fe)
     '(#x03300 #x04dbf) '(#x04e00 #x0a48c) '(#x0a490 #x0a4c6) '(#x0a960 #x0a97c)
     '(#x0ac00 #x0d7a3) '(#x0f900 #x0faff) '(#x0fe10 #x0fe19) '(#x0fe30 #x0fe52)
     '(#x0fe54 #x0fe66) '(#x0fe68 #x0fe6b) '(#x0ff01 #x0ff60) '(#x0ffe0 #x0ffe6)
     '(#x1b000 #x1b001) '(#x1f200 #x1f202) '(#x1f210 #x1f23a) '(#x1f240 #x1f248)
     '(#x1f250 #x1f251) '(#x20000 #x2fffd) '(#x30000 #x3fffd)))

  (defun gen-binary-search-function (name vector)
    (declare (optimize (speed 0) (safety 3) (debug 3)))
    (labels ((rec (begin end)
               (when (<= begin end)
                 (let* ((i (floor (+ end begin) 2))
                        (elt (aref vector i))
                        (a (car elt))
                        (b (cadr elt))
                        (then (rec begin (1- i)))
                        (else (rec (1+ i) end)))
                   `(if (<= ,a code ,b)
                        t
                        ,(if (or then else)
                             `(if (< code ,a)
                                  ,then
                                  ,else)))))))
      (compile
       (eval
        `(defun ,name (code)
           (declare (optimize (speed 3) (safety 0) (debug 0)))
           (declare (fixnum code))
           ,(rec 0 (1- (length vector))))))))

  (gen-binary-search-function '%binary-search *eastasian-full*))

(defun wide-char-p (c)
  (declare (character c))
  (%binary-search (char-code c)))

(defun char-width (c w)
  (declare (character c) (fixnum w))
  (cond ((char= c #\tab)
         (+ (* (floor w (tab-size)) (tab-size)) (tab-size)))
        ((gethash c *char-replacement*)
         (loop :for c :across (gethash c *char-replacement*)
               :do (setf w (char-width c w)))
         w)
        ((or (wide-char-p c) (char<= #.(code-char 0) c #.(code-char 26)))
         (+ w 2))
        (t
         (+ w 1))))

(defun string-width (str &optional (start 0) end)
  (loop :with width := 0
        :for i :from start :below (or end (length str))
        :for c := (aref str i)
        :do (setq width (char-width c width))
        :finally (return width)))

(defun wide-index (str goal &key (start 0))
  (loop
    :with w := 0
    :for i :from start :below (length str) :by 1
    :for c := (schar str i)
    :do (setq w (char-width c w))
        (when (< goal w)
          (return i))))
