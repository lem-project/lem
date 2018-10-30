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

(loop :for i :from 0 :to #xff
      :do (setf (gethash (code-char (+ #xe000 i)) *char-replacement*)
                (format nil "\\~D" i)))

(defun tab-size ()
  *tab-size*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *eastasian-full*
    (vector
     '(#x1100 #x115f) '(#x231a #x231b) '(#x2329 #x232a) '(#x23e9 #x23ec)
     '(#x23f0 #x23f0) '(#x23f3 #x23f3) '(#x25fd #x25fe) '(#x2614 #x2615)
     '(#x2648 #x2653) '(#x267f #x267f) '(#x2693 #x2693) '(#x26a1 #x26a1)
     '(#x26aa #x26ab) '(#x26bd #x26be) '(#x26c4 #x26c5) '(#x26ce #x26ce)
     '(#x26d4 #x26d4) '(#x26ea #x26ea) '(#x26f2 #x26f3) '(#x26f5 #x26f5)
     '(#x26fa #x26fa) '(#x26fd #x26fd) '(#x2705 #x2705) '(#x270a #x270b)
     '(#x2728 #x2728) '(#x274c #x274c) '(#x274e #x274e) '(#x2753 #x2755)
     '(#x2757 #x2757) '(#x2795 #x2797) '(#x27b0 #x27b0) '(#x27bf #x27bf)
     '(#x2b1b #x2b1c) '(#x2b50 #x2b50) '(#x2b55 #x2b55) '(#x2e80 #x2e99)
     '(#x2e9b #x2ef3) '(#x2f00 #x2fd5) '(#x2ff0 #x2ffb) '(#x3000 #x303e)
     '(#x3041 #x3096) '(#x3099 #x30ff) '(#x3105 #x312f) '(#x3131 #x318e)
     '(#x3190 #x31ba) '(#x31c0 #x31e3) '(#x31f0 #x321e) '(#x3220 #x3247)
     '(#x3250 #x32fe) '(#x3300 #x4dbf) '(#x4e00 #xa48c) '(#xa490 #xa4c6)
     '(#xa960 #xa97c) '(#xac00 #xd7a3) '(#xf900 #xfaff) '(#xfe10 #xfe19)
     '(#xfe30 #xfe52) '(#xfe54 #xfe66) '(#xfe68 #xfe6b) '(#xff01 #xff60)
     '(#xffe0 #xffe6) '(#x16fe0 #x16fe1) '(#x17000 #x187f1) '(#x18800 #x18af2)
     '(#x1b000 #x1b11e) '(#x1b170 #x1b2fb) '(#x1f004 #x1f004) '(#x1f0cf #x1f0cf)
     '(#x1f18e #x1f18e) '(#x1f191 #x1f19a) '(#x1f200 #x1f202) '(#x1f210 #x1f23b)
     '(#x1f240 #x1f248) '(#x1f250 #x1f251) '(#x1f260 #x1f265) '(#x1f300 #x1f320)
     '(#x1f32d #x1f335) '(#x1f337 #x1f37c) '(#x1f37e #x1f393) '(#x1f3a0 #x1f3ca)
     '(#x1f3cf #x1f3d3) '(#x1f3e0 #x1f3f0) '(#x1f3f4 #x1f3f4) '(#x1f3f8 #x1f43e)
     '(#x1f440 #x1f440) '(#x1f442 #x1f4fc) '(#x1f4ff #x1f53d) '(#x1f54b #x1f54e)
     '(#x1f550 #x1f567) '(#x1f57a #x1f57a) '(#x1f595 #x1f596) '(#x1f5a4 #x1f5a4)
     '(#x1f5fb #x1f64f) '(#x1f680 #x1f6c5) '(#x1f6cc #x1f6cc) '(#x1f6d0 #x1f6d2)
     '(#x1f6eb #x1f6ec) '(#x1f6f4 #x1f6f9) '(#x1f910 #x1f93e) '(#x1f940 #x1f970)
     '(#x1f973 #x1f976) '(#x1f97a #x1f97a) '(#x1f97c #x1f9a2) '(#x1f9b0 #x1f9b9)
     '(#x1f9c0 #x1f9c2) '(#x1f9d0 #x1f9ff) '(#x20000 #x2fffd) '(#x30000 #x3fffd)))

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
