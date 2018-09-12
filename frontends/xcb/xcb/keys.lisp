(in-package :xcb)
;; nvim keyboard support
;;
;; long keynames must always send a <..>
;; 3 cases: regular, shifted, and otherwise-modified.
;; keynames of >1 must be packed into a <..> form no matter what;
;;
;;
;; *key-code* table contains codes for keys from 0 to 127 for normal and
;; shifterd representation.  If the code is < 127, it is to be used directly;
;; if the high bit is set, the low 7 bits are an index into *key-special* array
;; containing a string representation of the key.
;;
;; non-special codes must append C- A- or T- prefixes, based on the modifiers
;; special codes must also append the S- modifier.
;;

;; Given a string name, we can find

(defparameter *key-unshifted*
    (vector
     nil nil   nil  nil     nil nil   nil  nil
     nil   "Escape" "1"  "2"     "3" "4"   "5"  "6"
     ;;10 - 16
     "7"     "8"   "9"     "0"     "-"     "="   "Backspace"    "Tab"
     "q"     "w"   "e"     "r"     "t"     "y"   "u"     "i"
     ;;20 - 32     ;37=shift-lock
     "o"     "p"   "["     "]"     "Return" nil   "a"     "s"
     "d"     "f"   "g"     "h"     "j"     "k"   "l"     ";"
     ;;30 - 48
     "'"    "`"    nil     "\\"     "z"    "x"    "c"     "v"
     "b"    "n"    "m"     ","     "."    "/"    nil     "*"
     ;;40 - 64 ;left alt, caps-lock
     nil    " "     nil    "F1"    "F2"   "F3"    "F4"   "F5"
     "F6"   "F7"    "F8"   "F9"    "F10"  nil     nil    "7"
     ;;50 - 80
     "8"    "9"     "-"    "4"     "5"    "6"     "+"    "1"
     "2"    "3"     "0"    "."     nil nil nil    "F11"
     ;;60 - 96
     "F12"    nil   nil    nil     nil     nil   nil    nil
     "Return"  nil   "/"    nil     nil    "Linefeed"   "Home" "Up" 
     ;;70 - 112      
     "PageUp" "Left"  "Right" "End" "Down" "PageDown" "Insert" "Delete"
     nil nil nil nil  ;; "macro" "mute" "volumedown" "volumeup"
     nil "=" nil nil  ;; "power"    "kpequal"   "kpplusminus" "pause"
     ))

(defparameter *key-shifted*
    (vector
     nil nil   nil  nil    nil     nil   nil  nil
     nil "Esc" "!"  "@"    "#"     "$"   "%"  "^"
     ;;10 - 16
     "&"     "*"   "("     ")"     "_"     "+"   "BS"    "Tab"
     "Q"     "W"   "E"     "R"     "T"     "Y"   "U"     "I"
     ;;20 - 32     
     "O"     "P"   "{"     "}"     "Return" nil   "A"     "S"
     "D"     "F"   "G"     "H"     "J"     "K"   "L"     ":"
     ;;30 - 48
     "\""    "~"    nil     "|"    "Z"    "X"   "C"     "V"
     "B"    "N"    "M"     "LT"    ">"    "?"    nil     "*"
     ;;40 - 64
     nil    "Space" nil    "F1"    "F2"   "F3"    "F4"   "F5"
     "F6"   "F7"    "F8"   "F9"    "F10"  nil     nil    "7"
     ;;50 - 80
     "8"    "9"     "-"    "4"     "5"    "6"     "+"    "1"
     "2"    "3"     "0"    "."     nil nil nil    "F11"
     ;;60 - 96
     "F12"    nil   nil    nil     nil     nil   nil    nil
     "Return"  nil   "/"    nil     nil    "Linefeed"   "Home" "Up" 
     ;;70 - 112      
     "PageUp" "Left"  "Right" "End" "Down" "PageDown" "Insert" "Delete"
     nil nil nil nil  ;; "macro" "mute" "volumedown" "volumeup"
     nil "=" nil nil  ;; "power"    "kpequal"   "kpplusminus" "pause"
))

(defparameter *mod-string*
  (make-array 16 :element-type 'string
	      :initial-contents
	      '("<"      "<S-"     "<C-"     "<C-S-"
		"<A-"   "<A-S-"   "<A-C-"   "<A-C-S-"
		"<T-"   "<T-S-"   "<T-C-"   "<T-C-S-"
		"<T-A-" "<T-A-S-" "<T-A-C-" "<T-A-C-S-")))
;; count of bytes in above strings, + 1 for the closer

;;==============================================================================
(defun mod-normalize (modi)
  ;; x Modifiers 
  ;; 0000 0001 shift
  ;; 0000 0010 shift-locked
  ;; 0000 0100 control
  ;; 0000 1000 alt
  ;; 0100 0000 win
  ;; are translated into a 4-bit notation
  ;; 0001 shift
  ;; 0010 ctrl
  ;; 0100 alt
    ;; 1000 win
  (+ (if (zerop (logand    3 modi)) 0 1)
     (ash (logand    4 modi) -1) ;bit 2 goes to bit 1
     (ash (logand    8 modi) -1) ;bit 3 goes to bit 2
     (ash (logand #x40 modi) -3) ;bit 6 goes to bit 3
     ))

;;==============================================================================
;; barf out string length, and the string representation of the character.
;; Todo: utf8...

(defun key-process (key xmod)
  (let* ((mod (mod-normalize xmod))
	 (it (aref (if (logbitp 0 mod) *key-shifted* *key-unshifted*) key)))
    ;; shifted keys with 1-character symbols are considered unshifted
    (when (= 1 (length it))
      (setf mod (logand mod #xFE)))
    (values it mod) )

  
  )
(defun key-out (key xmod &optional
			   (write-start (lambda (length)
					  (princ length)))
			   (write-obj (lambda (obj)
					(princ obj))))
  (when (< key 128)
    (let* ((mod (mod-normalize xmod));; compact mod map
	   (it  (aref (if (= mod 1) *key-shifted* *key-unshifted*) key))
	   (bytecnt (length it)))
      (when it
	(if (and (= bytecnt 1) (< mod 2)) ;;really simple case?
	    (progn (funcall write-start 1)     ;;1 byte 
		   (funcall write-obj it)
					;		 (format t "SHORT ~A~& " (aref it 0))
		   ) ;;with the character
	    (let ((modstr (aref *mod-string* mod)))
	      (funcall write-start (+ 1 (length modstr) bytecnt))
	      (funcall write-obj modstr)
	      (funcall write-obj it)
	      (funcall write-obj ">")))))
    ))
