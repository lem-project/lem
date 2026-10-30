(defpackage :lem/insert-char
  (:use :cl
        :lem)
  (:export :insert-char
           :char-from-name-or-code)
  #+sbcl
  (:lock t))
(in-package :lem/insert-char)

(defconstant +completion-limit+ 500
  "Maximum number of character names shown in the completion list.")

(defvar *char-name-candidates* nil
  "Cached list of (NAME . CHAR) pairs for character name completion.")

(define-key *global-keymap* "C-x 8 Return" 'insert-char)

(defun algorithmically-named-p (name)
  "Return T if NAME is generated from a code point range.
Such names are excluded from completion, like Emacs's `ucs-names'."
  (or (str:starts-with-p "CJK UNIFIED IDEOGRAPH-" name)
      (str:starts-with-p "HANGUL SYLLABLE " name)
      (str:starts-with-p "TANGUT IDEOGRAPH-" name)))

(defun char-name-candidates ()
  "Return the cached list of (NAME . CHAR) pairs."
  (or *char-name-candidates*
      (setf *char-name-candidates*
            (loop :for code :below cl-unicode:+code-point-limit+
                  :for char := (code-char code)
                  :for name := (and char
                                    (or (cl-unicode:unicode-name code)
                                        (cl-unicode:unicode1-name code)))
                  :when (and name (not (algorithmically-named-p name)))
                    :collect (cons (string-upcase name) char)))))

(defun completion-char-names (input)
  "Return completion items for the character names matching INPUT.
A leading `*' makes INPUT match anywhere in the name."
  (let* ((substring-p (and (plusp (length input))
                           (char= (char input 0) #\*)))
         (input (string-upcase (if substring-p (subseq input 1) input))))
    (when (plusp (length input))
      (let* ((candidates (char-name-candidates))
             (matches (if substring-p
                          (remove-if-not (lambda (pair)
                                           (search input (car pair)))
                                         candidates)
                          (completion-strings input candidates :key #'car))))
        (loop :for (name . char) :in (subseq matches 0
                                             (min +completion-limit+
                                                  (length matches)))
              :collect (lem/completion-mode:make-completion-item
                        :label name
                        :detail (format nil "U+~4,'0X~@[ ~A~]"
                                        (char-code char)
                                        (and (graphic-char-p char)
                                             (string char)))))))))

(defun %scalar-value-p (code)
  "Return T if CODE is a Unicode scalar value."
  (and (integerp code)
       (<= 0 code #x10FFFF)
       (not (<= #xD800 code #xDFFF))))

(defun %code-point-char (code)
  "Return the character for CODE, or NIL if it is not a Unicode scalar value."
  (when (%scalar-value-p code)
    (code-char code)))

(defun %parse-hex (string)
  "Parse STRING as a hexadecimal code point, or return NIL."
  (when (and (plusp (length string))
             (every (lambda (c) (digit-char-p c 16)) string))
    (%code-point-char (parse-integer string :radix 16))))

(defun %parse-hash-notation (string)
  "Parse STRING as a Lisp radix notation, or return NIL."
  (when (and (plusp (length string))
             (char= (char string 0) #\#))
    (let ((*read-eval* nil))
      (let ((value (ignore-errors (read-from-string string nil nil))))
        (and (integerp value) (%code-point-char value))))))

(defun char-from-name-or-code (string)
  "Return the character designated by STRING.
STRING can be a Unicode character name, \"U+263A\", a hexadecimal
code point such as \"263A\", or Lisp radix notation such as \"#x263A\"."
  (let* ((string (string-trim '(#\Space #\Tab #\Newline) string))
         (upper (string-upcase string))
         (named (cl-unicode:character-named upper :try-hex-notation-p t)))
    (or (and named
             (%scalar-value-p (char-code named))
             named)
        (%parse-hex upper)
        (%parse-hash-notation string)
        (editor-error "No character named ~S" string))))

(define-command insert-char (&optional (count 1)) (:universal)
  "Insert the Unicode character specified by name or code point.
With a prefix argument, insert COUNT copies."
  (let ((char (char-from-name-or-code
               (prompt-for-string
                "Insert character (Unicode name or hex): "
                :completion-function #'completion-char-names
                :history-symbol 'insert-char))))
    (do-each-cursors ()
      (insert-character (current-point) char (max 0 count)))))
