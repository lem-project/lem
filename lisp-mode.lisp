(in-package :lem)

(defvar *lisp-mode-keymap*
  (make-keymap "lisp" nil *global-keymap*))

(define-mode lisp-mode
  :name "lisp-mode"
  :keymap *lisp-mode-keymap*
  :syntax-table (make-syntax-table
                 :space-chars '(#\space #\tab #\newline)
                 :symbol-chars '(#\$ #\& #\* #\+ #\- #\_ #\< #\>)
                 :paren-alist '((#\( . #\))
                                (#\[ . #\])
                                (#\{ . #\}))
                 :string-quote-chars '(#\")
                 :escape-chars '(#\\)
                 :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
                 :comment-starter-chars '(#\;)
                 :comment-ender-chars '(#\newline)))
