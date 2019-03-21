(in-package :lem-lisp-mode)

(define-key *lisp-mode-keymap* "C-c C-d a" 'lisp-apropos)
(define-key *lisp-mode-keymap* "C-c C-d z" 'lisp-apropos-all)
(define-key *lisp-mode-keymap* "C-c C-d p" 'lisp-apropos-package)

(define-attribute apropos-headline-attribute
  (t :bold-p t))

(defvar *lisp-apropos-mode-keymap*
  (make-keymap :name '*lisp-apropos-mode-keymap*
               :parent *lisp-mode-keymap*))

(define-key *lisp-apropos-mode-keymap* "q" 'quit-window)
(define-key *lisp-apropos-mode-keymap* "Return" 'lem.language-mode::find-definitions)

(define-major-mode lisp-apropos-mode lisp-mode
    (:name "lisp-apropos"
     :keymap *lisp-apropos-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) nil))

(defun show-apropos (data package)
  (let ((buffer (make-buffer "*lisp-apropos*")))
    (switch-to-buffer buffer)
    (lisp-apropos-mode)
    (erase-buffer buffer)
    (save-excursion
      (let ((point (current-point)))
        (loop :for plist :in data
              :do (let ((designator (cadr plist))
                        (plist1 (cddr plist)))
                    (insert-string point designator
                                   :attribute 'apropos-headline-attribute)
                    (loop :for (k v) :on plist1 :by #'cddr
                          :do (insert-string point (format nil "~%  ~A: ~A" k v)))
                    (insert-character point #\newline 2)))))
    (lisp-set-package package)))

(defun lisp-apropos-internal (string only-external-p package case-sensitive-p)
  (show-apropos (lisp-eval
                 `(swank:apropos-list-for-emacs ,string
                                                ,only-external-p
                                                ,case-sensitive-p
                                                ,package))
                (or package
                    (current-package))))

(define-command lisp-apropos (&optional arg) ("P")
  (check-connection)
  (let ((string)
        (only-external-p t)
        (package nil)
        (case-sensitive-p nil))
    (if arg
        (setq string (prompt-for-string "lisp Apropos: ")
              only-external-p (prompt-for-y-or-n-p "External symbols only? ")
              package (let ((name (read-package-name)))
                        (if (string= "" name)
                            nil
                            name))
              case-sensitive-p (prompt-for-y-or-n-p "Case-sensitive? "))
        (setq string (prompt-for-string "lisp Apropos: ")))
    (lisp-apropos-internal string only-external-p package case-sensitive-p)))

(define-command lisp-apropos-all () ()
  (check-connection)
  (lisp-apropos-internal (prompt-for-string "lisp Apropos: ")
                         nil nil nil))

(define-command lisp-apropos-package (internal) ("P")
  (check-connection)
  (let ((package (read-package-name)))
    (lisp-apropos-internal ""
                           (not internal)
                           (if (string= package "")
                               (current-package)
                               package)
                           nil)))
