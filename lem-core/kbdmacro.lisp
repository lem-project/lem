(in-package :cl-user)
(defpackage :lem.kbdmacro
  (:use :cl :lem)
  (:export
   :kbdmacro-start
   :kbdmacro-end
   :kbdmacro-execute
   :apply-macro-to-region-lines))
(in-package :lem.kbdmacro)

(defvar *last-macro-chars* nil)
(defvar *macro-running-p* nil)

(define-key *global-keymap* (kbd "C-x (") 'kbdmacro-start)
(define-command kbdmacro-start () ()
  (cond ((key-recording-p)
         (stop-record-key)
         (editor-error "Macro already active"))
        (t
         (start-record-key))))

(define-key *global-keymap* (kbd "C-x )") 'kbdmacro-end)
(define-command kbdmacro-end () ()
  (cond (*macro-running-p* t)
        ((not (key-recording-p))
         (editor-error "Macro not active"))
        (t
         (setq *last-macro-chars* (stop-record-key)))))

(define-key *global-keymap* (kbd "C-x e") 'kbdmacro-execute)
(define-command kbdmacro-execute (n) ("p")
  (cond ((key-recording-p)
         (editor-error "Macro already active"))
        (*macro-running-p*
         (editor-error "Macro already active"))
        ((null *last-macro-chars*)
         t)
        (t
         (let ((*macro-running-p* t))
           (loop
	      :repeat n
	      :while (execute-key-sequence *last-macro-chars*)
	      :finally (return t))))))

(define-command apply-macro-to-region-lines () ()
  (apply-region-lines (region-beginning)
                      (region-end)
                      (lambda (point)
                        (move-point (current-point) point)
                        (kbdmacro-execute 1)))
  t)
