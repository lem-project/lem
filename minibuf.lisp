;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*minibuf-window*
          minibuf-clear
          minibuf-print
          minibuf-y-or-n-p
          minibuf-read-char
          *minibuf-keymap*
          active-minibuffer-window
          check-switch-minibuffer-window
          minibuf-read-line-confirm
          minibuf-read-line-completion
          minibuf-read-line-clear-before
          minibuf-read-line-prev-history
          minibuf-read-line-next-history
          minibuf-get-line
          minibuf-read-line-refresh
          minibuf-read-line
          minibuf-read-string
          minibuf-read-number
          minibuf-read-buffer
          minibuf-read-file))

(defvar *mb-print-flag* nil)

(define-major-mode minibuffer-mode nil
  (:name "minibuffer"
   :keymap-var *minibuf-keymap*))

(defun minibuf-init ()
  (let* ((buffer (make-buffer " *minibuffer*"))
         (window (make-window buffer
                              1
                              charms/ll:*cols*
                              (1- charms/ll:*lines*)
                              0)))
    (setq *minibuf-window* window)))

(defun minibuf-resize ()
  (window-set-pos *minibuf-window*
                  (1- charms/ll:*lines*)
                  0)
  (window-set-size *minibuf-window*
                   1
                   charms/ll:*cols*)
  (charms/ll:werase (window-win *minibuf-window*))
  (charms/ll:wrefresh (window-win *minibuf-window*)))

(defun minibuf-clear ()
  (when *mb-print-flag*
    (charms/ll:werase (window-win *minibuf-window*))
    (charms/ll:wrefresh (window-win *minibuf-window*))
    (setq *mb-print-flag* nil)))

(defun minibuf-print (msg)
  (setq *mb-print-flag* t)
  (charms/ll:werase (window-win *minibuf-window*))
  (charms/ll:mvwaddstr
   (window-win *minibuf-window*)
   0
   0
   (replace-string (string #\newline) "<NL>" msg))
  (charms/ll:wrefresh (window-win *minibuf-window*)))

(defun minibuf-print-sit-for (msg seconds)
  (minibuf-print msg)
  (sit-for seconds nil))

(defun minibuf-y-or-n-p (prompt)
  (setq *mb-print-flag* t)
  (do () (nil)
    (minibuf-print (format nil "~a [y/n]? " prompt))
    (let ((c (getch)))
      (cond
       ((char= #\y c)
        (return t))
       ((char= #\n c)
        (return nil))))))

(defun minibuf-read-char (prompt)
  (setq *mb-print-flag* t)
  (minibuf-print prompt)
  (getch))

(define-key *minibuf-keymap* (kbd "C-j") 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* (kbd "C-m") 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* (kbd "C-i") 'minibuf-read-line-completion)
(define-key *minibuf-keymap* (kbd "C-u") 'minibuf-read-line-clear-before)
(define-key *minibuf-keymap* (kbd "C-p") 'minibuf-read-line-prev-history)
(define-key *minibuf-keymap* (kbd "C-n") 'minibuf-read-line-next-history)
(define-key *minibuf-keymap* (kbd "C-g") 'minibuf-read-line-break)

(defvar *minibuf-read-line-tmp-window*)

(defvar *minibuf-read-line-prompt*)
(defvar *minibuf-read-line-loop*)
(defvar *minibuf-read-line-comp-f*)
(defvar *minibuf-read-line-existing-p*)

(defvar *minibuf-read-line-history* (make-history))

(defvar *minibuf-read-line-depth* 0)

(defun check-switch-minibuffer-window ()
  (when (eq (current-window) *minibuf-window*)
    (error 'switch-minibuffer-window)))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      *minibuf-window*
      nil))

(define-command minibuf-read-line-confirm () ()
  (let ((str (minibuf-get-line)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (delete-completion-window)
      (setq *minibuf-read-line-loop* nil)))
  t)

(define-command minibuf-read-line-completion () ()
  (when *minibuf-read-line-comp-f*
    (let ((target-str
           (region-string (point-min) (current-point))))
      (let ((str
             (let ((*current-window* *minibuf-read-line-tmp-window*))
               (popup-completion *minibuf-read-line-comp-f*
                                 target-str))))
        (minibuf-read-line-clear-before)
        (insert-string str))))
  t)

(define-command minibuf-read-line-clear-before () ()
  (kill-region (point-min) (current-point))
  t)

(define-command minibuf-read-line-prev-history () ()
  (multiple-value-bind (str win)
      (prev-history *minibuf-read-line-history*)
    (when win
      (kill-region (point-min) (point-max))
      (insert-string str))))

(define-command minibuf-read-line-next-history () ()
  (multiple-value-bind (str win)
      (next-history *minibuf-read-line-history*)
    (when win
      (kill-region (point-min) (point-max))
      (insert-string str))))

(define-command minibuf-read-line-break () ()
  (error 'editor-abort :depth (1- *minibuf-read-line-depth*)))

(defun minibuf-get-line ()
  (join (string #\newline)
        (buffer-take-lines (window-buffer *minibuf-window*))))

(defun minibuf-point-linum ()
  (window-current-linum *minibuf-window*))

(defun minibuf-point-charpos ()
  (window-current-charpos *minibuf-window*))

(defun minibuf-read-line-refresh (prompt)
  (minibuf-print (concatenate 'string prompt (minibuf-get-line)))
  (charms/ll:wmove
   (window-win *minibuf-window*)
   0
   (+ (multiple-value-bind (strings len)
          (split-string prompt #\newline)
        (+ (* (length "<NL>") (1- len))
           (apply #'+ (mapcar #'str-width strings))))
      (str-width
       (apply 'concatenate 'string
              (buffer-take-lines (window-buffer *minibuf-window*)
                                 1
                                 (1- (minibuf-point-linum)))))
      (* (length "<NL>") (1- (minibuf-point-linum)))
      (str-width
       (buffer-line-string (window-buffer *minibuf-window*)
                           (minibuf-point-linum))
       0
       (minibuf-point-charpos))))
  (charms/ll:wrefresh (window-win *minibuf-window*)))

(defun minibuf-window-update ()
  (minibuf-read-line-refresh *minibuf-read-line-prompt*))

(defun minibuf-read-line-loop (prompt comp-f existing-p)
  (do ((*minibuf-read-line-loop* t)
       (*minibuf-read-line-prompt* prompt)
       (*minibuf-read-line-existing-p* existing-p)
       (*minibuf-read-line-comp-f* comp-f)
       (*curr-flags* (make-flags) (make-flags))
       (*last-flags* (make-flags) *curr-flags*))
      ((not *minibuf-read-line-loop*)
       (let ((str (minibuf-get-line)))
         (add-history *minibuf-read-line-history* str)
         str))
    (window-maybe-update)
    (let* ((key (input-key))
           (cmd (find-keybind key)))
      (handler-case (cmd-call cmd nil)
        (editor-abort (c)
                      (if (/= (editor-abort-depth c)
                              *minibuf-read-line-depth*)
                          (error c)))
        (switch-minibuffer-window ()
                                  (minibuf-print-sit-for "Cannot switch buffer in minibuffer window"
                                                         1))))))

(defun minibuf-read-line (prompt initial comp-f existing-p)
  (let ((*minibuf-read-line-tmp-window* (current-window))
        (*current-window* *minibuf-window*)
        (*universal-argument* nil)
        (minibuf-buffer-prev-string
         (join "" (buffer-take-lines (window-buffer *minibuf-window*))))
        (minibuf-buffer-prev-point
         (window-point *minibuf-window*))
        (*minibuf-read-line-depth*
         (1+ *minibuf-read-line-depth*)))
    (erase-buffer)
    (minibuffer-mode)
    (when initial
      (insert-string initial))
    (unwind-protect (minibuf-read-line-loop prompt comp-f existing-p)
      (when (deleted-window-p (current-window))
        (setq *current-window* (car (window-list))))
      (with-current-window *minibuf-window*
        (erase-buffer)
        (insert-string minibuf-buffer-prev-string)
        (point-set minibuf-buffer-prev-point)))))

(defun minibuf-read-string (prompt &optional initial)
  (minibuf-read-line prompt (or initial "") nil nil))

(defun minibuf-read-number (prompt &optional min max)
  (parse-integer
   (minibuf-read-line prompt "" nil
                      #'(lambda (str)
                          (multiple-value-bind (n len)
                              (parse-integer str :junk-allowed t)
                            (and
                             n
                             (/= 0 (length str))
                             (= (length str) len)
                             (if min (<= min n) t)
                             (if max (<= n max) t)))))))

(defun minibuf-read-buffer (prompt &optional default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let* ((buffer-names (mapcar 'buffer-name *buffer-list*))
         (result (minibuf-read-line
                  prompt
                  ""
                  #'(lambda (name)
                      (completion name buffer-names))
                  (and existing
                       #'(lambda (name)
                           (member name buffer-names :test 'string=))))))
    (if (string= result "")
        default
        result)))

(defun minibuf-read-file (prompt &optional directory default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result (minibuf-read-line prompt
                                   directory
                                   #'file-completion
                                   (and existing #'cl-fad:file-exists-p))))
    (if (string= result "")
        default
        result)))
