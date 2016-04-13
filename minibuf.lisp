;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(minibuf-clear
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

(defvar *minibuf-window*)

(defun minibuffer-window () *minibuf-window*)
(defun minibuffer-window-p (window) (eq window (minibuffer-window)))
(defun minibuffer-window-active-p () (eq (current-window) (minibuffer-window)))
(defun minibuffer () (window-buffer (minibuffer-window)))
(defun minibufferp (buffer) (eq buffer (minibuffer)))

(define-major-mode minibuffer-mode nil
  (:name "minibuffer"
   :keymap *minibuf-keymap*))

(defun minibuf-init ()
  (let* ((buffer (make-buffer " *minibuffer*"))
         (window (make-window buffer
                              1
                              charms/ll:*cols*
                              (1- charms/ll:*lines*)
                              0)))
    (setq *minibuf-window* window)))

(defun minibuf-resize ()
  (window-set-pos (minibuffer-window)
                  (1- charms/ll:*lines*)
                  0)
  (window-set-size (minibuffer-window)
                   1
                   charms/ll:*cols*)
  (charms/ll:werase (window-screen (minibuffer-window)))
  (charms/ll:wnoutrefresh (window-screen (minibuffer-window))))

(defun minibuf-clear ()
  (when *mb-print-flag*
    (charms/ll:werase (window-screen (minibuffer-window)))
    (charms/ll:wnoutrefresh (window-screen (minibuffer-window)))
    (setq *mb-print-flag* nil)))

(defun minibuf-print (msg)
  (setq *mb-print-flag* t)
  (charms/ll:werase (window-screen (minibuffer-window)))
  (charms/ll:mvwaddstr
   (window-screen (minibuffer-window))
   0
   0
   (replace-string (string #\newline) "<NL>" msg))
  (charms/ll:wnoutrefresh (window-screen (minibuffer-window))))

(defun minibuf-print-sit-for (msg seconds)
  (minibuf-print msg)
  (sit-for seconds nil))

(defun minibuf-read-char (prompt)
  (setq *mb-print-flag* t)
  (minibuf-print prompt)
  (charms/ll:doupdate)
  (getch))

(defun minibuf-y-or-n-p (prompt)
  (setq *mb-print-flag* t)
  (do () (nil)
    (let ((c (minibuf-read-char (format nil "~a [y/n]? " prompt))))
      (cond
       ((char= #\y c)
        (return t))
       ((char= #\n c)
        (return nil))))))

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
  (when (minibuffer-window-active-p)
    (error 'switch-minibuffer-window)))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      (minibuffer-window)
      nil))

(define-command minibuf-read-line-confirm () ()
  (let ((str (minibuf-get-line)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (setq *minibuf-read-line-loop* nil)))
  t)

(define-command minibuf-read-line-completion () ()
  (when *minibuf-read-line-comp-f*
    (let ((target-str
           (region-string (point-min) (current-point))))
      (let ((str
             (with-current-window *minibuf-read-line-tmp-window*
               (popup-completion *minibuf-read-line-comp-f*
                                 target-str))))
        (delete-region (point-min) (current-point))
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
        (buffer-take-lines (minibuffer))))

(defun minibuf-point-linum ()
  (window-current-linum (minibuffer-window)))

(defun minibuf-point-charpos ()
  (window-current-charpos (minibuffer-window)))

(defun minibuf-read-line-refresh (prompt)
  (minibuf-print (concatenate 'string prompt (minibuf-get-line)))
  (charms/ll:wmove
   (window-screen (minibuffer-window))
   0
   (+ (multiple-value-bind (strings len)
          (split-string prompt #\newline)
        (+ (* (length "<NL>") (1- len))
           (apply #'+ (mapcar #'str-width strings))))
      (str-width
       (apply 'concatenate 'string
              (buffer-take-lines (minibuffer)
                                 1
                                 (1- (minibuf-point-linum)))))
      (* (length "<NL>") (1- (minibuf-point-linum)))
      (str-width
       (buffer-line-string (minibuffer)
                           (minibuf-point-linum))
       0
       (minibuf-point-charpos))))
  (charms/ll:wrefresh (window-screen (minibuffer-window))))

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
  (let ((*minibuf-read-line-tmp-window* (current-window)))
    (with-current-window (minibuffer-window)
      (let ((*universal-argument* nil)
            (minibuf-buffer-prev-string
             (join "" (buffer-take-lines (minibuffer))))
            (minibuf-buffer-prev-point
             (window-point (minibuffer-window)))
            (*minibuf-read-line-depth*
             (1+ *minibuf-read-line-depth*)))
        (erase-buffer)
        (minibuffer-mode)
        (when initial
          (insert-string initial))
        (unwind-protect (call-with-save-windows
                         (lambda ()
                           (minibuf-read-line-loop prompt comp-f existing-p)))
          (with-current-window (minibuffer-window)
            (erase-buffer)
            (insert-string minibuf-buffer-prev-string)
            (point-set minibuf-buffer-prev-point)))))))

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
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
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
