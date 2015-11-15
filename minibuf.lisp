;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(*minibuf-window*
          minibuf-clear
          minibuf-print
          minibuf-y-or-n-p
          minibuf-read-char
          *minibuf-keymap*
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

(defvar *minibuf-keymap*
  (make-keymap "minibuffer"))

(define-major-mode minibuffer-mode nil
  (:name "minibuffer"
   :keymap *minibuf-keymap*))

(defun minibuf-init ()
  (let* ((buffer (make-buffer " *minibuffer*"))
         (window (make-window buffer
                              1
                              cl-charms/low-level:*cols*
                              (1- cl-charms/low-level:*lines*)
                              0)))
    (setq *minibuf-window* window)))

(defun minibuf-resize ()
  (window-set-pos *minibuf-window*
                  (1- cl-charms/low-level:*lines*)
                  0)
  (window-set-size *minibuf-window*
                   1
                   cl-charms/low-level:*cols*)
  (cl-charms/low-level:werase (window-win *minibuf-window*))
  (cl-charms/low-level:wrefresh (window-win *minibuf-window*)))

(defun minibuf-clear ()
  (when *mb-print-flag*
    (cl-charms/low-level:werase (window-win *minibuf-window*))
    (cl-charms/low-level:wrefresh (window-win *minibuf-window*))
    (setq *mb-print-flag* nil)))

(defun minibuf-print (msg)
  (setq *mb-print-flag* t)
  (cl-charms/low-level:werase (window-win *minibuf-window*))
  (cl-charms/low-level:mvwaddstr
   (window-win *minibuf-window*)
   0
   0
   (replace-string (string #\newline) "<NL>" msg))
  (cl-charms/low-level:wrefresh (window-win *minibuf-window*)))

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
(define-key *minibuf-keymap* (kbd "C-d") 'minibuf-read-line-delete-char)
(define-key *minibuf-keymap* (kbd "[dc]") 'minibuf-read-line-delete-char)

(defvar *minibuf-read-line-tmp-window*)

(defvar *minibuf-read-line-loop*)
(defvar *minibuf-read-line-comp-f*)
(defvar *minibuf-read-line-existing-p*)

(defvar *minibuf-read-line-history* (make-history))
(defvar *minibuf-read-line-busy-p* nil)

(defvar *minibuf-allow-eof-p*)
(defvar *minibuf-eof-error* (gensym))

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
           (region-string (point-min) (point))))
      (let ((str
             (let ((*current-window* *minibuf-read-line-tmp-window*))
               (popup-completion *minibuf-read-line-comp-f*
                                 target-str))))
        (minibuf-read-line-clear-before)
        (insert-string str))))
  t)

(define-command minibuf-read-line-clear-before () ()
  (kill-region (point-min) (point))
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
  (throw 'abort 'abort))

(define-command minibuf-read-line-delete-char (n) ("P")
  (if *minibuf-allow-eof-p*
      (if (string= "" (region-string (point-min) (point-max)))
          (throw *minibuf-eof-error* nil)
          (delete-char n))
      (delete-char n)))

(defun minibuf-get-line ()
  (buffer-line-string (window-buffer *minibuf-window*) 1))

(defun minibuf-read-line-refresh (prompt)
  (minibuf-print (concatenate 'string prompt (minibuf-get-line)))
  (cl-charms/low-level:wmove (window-win *minibuf-window*)
                             (1- (window-cur-linum *minibuf-window*))
                             (+ (length prompt)
                                (window-cur-col *minibuf-window*)))
  (cl-charms/low-level:wrefresh (window-win *minibuf-window*)))

(defun minibuf-read-line (prompt initial comp-f existing-p
                                 &optional allow-eof-p)
  (when *minibuf-read-line-busy-p*
    (return-from minibuf-read-line nil))
  (let ((*minibuf-read-line-tmp-window* *current-window*)
        (*current-window* *minibuf-window*)
        (*minibuf-read-line-busy-p* t)
        (*universal-argument* nil)
        (*minibuf-allow-eof-p* allow-eof-p))
    (erase-buffer)
    (minibuffer-mode)
    (when initial
      (insert-string initial))
    (catch *minibuf-eof-error*
      (do ((*minibuf-read-line-loop* t)
           (*minibuf-read-line-existing-p* existing-p)
           (*minibuf-read-line-comp-f* comp-f)
           (*curr-flags* (make-flags) (make-flags))
           (*last-flags* (make-flags) *curr-flags*))
          ((not *minibuf-read-line-loop*)
           (let ((str (minibuf-get-line)))
             (add-history *minibuf-read-line-history* str)
             str))
        (minibuf-read-line-refresh prompt)
        (let* ((key (input-key))
               (cmd (find-keybind key)))
          (cmd-call cmd 1))))))

(defun minibuf-read-string (prompt &optional initial allow-eof-p)
  (minibuf-read-line prompt (or initial "") nil nil allow-eof-p))

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
