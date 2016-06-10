(in-package :lem)

(export '(message
          minibuf-y-or-n-p
          minibuf-read-char
          *minibuf-keymap*
          active-minibuffer-window
          check-switch-minibuffer-window
          minibuf-read-line-confirm
          minibuf-read-line-completion
          minibuf-read-line-prev-history
          minibuf-read-line-next-history
          minibuf-get-line
          minibuf-read-line
          minibuf-read-string
          minibuf-read-number
          minibuf-read-buffer
          minibuf-read-file))

(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)

(defun minibuffer-window () *minibuf-window*)
(defun minibuffer-window-p (window) (eq window (minibuffer-window)))
(defun minibuffer-window-active-p () (eq (current-window) (minibuffer-window)))
(defun minibuffer () (window-buffer (minibuffer-window)))
(defun minibufferp (buffer) (eq buffer (minibuffer)))
(defun minibuffer-calls-window () *minibuffer-calls-window*)

(define-major-mode minibuffer-mode nil
  (:name "minibuffer"
   :keymap *minibuf-keymap*
   :syntax-table (make-syntax-table
                  :symbol-chars '(#\_ #\-))))

(defun minibuf-init ()
  (let* ((buffer (make-buffer " *minibuffer*"))
         (window (make-window buffer 0 (1- (display-height)) (display-width) 1)))
    (setq *minibuf-window* window)))

(defun minibuf-update-size ()
  (window-set-pos (minibuffer-window) 0 (1- (display-height)))
  (window-set-size (minibuffer-window) (display-width) 1))

(defun message (string &rest args)
  (when (interactive-p)
    (let ((flag (minibuffer-window-active-p)))
      (message-internal (if (null string)
                            nil
                            (replace-string (string #\newline)
                                            "<NL>"
                                            (apply #'format nil string args)))
                        flag)
      (when flag
        (sit-for 1 nil)
        (message-internal nil nil))))
  t)

(defun minibuf-read-char (prompt)
  (when (interactive-p)
    (message prompt)
    (redraw-display))
  (let ((c (read-key)))
    (when (interactive-p)
      (message nil))
    (if (char= c C-g)
        (error 'editor-abort)
        c)))

(defun minibuf-y-or-n-p (prompt)
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
(define-key *minibuf-keymap* (kbd "M-p") 'minibuf-read-line-prev-history)
(define-key *minibuf-keymap* (kbd "M-n") 'minibuf-read-line-next-history)
(define-key *minibuf-keymap* (kbd "C-g") 'minibuf-read-line-break)

(defvar *minibuf-read-line-prompt*)
(defvar *minibuf-read-line-comp-f*)
(defvar *minibuf-read-line-existing-p*)

(defvar *minibuf-read-line-history* (make-history))

(defvar *minibuf-read-line-depth* 0)

(defun check-switch-minibuffer-window ()
  (when (minibuffer-window-active-p)
    (editor-error "Cannot switch buffer in minibuffer window")))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      (minibuffer-window)
      nil))

(define-command minibuf-read-line-confirm () ()
  (let ((str (minibuf-get-line)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (throw 'minibuf-read-line-end t)))
  t)

(define-command minibuf-read-line-completion () ()
  (when *minibuf-read-line-comp-f*
    (start-completion *minibuf-read-line-comp-f*
                      (region-string (point-min) (current-point))))
  t)

(define-command minibuf-read-line-prev-history () ()
  (multiple-value-bind (str win)
      (prev-history *minibuf-read-line-history*)
    (when win
      (buffer-erase)
      (insert-string str))))

(define-command minibuf-read-line-next-history () ()
  (multiple-value-bind (str win)
      (next-history *minibuf-read-line-history*)
    (when win
      (buffer-erase)
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

(defun minibuf-window-update ()
  (let ((prompt *minibuf-read-line-prompt*))
    (screen-erase (window-screen (minibuffer-window)))
    (screen-print-string (window-screen (minibuffer-window)) 0 0
                         (concatenate 'string
                                      prompt
                                      (replace-string (string #\newline) "<NL>" (minibuf-get-line)))
                         nil)
    (screen-move-cursor (window-screen (minibuffer-window))
                        (+ (multiple-value-bind (strings len)
                               (split-string prompt #\newline)
                             (+ (* (length "<NL>") (1- len))
                                (reduce #'+ (mapcar #'str-width strings))))
                           (str-width
                            (join ""
                                  (buffer-take-lines (minibuffer)
                                                     1
                                                     (1- (minibuf-point-linum)))))
                           (* (length "<NL>") (1- (minibuf-point-linum)))
                           (str-width
                            (buffer-line-string (minibuffer)
                                                (minibuf-point-linum))
                            0
                            (minibuf-point-charpos)))
                        0)))

(defun minibuf-read-line-loop (prompt comp-f existing-p)
  (let ((*minibuf-read-line-prompt* prompt)
        (*minibuf-read-line-existing-p* existing-p)
        (*minibuf-read-line-comp-f* comp-f))
    (catch 'minibuf-read-line-end
      (do-commandloop ()
        (redraw-display)
        (let ((cmd (read-key-command)))
          (handler-case (cmd-call cmd nil)
            (editor-abort (c)
              (when (/= (editor-abort-depth c)
                        *minibuf-read-line-depth*)
                (error c)))
            (readonly ()
              (message "Read Only"))
            (editor-error (c)
              (message (editor-error-message c)))))))
    (let ((str (minibuf-get-line)))
      (add-history *minibuf-read-line-history* str)
      str)))

(defun minibuf-read-line (prompt initial comp-f existing-p)
  (let ((*minibuffer-calls-window* (current-window)))
    (handler-case
        (with-allow-interrupt nil
          (with-current-window (minibuffer-window)
            (let ((minibuf-buffer-prev-string
                   (join "" (buffer-take-lines (minibuffer))))
                  (minibuf-buffer-prev-point
                   (window-point (minibuffer-window)))
                  (*minibuf-read-line-depth*
                   (1+ *minibuf-read-line-depth*)))
              (buffer-erase)
              (minibuffer-mode)
              (when initial
                (insert-string initial))
              (unwind-protect (call-with-save-windows
                               (minibuffer-calls-window)
                               (lambda ()
                                 (minibuf-read-line-loop prompt comp-f existing-p)))
                (with-current-window (minibuffer-window)
                  (buffer-erase)
                  (insert-string minibuf-buffer-prev-string)
                  (point-set minibuf-buffer-prev-point))))))
      (editor-abort (c)
                    (error c)))))

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
  (let ((result
         (minibuf-read-line prompt
                            directory
                            (lambda (str)
                              (setf str (expand-file-name str))
                              (let* ((dirname (directory-namestring str))
                                     (files (mapcar #'namestring (cl-fad:list-directory dirname))))
                                (let ((strings
                                       (loop
                                         :for pathname :in (or (directory str) (list str))
                                         :for str := (namestring pathname)
                                         :append
                                         (multiple-value-bind (andstr strings)
                                             (completion (enough-namestring str dirname)
                                                         files
                                                         :test #'completion-test
                                                         :separator "-."
                                                         :key #'(lambda (path)
                                                                  (enough-namestring path dirname)))
                                           (when andstr strings)))))
                                  (values (logand-strings strings) strings))))
                            (and existing #'cl-fad:file-exists-p))))
    (if (string= result "")
        default
        result)))
