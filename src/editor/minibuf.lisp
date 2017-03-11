(in-package :lem)

(export '(*enable-recursive-minibuffers*
          *minibuf-keymap*
          minibuffer-prompt-attribute
          minibuffer-window-p
          minibuffer-window-height
          minibufferp
          message
          message-without-log
          minibuf-read-char
          active-minibuffer-window
          check-switch-minibuffer-window
          minibuf-read-line-confirm
          minibuf-read-line-completion
          minibuf-read-line-prev-history
          minibuf-read-line-next-history
          prompt-for-y-or-n-p
          prompt-for-line
          prompt-for-string
          prompt-for-integer
          prompt-for-buffer
          prompt-for-file))

(defparameter *minibuffer-window-height* 1)
(defvar *enable-recursive-minibuffers* t)

(defvar +recursive-minibuffer-break-tag+ (gensym))

(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)
(defvar *minibuffer-start-charpos*)

(define-attribute minibuffer-prompt-attribute
  (t :foreground "blue" :bold-p t))

(defun minibuffer-window () *minibuf-window*)
(defun minibuffer-window-p (window) (eq window (minibuffer-window)))
(defun minibuffer-window-active-p () (eq (current-window) (minibuffer-window)))
(defun minibuffer-window-height () *minibuffer-window-height*)
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
         (window (make-window buffer
                              0
                              (- (display-height)
                                 (minibuffer-window-height))
                              (display-width)
                              (minibuffer-window-height)
                              nil)))
    (setq *minibuf-window* window)))

(defun minibuf-update-size ()
  (window-set-pos (minibuffer-window) 0 (1- (display-height)))
  (window-set-size (minibuffer-window) (display-width) 1))

(defun log-message (string args)
  (when string
    (let ((msg (apply #'format nil string args)))
      (let ((buffer (get-buffer-create "*Messages*")))
        (with-open-stream (stream (make-buffer-output-stream
                                   (buffers-end buffer)))
          (fresh-line stream)
          (princ msg stream))))))

(defun message-1 (string args)
  (when (interactive-p)
    (let ((flag (minibuffer-window-active-p)))
      (print-echoarea (if (null string)
                          nil
                          (apply #'format nil string args))
                      flag)
      (when flag
        (sit-for 1 nil)
        (print-echoarea nil nil)))))

(defun message (string &rest args)
  (log-message string args)
  (message-1 string args)
  t)

(defun message-without-log (string &rest args)
  (message-1 string args)
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

(defun prompt-for-y-or-n-p (prompt)
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

(defvar *minibuf-read-line-history-table* (make-hash-table))
(defvar *minibuf-read-line-history*)

(defvar *minibuf-read-line-depth* 0)
(defvar *minibuf-prev-prompt* nil)

(defun check-switch-minibuffer-window ()
  (when (minibuffer-window-active-p)
    (editor-error "Cannot switch buffer in minibuffer window")))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      (minibuffer-window)
      nil))

(defun minibuffer-start-point ()
  (character-offset
   (copy-point (buffers-start (minibuffer))
               :temporary)
   *minibuffer-start-charpos*))

(defun get-minibuffer-string ()
  (points-to-string (minibuffer-start-point)
                    (buffers-end (minibuffer))))

(defun minibuffer-clear-input ()
  (delete-between-points (minibuffer-start-point)
                         (buffers-end (minibuffer))))

(define-command minibuf-read-line-confirm () ()
  (let ((str (get-minibuffer-string)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (throw 'minibuf-read-line-end t)))
  t)

(define-command minibuf-read-line-completion () ()
  (labels ((f (auto-insert)
             (multiple-value-bind (str items)
                 (funcall *minibuf-read-line-comp-f* (get-minibuffer-string))
               (declare (ignore str))
               (with-point ((start (minibuffer-start-point))
                            (end (current-point)))
                 (run-completion
                  (loop :for item? :in items
                        :for item := (typecase item?
                                       (string
                                        (make-completion-item :label item?
                                                              :start start
                                                              :end end))
                                       (completion-item
                                        item))
                        :when item
                        :collect item)
                  :auto-insert auto-insert
                  :restart-function (lambda () (f nil)))))))
    (when *minibuf-read-line-comp-f*
      (f t))))

(define-command minibuf-read-line-prev-history () ()
  (multiple-value-bind (str win)
      (lem.history:prev-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string (current-point) str))))

(define-command minibuf-read-line-next-history () ()
  (multiple-value-bind (str win)
      (lem.history:next-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string (current-point) str))))

(define-command minibuf-read-line-break () ()
  (throw +recursive-minibuffer-break-tag+
    +recursive-minibuffer-break-tag+))

(defun minibuf-window-update ()
  (screen-erase (window-screen (minibuffer-window)))
  (screen-print-string (window-screen (minibuffer-window)) 0 0
                       (points-to-string (buffers-start (minibuffer))
                                         (buffers-end (minibuffer))))
  (let ((point (buffer-point (minibuffer))))
    (screen-move-cursor (window-screen (minibuffer-window))
                        (point-charpos point)
                        (line-number-at-point point))))

(defun minibuf-read-line-loop (comp-f existing-p)
  (let ((*minibuf-read-line-existing-p* existing-p)
        (*minibuf-read-line-comp-f* comp-f))
    (catch 'minibuf-read-line-end
      (command-loop))
    (let ((str (get-minibuffer-string)))
      (lem.history:add-history *minibuf-read-line-history* str)
      str)))

(defun prompt-for-line (prompt initial comp-f existing-p history-name)
  (when (and (not *enable-recursive-minibuffers*) (< 0 *minibuf-read-line-depth*))
    (editor-error "ERROR: recursive use of minibuffer"))
  (let ((*minibuffer-calls-window* (current-window))
        (*minibuf-read-line-history* (let ((table (gethash history-name *minibuf-read-line-history-table*)))
                                       (or table
                                           (setf (gethash history-name *minibuf-read-line-history-table*)
                                                 (lem.history:make-history))))))
    (let ((result
           (catch +recursive-minibuffer-break-tag+
             (handler-case
                 (with-current-window (minibuffer-window)
                   (let ((minibuf-buffer-prev-string
                          (points-to-string (buffers-start (minibuffer))
                                            (buffers-end (minibuffer))))
                         (prev-prompt-length
                          (when *minibuf-prev-prompt*
                            (length *minibuf-prev-prompt*)))
                         (minibuf-buffer-prev-point
                          (window-point (minibuffer-window)))
                         (*minibuf-prev-prompt* prompt)
                         (*minibuf-read-line-depth*
                          (1+ *minibuf-read-line-depth*)))
                     (let ((*inhibit-read-only* t))
                       (erase-buffer))
                     (minibuffer-mode)
                     (unless (string= "" prompt)
                       (insert-string (current-point) prompt
                                      :attribute 'minibuffer-prompt-attribute
                                      :read-only t
                                      :field t)
                       (character-offset (current-point) (length prompt)))
                     (let ((*minibuffer-start-charpos* (point-charpos (current-point))))
                       (when initial
                         (insert-string (current-point) initial))
                       (unwind-protect (call-with-save-windows
                                        (minibuffer-calls-window)
                                        (lambda ()
                                          (minibuf-read-line-loop comp-f existing-p)))
                         (with-current-window (minibuffer-window)
                           (let ((*inhibit-read-only* t))
                             (erase-buffer))
                           (insert-string (current-point) minibuf-buffer-prev-string)
                           (when prev-prompt-length
                             (with-point ((start (current-point))
                                          (end (current-point)))
                               (line-start start)
                               (line-offset end 0 prev-prompt-length)
                               (put-text-property start end :attribute 'minibuffer-prompt-attribute)
                               (put-text-property start end :read-only t)
                               (put-text-property start end :field t)))
                           (move-point (current-point) minibuf-buffer-prev-point))))))
               (editor-abort (c)
                             (error c))))))
      (if (eq result +recursive-minibuffer-break-tag+)
          (error 'editor-abort)
          result))))

(defun prompt-for-string (prompt &optional initial)
  (prompt-for-line prompt (or initial "") nil nil 'mh-read-string))

(defun prompt-for-integer (prompt &optional min max)
  (parse-integer
   (prompt-for-line prompt "" nil
                    #'(lambda (str)
                        (multiple-value-bind (n len)
                            (parse-integer str :junk-allowed t)
                          (and
                           n
                           (/= 0 (length str))
                           (= (length str) len)
                           (if min (<= min n) t)
                           (if max (<= n max) t))))
                    'mh-read-number)))

(defun prompt-for-buffer (prompt &optional default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result (prompt-for-line
                 prompt
                 ""
                 'completion-buffer-name
                 (and existing
                      (lambda (name)
                        (member name (buffer-list) :test #'string= :key #'buffer-name)))
                 'mh-read-buffer)))
    (if (string= result "")
        default
        result)))

(defun prompt-for-file (prompt &optional directory (default (buffer-directory)) existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result
         (prompt-for-line prompt
                          directory
                          (lambda (str)
                            (completion-file str directory))
                          (and existing #'cl-fad:file-exists-p)
                          'mh-read-file)))
    (if (string= result "")
        default
        result)))
