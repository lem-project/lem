(in-package :lem)

(export '(minibuffer-window-p
          minibuffer-window-height
          message
          minibuf-y-or-n-p
          minibuf-read-char
          *minibuf-keymap*
          active-minibuffer-window
          check-switch-minibuffer-window
          minibuf-read-line-confirm
          minibuf-read-line-completion
          minibuf-read-line-prev-history
          minibuf-read-line-next-history
          prompt-for-line
          prompt-for-string
          prompt-for-integer
          prompt-for-buffer
          prompt-for-file))

(defparameter *minibuffer-window-height* 1)

(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)
(defvar *minibuffer-start-point*)
(defvar *minibuffer-prompt-attribute* (make-attribute "blue" nil :bold-p t))

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

(defun message (string &rest args)
  (log-message string args)
  (when (interactive-p)
    (let ((flag (minibuffer-window-active-p)))
      (print-echoarea (if (null string)
                          nil
                          (apply #'format nil string args))
                      flag)
      (when flag
        (sit-for 1 nil)
        (print-echoarea nil nil))))
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

(defvar *minibuf-read-line-history-table* (make-hash-table))
(defvar *minibuf-read-line-history*)

(defvar *minibuf-read-line-depth* 0)

(defun check-switch-minibuffer-window ()
  (when (minibuffer-window-active-p)
    (editor-error "Cannot switch buffer in minibuffer window")))

(defun active-minibuffer-window ()
  (if (/= 0 *minibuf-read-line-depth*)
      (minibuffer-window)
      nil))

(defun get-minibuffer-string ()
  (points-to-string *minibuffer-start-point*
                    (buffers-end (minibuffer))))

(defun minibuffer-clear-input ()
  (delete-between-points *minibuffer-start-point*
                         (buffers-end (minibuffer))))

(define-command minibuf-read-line-confirm () ()
  (let ((str (get-minibuffer-string)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (throw 'minibuf-read-line-end t)))
  t)

(define-command minibuf-read-line-completion () ()
  (when *minibuf-read-line-comp-f*
    (start-completion *minibuf-read-line-comp-f*
                      (get-minibuffer-string)))
  t)

(define-command minibuf-read-line-prev-history () ()
  (multiple-value-bind (str win)
      (prev-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string (current-point) str))))

(define-command minibuf-read-line-next-history () ()
  (multiple-value-bind (str win)
      (next-history *minibuf-read-line-history*)
    (when win
      (minibuffer-clear-input)
      (insert-string (current-point) str))))

(define-command minibuf-read-line-break () ()
  (error 'editor-abort :depth (1- *minibuf-read-line-depth*)))

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
      (do-commandloop ()
        (redraw-display)
        (let ((cmd (read-key-command)))
          (handler-case (cmd-call cmd nil)
            (editor-abort (c)
                          (when (/= (editor-abort-depth c)
                                    *minibuf-read-line-depth*)
                            (error c)))
            (read-only-error ()
                             (message "Read Only"))
            (editor-error (c)
                          (message (editor-error-message c)))))))
    (let ((str (get-minibuffer-string)))
      (add-history *minibuf-read-line-history* str)
      str)))

(defun prompt-for-line (prompt initial comp-f existing-p history-name)
  (when (< 0 *minibuf-read-line-depth*)
    ;; 再帰的にミニバッファを使うと、深さを一つ戻すときにそのミニバッファのテキストプロパティも戻す必要があって
    ;; それがうまくできていないから、とりあえず再帰的なミニバッファの仕様を禁止している
    (editor-error "ERROR: recursive use of minibuffer"))
  (let ((*minibuffer-calls-window* (current-window))
        (*minibuf-read-line-history* (let ((table (gethash history-name *minibuf-read-line-history-table*)))
                                       (or table
                                           (setf (gethash history-name *minibuf-read-line-history-table*)
                                                 (make-history))))))
    (handler-case
        (call-with-allow-interrupt
         nil
         (lambda ()
           (with-current-window (minibuffer-window)
             (let ((minibuf-buffer-prev-string
                    (points-to-string (buffers-start (minibuffer))
                                      (buffers-end (minibuffer))))
                   (minibuf-buffer-prev-point
                    (window-point (minibuffer-window)))
                   (*minibuf-read-line-depth*
                    (1+ *minibuf-read-line-depth*)))
               (let ((*inhibit-read-only* t))
                 (erase-buffer))
               (minibuffer-mode)
               (unless (string= "" prompt)
                 (insert-string (current-point) prompt
                                :attribute *minibuffer-prompt-attribute*
                                :read-only t
                                :field t)
                 (character-offset (current-point) (length prompt)))
               (with-point ((*minibuffer-start-point* (current-point) :right-inserting))
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
                     (move-point (current-point) minibuf-buffer-prev-point))))))))
      (editor-abort (c)
                    (error c)))))

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

(defun prompt-for-file (prompt &optional directory default existing)
  (when default
    (setq prompt (format nil "~a(~a) " prompt default)))
  (let ((result
         (prompt-for-line prompt
                          directory
                          'completion-file
                          (and existing #'cl-fad:file-exists-p)
                          'mh-read-file)))
    (if (string= result "")
        default
        result)))
