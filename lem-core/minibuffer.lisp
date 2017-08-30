(in-package :lem)

(export '(*enable-recursive-minibuffers*
          *minibuffer-completion-function*
          *minibuffer-file-complete-function*
          *minibuf-keymap*
          minibuffer-prompt-attribute
          minibuffer-window-p
          minibuffer-window-height
          minibufferp
          message
          message-without-log
          message-buffer
          active-echoarea-p
          prompt-for-character
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

(defvar *current-minibuffer-window*)
(defvar *echoarea-window*)
(defvar *minibuf-window*)
(defvar *minibuffer-calls-window*)
(defvar *minibuffer-start-charpos*)

(defvar *minibuffer-completion-function* nil)
(defvar *minibuffer-file-complete-function* nil)

(define-attribute minibuffer-prompt-attribute
  (t :foreground "blue" :bold-p t))

(defun current-minibuffer-window () *current-minibuffer-window*)
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
  (setf *minibuf-window*
        (make-window (make-buffer "*minibuffer*" :temporary t :enable-undo-p t)
                     0
                     (- (display-height)
                        (minibuffer-window-height))
                     (display-width)
                     (minibuffer-window-height)
                     nil))
  (setf *echoarea-window*
        (make-window (make-buffer "*echoarea*" :temporary t :enable-undo-p nil)
                     0
                     (- (display-height)
                        (minibuffer-window-height))
                     (display-width)
                     (minibuffer-window-height)
                     nil))
  (setf *current-minibuffer-window* *echoarea-window*))

(defun minibuf-update-size ()
  (window-set-pos (minibuffer-window) 0 (1- (display-height)))
  (window-set-size (minibuffer-window) (display-width) 1)
  (window-set-pos *echoarea-window* 0 (1- (display-height)))
  (window-set-size *echoarea-window* (display-width) 1))

(defun log-message (string args)
  (when string
    (let ((msg (apply #'format nil string args)))
      (let ((buffer (make-buffer "*Messages*")))
        (with-open-stream (stream (make-buffer-output-stream
                                   (buffer-end-point buffer)))
          (fresh-line stream)
          (princ msg stream))))))

(defun message-without-log (string &rest args)
  (cond (string
         (erase-buffer (window-buffer *echoarea-window*))
         (let ((point (buffer-point (window-buffer *echoarea-window*))))
           (insert-string point (apply #'format nil string args)))
         (when (eq *current-minibuffer-window* *minibuf-window*)
           (handler-case (let ((*current-minibuffer-window*
                                 *echoarea-window*))
                           (sit-for 1 t))
             (editor-abort ()
               (minibuf-read-line-break)))))
        (t
         (erase-buffer (window-buffer *echoarea-window*))))
  t)

(defun message (string &rest args)
  (log-message string args)
  (apply #'message-without-log string args)
  t)

(defun message-buffer (buffer)
  (erase-buffer (window-buffer *echoarea-window*))
  (insert-buffer (buffer-point (window-buffer *echoarea-window*)) buffer))

(defun active-echoarea-p ()
  (and (eq *current-minibuffer-window* *echoarea-window*)
       (let ((buffer (window-buffer *echoarea-window*)))
         (point< (buffer-start-point buffer)
                 (buffer-end-point buffer)))))

(defun prompt-for-character (prompt)
  (when (interactive-p)
    (message "~A" prompt)
    (redraw-display))
  (let ((c (read-key)))
    (when (interactive-p)
      (message nil))
    (if (abort-key-p c)
        (error 'editor-abort)
        c)))

(defun prompt-for-y-or-n-p (prompt)
  (do () (nil)
    (let ((c (prompt-for-character (format nil "~a [y/n]? " prompt))))
      (cond
        ((char= #\y c)
         (return t))
        ((char= #\n c)
         (return nil))))))

(define-key *minibuf-keymap* "C-j" 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* "C-m" 'minibuf-read-line-confirm)
(define-key *minibuf-keymap* "C-i" 'minibuf-read-line-completion)
(define-key *minibuf-keymap* "M-p" 'minibuf-read-line-prev-history)
(define-key *minibuf-keymap* "M-n" 'minibuf-read-line-next-history)
(define-key *minibuf-keymap* "C-g" 'minibuf-read-line-break)

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
   (copy-point (buffer-start-point (minibuffer))
               :temporary)
   *minibuffer-start-charpos*))

(defun get-minibuffer-string ()
  (points-to-string (minibuffer-start-point)
                    (buffer-end-point (minibuffer))))

(defun minibuffer-clear-input ()
  (delete-between-points (minibuffer-start-point)
                         (buffer-end-point (minibuffer))))

(define-command minibuf-read-line-confirm () ()
  (let ((str (get-minibuffer-string)))
    (when (or (string= str "")
              (null *minibuf-read-line-existing-p*)
              (funcall *minibuf-read-line-existing-p* str))
      (throw 'minibuf-read-line-end t)))
  t)

(define-command minibuf-read-line-completion () ()
  (when (and *minibuf-read-line-comp-f*
             *minibuffer-completion-function*)
    (with-point ((start (minibuffer-start-point)))
      (funcall *minibuffer-completion-function*
               *minibuf-read-line-comp-f*
               start))))

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

(defun minibuf-read-line-loop (comp-f existing-p syntax-table)
  (let ((*minibuf-read-line-existing-p* existing-p)
        (*minibuf-read-line-comp-f* comp-f))
    (with-current-syntax syntax-table
      (catch 'minibuf-read-line-end
        (command-loop))
      (let ((str (get-minibuffer-string)))
        (lem.history:add-history *minibuf-read-line-history* str)
        str))))

(defun prompt-for-line (prompt initial comp-f existing-p history-name
                        &optional (syntax-table (current-syntax)))
  (when (and (not *enable-recursive-minibuffers*) (< 0 *minibuf-read-line-depth*))
    (editor-error "ERROR: recursive use of minibuffer"))
  (let ((*minibuffer-calls-window* (current-window))
        (*minibuf-read-line-history* (let ((table (gethash history-name *minibuf-read-line-history-table*)))
                                       (or table
                                           (setf (gethash history-name *minibuf-read-line-history-table*)
                                                 (lem.history:make-history)))))
        (*current-minibuffer-window* (minibuffer-window)))
    (let ((result
            (catch +recursive-minibuffer-break-tag+
              (handler-case
                  (with-current-window (minibuffer-window)
                    (let ((minibuf-buffer-prev-string
                            (points-to-string (buffer-start-point (minibuffer))
                                              (buffer-end-point (minibuffer))))
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
                        (unwind-protect (minibuf-read-line-loop comp-f existing-p syntax-table)
                          (if (deleted-window-p (minibuffer-calls-window))
                              (setf (current-window) (car (window-list)))
                              (setf (current-window) (minibuffer-calls-window)))
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
                           (when *minibuffer-file-complete-function*
                             (lambda (str)
                               (funcall *minibuffer-file-complete-function*
                                        str directory)))
                           (and existing #'probe-file)
                           'mh-read-file)))
    (if (string= result "")
        default
        result)))
