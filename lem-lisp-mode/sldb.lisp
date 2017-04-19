(in-package :lem-lisp-mode)

(define-attribute topline-attribute)

(define-attribute condition-attribute
  (t :foreground "red" :bold-p t))

(define-attribute section-attribute
  (t :background "gray" :foreground "black"))

(define-attribute restart-number-attribute
  (t :bold-p t))

(define-attribute restart-type-attribute
  (t :foreground "purple"))

(define-attribute restart-attribute)

(define-attribute frame-label-attribute
  (t :foreground "gray40"))

(define-major-mode sldb-mode ()
    (:name "sldb"
     :keymap *sldb-keymap*))

(define-key *sldb-keymap* "C-m" 'sldb-default-action)
(define-key *sldb-keymap* "M-n" 'sldb-details-down)
(define-key *sldb-keymap* "M-p" 'sldb-details-up)
(define-key *sldb-keymap* "q" 'sldb-quit)
(define-key *sldb-keymap* "c" 'sldb-continue)
(define-key *sldb-keymap* "a" 'sldb-abort)
(define-key *sldb-keymap* "r" 'sldb-restart-frame)
(define-key *sldb-keymap* "0" 'sldb-invoke-restart-0)
(define-key *sldb-keymap* "1" 'sldb-invoke-restart-1)
(define-key *sldb-keymap* "2" 'sldb-invoke-restart-2)
(define-key *sldb-keymap* "3" 'sldb-invoke-restart-3)
(define-key *sldb-keymap* "4" 'sldb-invoke-restart-4)
(define-key *sldb-keymap* "5" 'sldb-invoke-restart-5)
(define-key *sldb-keymap* "6" 'sldb-invoke-restart-6)
(define-key *sldb-keymap* "7" 'sldb-invoke-restart-7)
(define-key *sldb-keymap* "8" 'sldb-invoke-restart-8)
(define-key *sldb-keymap* "9" 'sldb-invoke-restart-9)

(defun get-sldb-buffer (thread)
  (dolist (buffer (buffer-list))
    (when (eql thread (buffer-value buffer 'thread))
      (return buffer))))

(defun get-sldb-buffer-create (thread)
  (or (get-sldb-buffer thread)
      (get-buffer-create (format nil "*sldb ~D*" thread))))

(defun sldb-frame.number (frame) (first frame))
(defun sldb-frame.string (frame) (second frame))
(defun sldb-frame.plist (frame) (third frame))

(defun prune-initial-frames (frames)
  (or (loop :for frame :in frames
            :for (n form) := frame
            :until (ppcre:scan (load-time-value
                                (ppcre:create-scanner
                                 "^(?:[() ]|lambda)*swank\\b"
                                 :case-insensitive-mode t))
                               form)
            :collect frame)
      frames))

(defun sldb-setup (thread level condition restarts frames conts)
  (let ((buffer (get-sldb-buffer-create thread)))
    (setf (current-window) (display-buffer buffer))
    (change-buffer-mode buffer 'sldb-mode)
    (setf (buffer-read-only-p buffer) nil)
    (setf (variable-value 'truncate-lines :buffer buffer) nil)
    (setf (buffer-value buffer 'thread)
          thread
          (buffer-value buffer 'level)
          level
          (buffer-value buffer 'condition)
          condition
          (buffer-value buffer 'restarts)
          restarts
          (buffer-value buffer 'continuations)
          conts)
    (erase-buffer buffer)
    (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
              (lambda (buffer)
                (declare (ignore buffer))
                (sldb-quit)))
    (let ((point (buffer-point buffer)))
      (destructuring-bind (message type extras) condition
        (declare (ignore extras))
        (insert-string point message :attribute 'topline-attribute)
        (insert-character point #\newline)
        (insert-string point type :attribute 'condition-attribute)
        (insert-character point #\newline))
      (insert-string point (format nil "~%Restarts:~%") :attribute 'section-attribute)
      (loop :for n :from 0
            :for (title description) :in restarts
            :do
            (insert-string point " ")
            (insert-string point (format nil "~D" n) :attribute 'restart-number-attribute)
            (insert-string point ": [")
            (insert-string point title :attribute 'restart-type-attribute)
            (insert-string point "] ")
            (insert-string point description :attribute 'restart-attribute)
            (insert-character point #\newline))
      (insert-string point (format nil "~%Backtrace:~%") :attribute 'section-attribute)
      (setf (buffer-value buffer 'backtrace-start-point)
            (copy-point point :right-inserting))
      (save-excursion
        (sldb-insert-frames point (prune-initial-frames frames) t)))
    (setf (buffer-read-only-p buffer) t)))

(defun sldb-insert-frames (point frames more)
  (dolist (frame frames)
    (sldb-insert-frame point frame))
  (when more
    (insert-string point " --more--"
                   'default-action #'sldb-fetch-all-frames
                   :attribute 'section-attribute)))

(defun sldb-insert-frame (point frame)
  (let ((number (sldb-frame.number frame))
        (string (sldb-frame.string frame)))
    (with-point ((s point))
      (insert-string point " ")
      (insert-string point (format nil "~2d:" number) :attribute 'frame-label-attribute)
      (insert-string point " ")
      (insert-string point string)
      (put-text-property s point 'frame frame)
      (put-text-property s point 'default-action 'sldb-toggle-details)
      (insert-character point #\newline))))

(defun sldb-fetch-all-frames ()
  (let ((*inhibit-read-only* t)
        (p (current-point)))
    (buffer-start p)
    (next-single-property-change p 'frame)
    (delete-between-points p (buffer-end-point (current-buffer)))
    (save-excursion
      (sldb-insert-frames p (lisp-eval '(swank:backtrace 0 nil)) nil))))

(defun sldb-toggle-details (&optional on)
  (let ((*inhibit-read-only* t))
    (if (or on (not (sldb-frame-details-visible-p (current-point))))
        (sldb-show-frame-details)
        (sldb-hide-frame-details))))

(defun sldb-show-frame-details ()
  )

(defun sldb-hide-frame-details ()
  )

(defun sldb-frame-details-visible-p (point)
  (and (text-property-at point 'frame)
       (text-property-at point 'detail-visible-p)))

(defun sldb-active (thread level select)
  (let ((buffer (get-sldb-buffer thread)))
    (cond ((and buffer
                (= level (buffer-value buffer 'level -1)))
           (when select (pop-to-buffer buffer)))
          (t
           (sldb-reinitialize thread level)))))

(defun sldb-reinitialize (thread level)
  (lisp-rex
   '(swank:debugger-info-for-emacs 0 10)
   :continuation (lambda (value)
                   (alexandria:destructuring-ecase value
                     ((:ok result)
                      (apply #'sldb-setup thread level result))))
   :thread thread))

(defun sldb-exit (thread level stepping)
  (declare (ignore level))
  (let ((buffer (get-sldb-buffer thread)))
    (when buffer
      (cond (stepping
             (error "stepping"))
            ((eq buffer (window-buffer (current-window)))
             (quit-window (current-window) t)
             (let* ((repl-buffer (repl-buffer))
                    (repl-window (when repl-buffer
                                   (first (get-buffer-windows repl-buffer)))))
               (when repl-window
                 (setf (current-window) repl-window))))
            (t
             (kill-buffer buffer))))))

(define-command sldb-default-action () ()
  (let ((fn (text-property-at (current-point) 'default-action)))
    (when fn (funcall fn))))

(define-command sldb-details-down () ()
  (let ((p (current-point)))
    (next-single-property-change p 'frame)
    (when (end-line-p p)
      (next-single-property-change p 'frame))))

(define-command sldb-details-up () ()
  (let ((p (current-point))
        (sp (buffer-value (current-buffer)
                          'backtrace-start-point)))
    (cond ((point< p sp)
           (move-point p sp))
          (t
           (previous-single-property-change p 'frame)
           (when (end-line-p p)
             (previous-single-property-change p 'frame))))))

(define-command sldb-quit () ()
  (lisp-rex `(swank:throw-to-toplevel)
            :continuation (lambda (value)
                            (alexandria:destructuring-ecase
                                value
                              ((:ok x) (editor-error "sldb-quit returned [%s]" x))
                              ((:abort _) (declare (ignore _)))))))

(define-command sldb-continue () ()
  (when (null (buffer-value (current-buffer) 'restarts))
    (error "continue called outside of debug buffer"))
  (lisp-rex '(swank:sldb-continue)
            :continuation (lambda (value)
                            (alexandria:destructuring-case value
                              ((:ok x)
                               (editor-error "sldb-quit returned [~A]" x))))))

(define-command sldb-abort () ()
  (lisp-eval-async '(swank:sldb-abort)
                   (lambda (v)
                     (message "Restart returned: ~A" v))))

(define-command sldb-restart-frame () ()
  )

(defun sldb-invoke-restart (n)
  (check-type n integer)
  (lisp-rex `(swank:invoke-nth-restart-for-emacs
              ,(buffer-value (current-buffer) 'level -1)
              ,n)
            :continuation (lambda (x)
                            (alexandria:destructuring-ecase x
                              ((:ok value) (message "Restart returned: %s" value))
                              ((:abort _) (declare (ignore _)))))))

(define-command sldb-invoke-restart-0 () () (sldb-invoke-restart 0))
(define-command sldb-invoke-restart-1 () () (sldb-invoke-restart 1))
(define-command sldb-invoke-restart-2 () () (sldb-invoke-restart 2))
(define-command sldb-invoke-restart-3 () () (sldb-invoke-restart 3))
(define-command sldb-invoke-restart-4 () () (sldb-invoke-restart 4))
(define-command sldb-invoke-restart-5 () () (sldb-invoke-restart 5))
(define-command sldb-invoke-restart-6 () () (sldb-invoke-restart 6))
(define-command sldb-invoke-restart-7 () () (sldb-invoke-restart 7))
(define-command sldb-invoke-restart-8 () () (sldb-invoke-restart 8))
(define-command sldb-invoke-restart-9 () () (sldb-invoke-restart 9))

(pushnew (lambda (event)
           (alexandria:destructuring-case event
             ((:debug-activate thread level &optional select)
              (sldb-active thread level select)
              t)
             ((:debug thread level condition restarts frames conts)
              (sldb-setup thread level condition restarts frames conts)
              t)
             ((:debug-return thread level stepping)
              (sldb-exit thread level stepping)
              t)))
         *event-hooks*)
