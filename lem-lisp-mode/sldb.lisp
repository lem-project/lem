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

(define-attribute local-name-attribute)

(define-attribute local-value-attribute)

(define-attribute catch-tag-attribute)

(define-major-mode sldb-mode ()
    (:name "sldb"
     :keymap *sldb-keymap*))

(define-key *sldb-keymap* "C-m" 'sldb-default-action)
(define-key *sldb-keymap* "M-n" 'sldb-details-down)
(define-key *sldb-keymap* "M-p" 'sldb-details-up)
(define-key *sldb-keymap* "C-i" 'sldb-forward-button)
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

(defun frame-number (frame) (first frame))
(defun frame-string (frame) (second frame))
(defun frame-plist (frame) (third frame))

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
      (sldb-insert-condition point condition)
      (insert-string point (format nil "~%Restarts:~%") :attribute 'section-attribute)
      (sldb-insert-restarts point restarts)
      (insert-string point (format nil "~%Backtrace:~%") :attribute 'section-attribute)
      (setf (buffer-value buffer 'backtrace-start-point)
            (copy-point point :right-inserting))
      (save-excursion
        (sldb-insert-frames point (prune-initial-frames frames) t)))
    (setf (buffer-read-only-p buffer) t)))

(defun sldb-insert-condition (point condition)
  (destructuring-bind (message type extras) condition
    (declare (ignore extras))
    (insert-string point message :attribute 'topline-attribute)
    (insert-character point #\newline)
    (insert-string point type :attribute 'condition-attribute)
    (insert-character point #\newline)))

(defun sldb-insert-restarts (point restarts)
  (loop :for n :from 0
        :for (title description) :in restarts
        :do
        (insert-string point " ")
        (insert-string point (format nil "~D: " n) :attribute 'restart-number-attribute)
        (insert-button point (format nil "[~A] " title)
                       (let ((n n)) (lambda () (sldb-invoke-restart n)))
                       :attribute 'restart-type-attribute)
        (insert-string point description :attribute 'restart-attribute)
        (insert-character point #\newline)))

(defun sldb-insert-frames (point frames more)
  (dolist (frame frames)
    (sldb-insert-frame point frame))
  (when more
    (insert-button point " --more--"
                   #'sldb-fetch-all-frames
                   :attribute 'section-attribute
                   :button-tag 'sldb-more-frames)))

(defun sldb-insert-frame (point frame)
  (let ((number (frame-number frame))
        (string (frame-string frame)))
    (insert-string point " ")
    (insert-string point (format nil "~2d:" number) :attribute 'frame-label-attribute)
    (insert-string point " ")
    (insert-button point string #'sldb-toggle-details 'frame frame :button-tag 'sldb-frame)
    (insert-character point #\newline)))

(defun sldb-fetch-all-frames ()
  (let ((*inhibit-read-only* t)
        (p (current-point)))
    (move-point p (buffer-value (current-buffer) 'backtrace-start-point))
    (delete-between-points p (buffer-end-point (current-buffer)))
    (save-excursion
      (sldb-insert-frames p (lisp-eval '(swank:backtrace 0 nil)) nil))))

(defun sldb-toggle-details ()
  (let* ((point (current-point))
         (frame-button (button-at point)))
    (when (and frame-button (button-get frame-button 'frame))
      (save-excursion
        (let ((*inhibit-read-only* t))
          (if (button-get frame-button 'toggle)
              (sldb-hide-frame-details point frame-button)
              (sldb-show-frame-details point frame-button)))))))

(defun sldb-show-frame-details (point frame-button)
  (destructuring-bind (locals catches)
      (lisp-eval `(swank:frame-locals-and-catch-tags
                   ,(frame-number
                     (button-get frame-button 'frame))))
    (setf (button-get frame-button 'toggle) t)
    (move-point point (button-end frame-button))
    (let ((indent1 (make-string 7 :initial-element #\space))
          (indent2 (make-string 9 :initial-element #\space)))
      (insert-character point #\newline)
      (insert-string point indent1)
      (insert-string point (if locals "Locals:" "[No Locals]")
                     :attribute 'section-attribute)
      (loop :for var :in locals
            :do (destructuring-bind (&key name id value) var
                  (insert-character point #\newline)
                  (insert-string point indent2)
                  (insert-button point (format nil "~A~A" name (if (zerop id) "" (format nil "#~D" id)))
                                 'sldb-inspect-var
                                 :attribute 'local-name-attribute)
                  (insert-string point " = ")
                  (insert-string point value :attribute 'local-value-attribute)))
      (when catches
        (insert-character point #\newline)
        (insert-string point indent1)
        (insert-string point "Catch-tags:" :attribute 'section-attribute)
        (dolist (tag catches)
          (insert-character point #\newline)
          (insert-string point indent2)
          (insert-string point tag :attribute 'catch-tag-attribute))))))

(defun sldb-hide-frame-details (point frame-button)
  (setf (button-get frame-button 'toggle) nil)
  (move-point point (button-end frame-button))
  (with-point ((start point))
    (character-offset start 1)
    (sldb-down point)
    (line-start point)
    (delete-between-points start point)))

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
  (let ((fn (text-property-at (current-point) 'action)))
    (when fn (funcall fn))))

(defun sldb-down (p)
  (next-single-property-change p 'sldb-frame)
  (when (end-line-p p)
    (or (next-single-property-change p 'sldb-frame)
        (next-single-property-change p 'sldb-more-frames))))

(defun sldb-up (p)
  (let ((sp (buffer-value (current-buffer)
                          'backtrace-start-point)))
    (cond ((point< p sp)
           (move-point p sp))
          (t
           (previous-single-property-change p 'sldb-frame)
           (when (end-line-p p)
             (previous-single-property-change p 'sldb-frame))))))

(define-command sldb-details-down () ()
  (sldb-down (current-point)))

(define-command sldb-details-up () ()
  (sldb-up (current-point)))

(define-command sldb-forward-button () ()
  (let ((p (current-point)))
    (or (forward-button p)
        (progn
          (buffer-start p)
          (forward-button p)))))

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

(defun frame-number-at-point (point)
  (let ((frame (text-property-at point 'sldb-frame)))
    (when frame
      (frame-number frame))))

(define-command sldb-restart-frame (frame-number)
    ((list (frame-number-at-point (current-point))))
  (when frame-number
    (lisp-rex `(swank:restart-frame ,frame-number)
              :continuation (lambda (v)
                              (alexandria:destructuring-ecase v
                                ((:ok value) (message "~A" value))
                                ((:abort _) (declare (ignore _))))))))

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

(defun sldb-inspect-var ()
  )

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
