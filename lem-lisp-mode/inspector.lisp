(in-package :lem-lisp-mode)

(define-attribute inspector-label-attribute
  )

(define-attribute inspector-value-attribute
  )

(define-attribute inspector-action-attribute
  )

(defvar *inspector-limit* 500)
(defvar *inspector-mark-stack* '())

(define-major-mode lisp-inspector-mode lisp-ui-mode
    (:name "lisp-inspector"
     :keymap *lisp-inspector-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-command lisp-inspect (string)
    ((list (or (symbol-string-at-point (current-point))
               (prompt-for-sexp "Inspect value (evaluated): "))))
  (lisp-eval-async `(swank:init-inspector ,string) 'open-inspector))

(defun inspector-buffer ()
  (or (get-buffer "*lisp-inspector*")
      (let ((buffer (make-buffer "*lisp-inspector*" :enable-undo-p nil)))
        (setf *inspector-mark-stack* '())
        (change-buffer-mode buffer 'lisp-inspector-mode)
        buffer)))

(defun open-inspector (inspected-parts &optional inspector-position hook)
  (let* ((buffer (inspector-buffer))
         (point (buffer-point buffer)))
    (with-current-window (display-buffer buffer)
      (when hook
        (add-hook (variable-value 'kill-buffer-hook :buffer buffer) hook))
      (let ((*inhibit-read-only* t))
        (erase-buffer buffer)
        (destructuring-bind (&key id title content) inspected-parts
          (insert-button point title
                         (make-inspect-action :part id)
                         :attribute 'inspector-value-attribute)
          (delete-between-points point (buffer-end-point buffer))
          (insert-string point
                         (format nil "~%--------------------~%")
                         :attribute 'inspector-label-attribute)
          (save-excursion
            (inspector-insert-content content))
          (when inspector-position
            (move-to-line point (car inspector-position))
            (line-offset point 0 (cdr inspector-position))))))))

(defun inspector-insert-content (content)
  (inspector-fetch-chunk
   content nil
   (lambda (chunk)
     (let ((*inhibit-read-only* t))
       (inspector-insert-chunk chunk t t)))))

(defun inspector-insert-chunk (chunk prev next)
  (destructuring-bind (ispecs len start end) chunk
    (when (and prev (> start 0))
      (inspector-insert-more-button start t))
    (mapc 'inspector-insert-ispec ispecs)
    (when (and next (< end len))
      (inspector-insert-more-button end nil))))

(defun inspector-insert-ispec (ispec)
  (if (stringp ispec)
      (insert-string (current-point) ispec)
      (alexandria:destructuring-ecase ispec
        ((:value string id)
         (insert-button (current-point) string
                        (make-inspect-action :part id)))
        ((:label string)
         (insert-string (current-point) string :attribute 'inspector-label-attribute))
        ((:action string id)
         (insert-button (current-point) string
                        (make-inspect-action :action id)
                        :attribute 'inspector-action-attribute)))))

(defun inspector-position (point)
  (cons (line-number-at-point point)
        (point-charpos point)))

(defun inspector-opener (parts)
  (when parts
    (open-inspector parts (inspector-position (current-point)))))

(defun inspector-new-opener (parts)
  (when parts
    (open-inspector parts)))

(defun make-inspect-action (type value)
  (lambda ()
    (ecase type
      ((:part)
       (lisp-eval-async `(swank:inspect-nth-part ,value)
                        'inspector-new-opener))
      ((:range)
       (inspector-fetch-more value))
      ((:action)
       (lisp-eval-async `(swank::inspector-call-nth-action ,value)
                        'inspector-opener)))))

(define-command lisp-inspector-operate-on-point () ()
  (let ((button (button-at (current-point))))
    (button-action button)))

(define-command lisp-inspector-pop () ()
  )

(define-command lisp-inspector-next () ()
  )

(define-command lisp-inspector-quit () ()
  )

;; slime-find-inspectable-object
;; slime-inspector-next-inspectable-object
;; slime-inspector-previous-inspectable-object

(define-command lisp-inspector-describe () ()
  )

(define-command lisp-inspector-pprint () ()
  )

(define-command lisp-inspector-eval () ()
  )

(define-command lisp-inspector-history () ()
  )

(define-command lisp-inspector-show-source () ()
  )

(define-command lisp-inspector-reinspect () ()
  )

(define-command lisp-inspector-toggle-verbose () ()
  )

(defun inspector-insert-more-button (index previous)
  (insert-button (current-point)
                 (format nil (if previous " [--more--]~%" " [--more--]"))
                 (make-inspect-action :range (cons index previous))
                 :attribute 'inspector-action-attribute))

(define-command lisp-inspector-fetch-all () ()
  (let ((button (button-at (buffer-end (current-point)))))
    (when button
      (let ((*inspector-limit*))
        (button-action button)))))

(defun inspector-fetch-more (index-previous-pair)
  (destructuring-bind (index . prev) index-previous-pair
    (inspector-fetch-chunk
     (list '() (1+ index) index index) prev
     (alexandria:rcurry
      (lambda (chunk prev)
        (let ((*inhibit-read-only* t))
          (let ((button (button-at (current-point))))
            (delete-between-points (button-start button) (button-end button))
            (inspector-insert-chunk chunk prev (not prev)))))
      prev))))

(defun inspector-fetch-chunk (chunk prev cont)
  (inspector-fetch chunk *inspector-limit* prev cont))

(defun inspector-fetch (chunk limit prev cont)
  (destructuring-bind (from to)
      (inspector-next-range chunk limit prev)
    (if (and from to)
        (lisp-eval-async `(swank:inspector-range ,from ,to)
                         (alexandria:rcurry (lambda (chunk2 chunk1 limit prev cont)
                                              (inspector-fetch
                                               (inspector-join-chunks chunk1 chunk2)
                                               limit prev cont))
                                            chunk limit prev cont))
        (funcall cont chunk))))

(defun inspector-next-range (chunk limit prev)
  (destructuring-bind (_ len start end) chunk
    (declare (ignore _))
    (let ((count (- end start)))
      (cond ((and prev (< 0 start) (or (not limit) (< count limit)))
             (list (if limit (max (- end limit) 0) 0) start))
            ((and (not prev) (< end len) (or (not limit) (< count limit)))
             (list end (if limit (+ start limit) most-positive-fixnum)))
            (t '(nil nil))))))

(defun inspector-join-chunks (chunk1 chunk2)
  (destructuring-bind (i1 _l1 s1 e1) chunk1
    (declare (ignore _l1))
    (destructuring-bind (i2 l2 s2 e2) chunk2
      (cond ((= e1 s2)
             (list (append i1 i2) l2 s1 e2))
            ((= e2 s1)
             (list (append i2 i1) l2 s2 e1))
            (t (error "Invalid chunks"))))))

(pushnew (lambda (event)
           (alexandria:destructuring-case event
             ((:inspect what thread tag)
              (let ((hook (when (and thread tag)
                            (alexandria:curry (lambda (sexp)
                                                (swank-protocol:send-message-string
                                                 *connection*
                                                 sexp))
                                              `(:emacs-return ,thread ,tag nil)))))
                (open-inspector what nil hook)))))
         *event-hooks*)
