(defpackage :lem/peek-legit
  (:use :cl :lem)
  (:export :*peek-legit-keymap*
           :collector-buffer
           :collector-insert
           :filename-attribute
           :get-move-function
           :highlight-matched-line
           :position-attribute
           :show-matched-line
           :with-appending-source
           :with-collecting-sources
           :with-insert
           :peek-legit-next-header
           :peek-legit-next
           :peek-legit-select
           :peek-legit-previous-header
           :peek-legit-previous
           :peek-legit-stage-file
           :peek-legit-unstage-file
           :quit)
  (:documentation "Defines the left window of the legit interface.

   Writes on the window the VCS components that are sent by the :legit package: untracked files, changes, staged changes, latest commits… They are displayed with custom attributes (read-only colors…) and text properties (on this line, the function to call on Enter is this lambda function…).
   Cursor mouvements and keybindings send changes to the right window."))

#|
Notes:

- if names don't conflict, use a :keyword for text properties, not a 'symbol (:commit-hash vs 'commit-hash). Keywords are easier to manipulate from another source file (no home package).

|#

(in-package :lem/peek-legit)


(define-minor-mode peek-legit-mode
    (:name "Peek"
     :keymap *peek-legit-keymap*)
  (setf (not-switchable-buffer-p (current-buffer)) t))

;; Git commands
;; Some are defined on legit.lisp for this keymap too.
(define-key *peek-legit-keymap* "s" 'peek-legit-stage-file)
(define-key *peek-legit-keymap* "u" 'peek-legit-unstage-file)

;; quit
(define-key *peek-legit-keymap* "Return" 'peek-legit-select)

;; navigation
(define-key *peek-legit-keymap* 'next-line 'peek-legit-next)
(define-key *peek-legit-keymap* "n" 'peek-legit-next)
(define-key *peek-legit-keymap* "C-n" 'peek-legit-next)
(define-key *peek-legit-keymap* 'previous-line 'peek-legit-previous)
(define-key *peek-legit-keymap* "p" 'peek-legit-previous)
(define-key *peek-legit-keymap* "C-p" 'peek-legit-previous)
(define-key *peek-legit-keymap* "Tab" 'next-window)


;;;
;;; The two windows pane.
;;;
(define-attribute filename-attribute
  (t :foreground :base0D))

(define-attribute highlight
  (t :background :base0D))

(defvar *collector*)

(defclass collector ()
  ((buffer :initarg :buffer
           :reader collector-buffer)
   (count :initform 0
          :accessor collector-count)))

(defvar *peek-window*)
(defvar *source-window*)
(defvar *parent-window*)

(defclass peek-window (floating-window) ())
(defclass source-window (floating-window) ())

(defmethod lem-core::%delete-window :before ((window peek-window))
  (finalize-peek-legit))

(defmethod lem-core::%delete-window :before ((window source-window))
  (finalize-peek-legit))

(defmethod compute-window-list ((current-window peek-window))
  (list *peek-window* *source-window*))

(defmethod compute-window-list ((current-window source-window))
  (list *source-window* *peek-window*))

(defvar *is-finalzing* nil)

(defun finalize-peek-legit ()
  (unless *is-finalzing*
    (let ((*is-finalzing* t))
      (finalize-highlight-overlays)
      (setf (current-window) *parent-window*)
      (delete-window *source-window*)
      (delete-window *peek-window*))))

(defun set-move-function (start end move-function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end :move-marker t))
  (put-text-property start end :move-function move-function))

(defun set-visit-file-function (start end function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end :file-marker t))
  (put-text-property start end :visit-file-function function))

(defun set-stage-function (start end function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end :stage-marker t))
  (put-text-property start end :stage-function function))

(defun set-unstage-function (start end function)
  (with-point ((end start))
    (character-offset end 1)
    (put-text-property start end :unstage-marker t))
  (put-text-property start end :unstage-function function))

(defun get-move-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point :move-function)))

(defun get-visit-file-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point :visit-file-function)))

(defun get-stage-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point :stage-function)))

(defun get-unstage-function (point)
  (with-point ((point point))
    (line-start point)
    (text-property-at point :unstage-function)))

(defun start-move-point (point)
  (buffer-start point)
  (unless (text-property-at point :move-marker)
    (next-move-point point)))

(defun next-move-point (point)
  "Find the next point (line) with a marker.
  This is how we distinguish between simple text, and meaningful text."
  (when (text-property-at point :move-marker)
    (next-single-property-change point :move-marker))
  (next-single-property-change point :move-marker))

(defun previous-move-point (point)
  (when (text-property-at point :move-marker)
    (previous-single-property-change point :move-marker))
  (previous-single-property-change point :move-marker))

(defun next-header-point (point)
  "Find the next point (line) with a header marker."
  (when (text-property-at point :header-marker)
    (next-single-property-change point :header-marker))
  (next-single-property-change point :header-marker))

(defun previous-header-point (point)
  "Find the previous point (line) with a header marker."
  (when (text-property-at point :header-marker)
    (previous-single-property-change point :header-marker))
  (previous-single-property-change point :header-marker))

(defun make-two-side-by-side-windows (buffer)
  (let* ((x-margin 4)
         (y-margin 2)
         (width (- (floor (display-width) 2) 2 x-margin))
         (height (- (display-height) 2 (* 2 y-margin)))
         (peek-window (make-instance 'peek-window
                                     :buffer buffer
                                     :x (+ 1 x-margin)
                                     :y (+ 1 y-margin)
                                     :width width
                                     :height height
                                     :use-border t))
         (source-window (make-instance 'source-window
                                       :buffer (make-buffer "*source*"
                                                            :temporary t
                                                            :enable-undo-p nil
                                                            :directory (uiop:getcwd))
                                       :x (+ (window-x peek-window) (window-width peek-window) 2)
                                       :y (+ 1 y-margin)
                                       :width width
                                       :height height
                                       :use-border t)))
    (list peek-window source-window)))

(defun display (collector)
  (when (boundp '*peek-window*)
    (delete-window *peek-window*))
  (when (boundp '*source-window*)
    (delete-window *source-window*))

  (destructuring-bind (peek-window source-window)
      (make-two-side-by-side-windows (collector-buffer collector))

    (unless (boundp '*parent-window*)
      (setf *parent-window* (current-window)))

    (setf *peek-window* peek-window)
    (setf *source-window* source-window)

    (setf (current-window) peek-window)
    (peek-legit-mode t)

    (start-move-point (buffer-point (collector-buffer collector)))
    (show-matched-line)))

(defun make-peek-legit-buffer ()
  (let ((buffer (make-buffer "*peek-legit*"
                             :temporary t
                             :enable-undo-p t
                             :directory (uiop:getcwd))))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    buffer))

(defun call-with-collecting-sources (function &key read-only)
  (let* ((*collector* (make-instance 'collector :buffer (make-peek-legit-buffer)))
         (point (buffer-point (collector-buffer *collector*))))
    (declare (ignorable point))
    (funcall function *collector*)
    (when read-only
      (setf (buffer-read-only-p (collector-buffer *collector*)) t))
      (display *collector*)))

(defmacro with-collecting-sources ((collector &key (read-only t)) &body body)
  `(call-with-collecting-sources (lambda (,collector)
                                   (declare (ignorable ,collector))
                                   ,@body)
                                 :read-only ,read-only))

(defun call-with-appending-source (insert-function
                                   move-function
                                   visit-file-function
                                   stage-function
                                   unstage-function)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (funcall insert-function point)
      (unless (start-line-p point)
        (insert-string point (string #\newline) :read-only t))
      (set-move-function start point move-function)
      (set-visit-file-function start point visit-file-function)
      (set-stage-function start point stage-function)
      (set-unstage-function start point unstage-function))
    (incf (collector-count *collector*))))

(defmacro with-appending-source ((point &key move-function
                                             visit-file-function
                                             stage-function
                                             unstage-function) &body body)
  `(call-with-appending-source (lambda (,point) ,@body)
                               ,move-function
                               ,visit-file-function
                               ,stage-function
                               ,unstage-function))

(defun collector-insert (s &key (newline t) header)
  (let ((point (buffer-point (collector-buffer *collector*))))
    (with-point ((start point))
      (character-offset start 1)
      (insert-string point s :read-only t)
      (when header
        (put-text-property start point :header-marker t))
      (when newline
        (insert-string point (string #\newline) :read-only t)))))

;;;
(define-attribute match-line-attribute
  (t :background :base02))

(defun get-matched-point ()
  (alexandria:when-let* ((move (get-move-function (buffer-point (window-buffer *peek-window*))))
                         (point (funcall move)))
    point))

(defun get-matched-file ()
  (alexandria:when-let* ((visit-file-function (get-visit-file-function
                                               (buffer-point (window-buffer *peek-window*))))
                         (file (funcall visit-file-function)))
    file))

(defun show-matched-line ()
  (alexandria:when-let (point (get-matched-point))
    (let* ((point (copy-point point :temporary))
           (buffer (point-buffer point)))
      (with-current-window *source-window*
        (switch-to-buffer buffer nil nil)
        (update-highlight-overlay point)
        (move-point (buffer-point buffer) point)
        (window-see (current-window))))))

(defmethod execute :after ((mode peek-legit-mode) command argument)
  (when (eq (current-window) *peek-window*)
    (show-matched-line)))

(defun highlight-matched-line (point)
  (let ((overlay (make-overlay-line point 'highlight)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :name "highlight-matched-line")
                 300)))


(define-command peek-legit-select () ()
  (alexandria:when-let ((file (get-matched-file)))
    (quit)
    (alexandria:if-let
        ((buffer (or (and (uiop:file-exists-p file)
                          (find-file-buffer file))
                     (find-file-buffer
                      (merge-pathnames
                       (lem-core/commands/project:find-root (buffer-filename))
                       file)))))
      (switch-to-buffer buffer)
      (editor-error "File ~a doesn't exist." file))))

(define-command peek-legit-next () ()
  (next-move-point (current-point)))

(define-command peek-legit-next-header () ()
  (next-header-point (current-point)))

(define-command peek-legit-previous-header () ()
  (previous-header-point (current-point)))

(define-command peek-legit-previous () ()
  (previous-move-point (current-point)))

(define-command peek-legit-stage-file () ()
  (alexandria:when-let* ((stage (get-stage-function (buffer-point (window-buffer *peek-window*))))
                         (point (funcall stage)))
    ;; Update the buffer, to see that a staged file goes to the staged section.
    ;; This calls git again and refreshes everything.
    (uiop:symbol-call :lem/legit :legit-status)
    point))

(define-command peek-legit-unstage-file () ()
  (alexandria:when-let* ((unstage (get-unstage-function (buffer-point (window-buffer *peek-window*))))
                         (point (funcall unstage)))
    ;; Update the buffer, to see that a staged file goes to the staged section.
    ;; This calls git again and refreshes everything.
    (uiop:symbol-call :lem/legit :legit-status)
    point))

(defun quit ()
  "Delete the two side windows."
  (setf (current-window) *parent-window*)
  (start-timer
   (make-idle-timer (lambda ()
                      (delete-window *peek-window*)
                      (delete-window *source-window*)))
   0))



;;;
(defvar *highlight-overlays* '())

(defun set-highlight-overlay (point)
  (let ((overlay (make-overlay-line point (ensure-attribute 'match-line-attribute))))
    (push overlay *highlight-overlays*)
    (setf (buffer-value (point-buffer point) 'highlight-overlay) overlay)))

(defun get-highlight-overlay (point)
  (buffer-value (point-buffer point) 'highlight-overlay))

(defun update-highlight-overlay (point)
  (let ((overlay (get-highlight-overlay point)))
    (cond (overlay
           (move-point (overlay-start overlay) point)
           (move-point (overlay-end overlay) point))
          (t
           (set-highlight-overlay point)))))

(defun finalize-highlight-overlays ()
  (dolist (overlay *highlight-overlays*)
    (buffer-unbound (overlay-buffer overlay) 'highlight-overlay)
    (delete-overlay overlay))
  (setf *highlight-overlays* '()))
