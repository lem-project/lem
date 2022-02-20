(defpackage :lem.search-replace
  (:use :cl :lem :alexandria)
  #+sbcl
  (:lock t))
(in-package :lem.search-replace)

(define-condition highlight-matches (after-executing-command) ())

(define-attribute highlight
  (t :foreground "white" :background "dark red"))

(define-attribute active-highlight
  (t :foreground "black" :background "cyan"))

(define-key *global-keymap* "C-s" 'search-string)
(define-key *global-keymap* "C-r" 'search-string)
(define-key *global-keymap* "C-M-s" 'search-regexp)
(define-key *global-keymap* "C-M-r" 'search-regexp)
(define-key *global-keymap* "M-s _" 'search-symbol)
(define-key *global-keymap* "M-s ." 'search-symbol-at-point)

(defvar *prompt-keymap* (make-keymap :name 'search-prompt))
(define-key *prompt-keymap* "C-s" 'search-next-matched)
(define-key *prompt-keymap* "C-r" 'search-previous-matched)
(define-key *prompt-keymap* "F2" 'search-replace)
(define-key *prompt-keymap* 'next-page 'search-replace-next-page)
(define-key *prompt-keymap* 'previous-page 'search-replace-previous-page)
(define-key *prompt-keymap* 'scroll-down 'search-replace-scroll-down)
(define-key *prompt-keymap* 'scroll-up 'search-replace-scroll-up)

(define-minor-mode search-prompt-mode
    (:name "Search"
     :keymap *prompt-keymap*
     :global t))

(defun compute-window-region (window)
  (let ((start (window-view-point window)))
    (list start
          (line-offset (copy-point start :temporary)
                       (window-height window)))))

(defmacro with-window-region ((start-point end-point window) &body body)
  `(destructuring-bind (,start-point ,end-point)
       (compute-window-region ,window)
     ,@body))

(defun call-with-buffer-check (function)
  (let* ((before-buffer (current-buffer))
         (before-points (copy-list (lem-base::buffer-points before-buffer))))
    (unwind-protect (funcall function)
      (unless (eq before-buffer (current-buffer))
        (log:warn "unexpected current-buffer" before-buffer (current-buffer)))
      (let ((after-points (lem-base::buffer-points (current-buffer))))
        (unless (length= before-points after-points)
          (log:warn "leak points"
                    (set-difference before-points
                                    after-points)))))))

(defmacro with-buffer-check (() &body body)
  `(call-with-buffer-check (lambda () ,@body)))

(defvar *context* nil)

(defstruct context
  search-forward
  search-backward
  prompt-window
  target-window
  cursor
  last-matched
  saved-point)

(defstruct matched
  (start (required-argument :start) :type point)
  (end (required-argument :end) :type point))

(defun make-current-context (search-forward search-backward)
  (let* ((prompt-window (active-prompt-window))
         (target-window (lem::caller-of-prompt-window prompt-window))
         (cursor (buffer-point (window-buffer target-window))))
    (make-context :search-forward search-forward
                  :search-backward search-backward
                  :prompt-window prompt-window
                  :target-window target-window
                  :cursor cursor
                  :saved-point (copy-point cursor))))

(defun delete-context (context)
  (delete-point (context-saved-point context))
  (setf (context-saved-point context) nil))

(defun clear-context ()
  (when *context*
    (delete-context *context*)))

(defun restore-cursor ()
  (when *context*
    (let ((point (context-saved-point *context*)))
      (move-point (buffer-point (point-buffer point)) point))))

(defun get-or-make-context (search-forward search-backward)
  (or *context*
      (setf *context* (make-current-context search-forward search-backward))))

(defun context-search-string (context)
  (get-prompt-input-string (context-prompt-window context)))

(defun highlight-overlays (buffer)
  (buffer-value buffer 'highlight-overlays))

(defun (setf highlight-overlays) (overlays buffer)
  (setf (buffer-value buffer 'highlight-overlays) overlays))

(defun create-matched-overlay (context matched)
  (let ((overlay
          (make-overlay (matched-start matched)
                        (matched-end matched)
                        (if (point<= (matched-start matched)
                                     (context-cursor context)
                                     (matched-end matched))
                            'active-highlight
                            'highlight)))
        (buffer (point-buffer (context-cursor context))))
    (push overlay (highlight-overlays buffer))
    overlay))

(defun clear-all-highlight (buffer)
  (mapc #'delete-overlay (highlight-overlays buffer))
  (setf (highlight-overlays buffer) '()))

(defun search-next-match (context point &key limit (forward t) (move t))
  (multiple-value-bind (search-forward search-backward)
      (if forward
          (values (context-search-forward context)
                  (context-search-backward context))
          (values (context-search-backward context)
                  (context-search-forward context)))
    (when-let* ((matched-end
                 (funcall search-forward
                          (copy-point point :temporary)
                          (context-search-string context)
                          limit))
                (matched-start
                 (funcall search-backward
                          (copy-point matched-end :temporary)
                          (context-search-string context))))
      (assert (length= 3 (remove-duplicates (list matched-start matched-end point))))
      (let ((matched (if (point< matched-start matched-end)
                         (make-matched :start matched-start :end matched-end)
                         (make-matched :end matched-start :start matched-end))))
        (setf (context-last-matched context) matched)
        (when move (move-point point matched-end))
        matched))))

(defun update-highlight (context)
  (clear-all-highlight (window-buffer (context-target-window context)))
  (unless (string= "" (context-search-string context))
    (with-window-region (start-point end-point (context-target-window context))
      (with-point ((point start-point))
        (loop :for matched := (search-next-match context point :limit end-point)
              :while matched
              :do (create-matched-overlay context matched))))))

(defun adjust-current-matched (context)
  (when-let ((matched (context-last-matched context)))
    (when (point<= (matched-start matched)
                   (context-cursor context)
                   (matched-end matched))
      (move-point (context-cursor context)
                  (matched-start matched)))))

(defun move-to-forward-matched (context)
  (let ((cursor (context-cursor context)))
    (when-let ((matched (search-next-match context cursor :forward t :move nil)))
      (when (point<= (matched-start matched) cursor)
        (move-point cursor (matched-end matched)))
      (search-next-match context cursor :forward t :move t))))

(defun move-to-backward-matched (context)
  (adjust-current-matched context)
  (search-next-match context (context-cursor context)
                     :forward nil :move t))

(defun move-matched-and-update-highlight (context &key (forward t))
  (when (if forward
            (move-to-forward-matched context)
            (move-to-backward-matched context))
    (adjust-current-matched context)
    (update-highlight context)
    (window-see (context-target-window context))))

(define-command search-next-matched () ()
  (when *context*
    (move-matched-and-update-highlight *context* :forward t)))

(define-command search-previous-matched () ()
  (when *context*
    (move-matched-and-update-highlight *context* :forward nil)))

(defun wrap (fn arg)
  (when *context*
    (with-current-window (context-target-window *context*)
      (funcall fn arg))))

(define-command search-replace-next-page (n) ("P")
  (wrap #'next-page n))

(define-command search-replace-previous-page (n) ("P")
  (wrap #'previous-page n))

(define-command search-replace-scroll-down (n) ("p")
  (wrap #'scroll-down n))

(define-command search-replace-scroll-up (n) ("p")
  (wrap #'scroll-up n))

(defstruct searcher
  prompt
  search-forward
  search-backward
  initial-value)

(defun string-searcher ()
  (make-searcher :prompt "Search: "
                 :search-forward #'search-forward
                 :search-backward #'search-backward))

(defun regexp-searcher ()
  (make-searcher :prompt "Search(regexp): "
                 :search-forward #'search-forward-regexp
                 :search-backward #'search-backward-regexp))

(defun symbol-searcher (&key initial-value)
  (make-searcher :prompt "Search(symbol): "
                 :search-forward #'search-forward-symbol
                 :search-backward #'search-backward-symbol
                 :initial-value initial-value))

(defun prompt-for-search (searcher)
  (flet ((update ()
           (update-highlight
            (get-or-make-context (searcher-search-forward searcher)
                                 (searcher-search-backward searcher)))))
    (when (searcher-initial-value searcher)
      ;; dirty hack:
      ;; If there is an initial value, update it at the beginning of the first command loop.
      (send-event #'update))
    (let ((*context* nil))
      (search-prompt-mode t)
      (unwind-protect
           (handler-bind ((highlight-matches
                            (lambda (c)
                              (declare (ignore c))
                              (update)))
                          (editor-abort
                            (lambda (c)
                              (declare (ignore c))
                              (restore-cursor))))
             (prompt-for-string (searcher-prompt searcher)
                                :initial-value (searcher-initial-value searcher)
                                :gravity :topright))
        (clear-all-highlight (current-buffer))
        (search-prompt-mode nil)
        (clear-context)))))

(define-command search-string () ()
  (prompt-for-search (string-searcher)))

(define-command search-regexp () ()
  (prompt-for-search (regexp-searcher)))

(define-command search-symbol () ()
  (prompt-for-search (symbol-searcher)))

(define-command search-symbol-at-point () ()
  (prompt-for-search (symbol-searcher :initial-value (symbol-string-at-point (current-point)))))

(define-command search-replace () ()
  (log:info "search-replace"))
