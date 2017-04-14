(defpackage :lem.language-mode
  (:use :cl :lem :lem.sourcelist)
  (:export
   :*language-mode-keymap*
   :line-comment
   :insertion-line-comment
   :find-definitions-function
   :find-references-function
   :language-mode
   :indent
   :newline-and-indent
   :indent-region
   :make-xref-location
   :make-xref-references))
(in-package :lem.language-mode)

(define-editor-variable line-comment nil)
(define-editor-variable insertion-line-comment nil)
(define-editor-variable find-definitions-function nil)
(define-editor-variable find-references-function nil)

(defun prompt-for-symbol (prompt history-name)
  (prompt-for-line prompt "" nil nil history-name))

(define-major-mode language-mode ()
    (:keymap *language-mode-keymap*)
  nil)

(define-key *language-mode-keymap* (kbd "C-i") 'indent)
(define-key *language-mode-keymap* (kbd "C-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "M-j") 'newline-and-indent)
(define-key *language-mode-keymap* (kbd "C-M-\\") 'indent-region)
(define-key *language-mode-keymap* (kbd "M-;") 'comment-or-uncomment-region)
(define-key *language-mode-keymap* (kbd "M-.") 'find-definitions)
(define-key *language-mode-keymap* (kbd "M-_") 'find-references)
(define-key *language-mode-keymap* (kbd "M-?") 'find-references)
(define-key *language-mode-keymap* (kbd "M-,") 'pop-definition-stack)

(define-command indent (&optional (n 1)) ("p")
  (if (variable-value 'calc-indent-function)
      (indent-line (current-point))
      (self-insert n)))

(define-command newline-and-indent (n) ("p")
  (newline n)
  (indent))

(define-command indent-region (start end) ("r")
  (save-excursion
    (apply-region-lines start end 'indent-line)))

(defun space*-p (point)
  (with-point ((point point))
    (skip-whitespace-forward point t)
    (end-line-p point)))

(define-command comment-or-uncomment-region (arg) ("P")
  (if arg
      (uncomment-region)
      (comment-region)))

(define-command comment-region () ()
  (let ((line-comment (or (variable-value 'insertion-line-comment :buffer)
                          (variable-value 'line-comment :buffer))))
    (when line-comment
      (save-excursion
        (with-point ((start (current-point) :right-inserting)
                     (end (current-point) :left-inserting))
          (cond
            ((buffer-mark-p (current-buffer))
             (move-point start (region-beginning))
             (move-point end (region-end)))
            (t
             (line-start start)
             (line-end end)))
          (skip-whitespace-forward start)
          (let ((charpos (point-charpos start)))
            (loop
              (when (same-line-p start end)
                (cond ((space*-p start))
                      (t
                       (insert-string start line-comment)
                       (unless (space*-p end)
                         (insert-character end #\newline))))
                (return))
              (unless (space*-p start)
                (insert-string start line-comment))
              (line-offset start 1 charpos))))))))

(define-command uncomment-region () ()
  (let ((line-comment (variable-value 'line-comment :buffer))
        (insertion-line-comment (variable-value 'insertion-line-comment :buffer)))
    (when line-comment
      (with-point ((start (current-point) :right-inserting)
                   (end (current-point) :right-inserting))
        (cond
          ((buffer-mark-p (current-buffer))
           (move-point start (region-beginning))
           (move-point end (region-end)))
          (t
           (line-start start)
           (line-end end)))
        (character-offset start -1)
        (loop
          (unless (search-comment-start-backward end start)
            (return))
          (when (looking-at end line-comment)
            (let ((res (looking-at end insertion-line-comment)))
              (if res
                  (delete-character end (length res))
                  (loop :while (looking-at end line-comment)
                    :do (delete-character end (length line-comment)))))))))))

(define-attribute xref-headline-attribute
  (t :bold-p t))

(define-attribute xref-title-attribute
  (:dark :foreground "cyan" :bold-p t)
  (:light :foreground "blue" :bold-p t))

(defstruct xref-location
  (file nil :read-only t :type (or string pathname))
  (position 1 :read-only t :type integer)
  (title "" :read-only t :type string))

(defstruct xref-references
  (type nil :read-only t)
  (locations nil :read-only t))

(defun go-to-location (location)
  (find-file (xref-location-file location))
  (move-to-position (current-point) (xref-location-position location)))

(define-command find-definitions () ()
  (alexandria:when-let (fn (variable-value 'find-definitions-function :buffer))
    (let ((locations (funcall fn)))
      (unless locations
        (editor-error "No definitions found"))
      (push-location-stack (current-point))
      (if (null (rest locations))
          (go-to-location (first locations))
          (let ((prev-file nil))
            (with-sourcelist (sourcelist "*definitions*")
              (dolist (location locations)
                (let ((file (xref-location-file location))
                      (title (xref-location-title location)))
                  (append-sourcelist sourcelist
                                     (lambda (p)
                                       (unless (equal prev-file file)
                                         (insert-string p file :attribute 'xref-headline-attribute)
                                         (insert-character p #\newline))
                                       (insert-string p (format nil "  ~A" title)
                                                      :attribute 'xref-title-attribute))
                                     (let ((location location))
                                       (lambda ()
                                         (go-to-location location))))
                  (setf prev-file file)))))))))

(define-command find-references () ()
  (alexandria:when-let (fn (variable-value 'find-references-function :buffer))
    (let ((refs (funcall fn)))
      (unless refs
        (editor-error "No references found"))
      (push-location-stack (current-point))
      (with-sourcelist (sourcelist "*references*")
        (dolist (ref refs)
          (let ((type (xref-references-type ref)))
            (append-sourcelist sourcelist
                               (lambda (p)
                                 (insert-string p (princ-to-string type)
                                                :attribute 'xref-headline-attribute))
                               nil)
            (dolist (location (xref-references-locations ref))
              (let ((title (xref-location-title location)))
                (append-sourcelist sourcelist
                                   (lambda (p)
                                     (insert-string p (format nil "  ~A" title)
                                                    :attribute 'xref-title-attribute))
                                   (let ((location location))
                                     (lambda ()
                                       (go-to-location location))))))))))))

(defvar *xref-stack-table* (make-hash-table :test 'equal))

(defun push-location-stack (point)
  (let ((buffer (point-buffer point)))
    (push (list (buffer-name buffer)
                (line-number-at-point point)
                (point-charpos point))
          (gethash (buffer-major-mode buffer) *xref-stack-table*))))

(define-command pop-definition-stack () ()
  (let ((elt (pop (gethash (buffer-major-mode (current-buffer))
                           *xref-stack-table*))))
    (when elt
      (destructuring-bind (buffer-name line-number charpos) elt
        (select-buffer buffer-name)
        (move-to-line (current-point) line-number)
        (line-offset (current-point) 0 charpos)))))
