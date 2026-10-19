(defpackage :lem-tutor
  (:use :cl :lem)
  (:export #:tutorial
           #:open-tutorial
           #:*tutorial-file*
           #:basics-tutorial-file))

(in-package :lem-tutor)

(defun basics-tutorial-file ()
  "The tutorial shipped with Lem (movement, editing, buffers, windows)."
  (merge-pathnames "tutorial-basics.txt"
                   (asdf:system-source-directory :lem-tutor)))

(defvar *tutorial-file* nil
  "The tutorial opened by the tutorial command; nil means Lem's basics tutorial.")

(defun tutorial-text ()
  (or *tutorial-file* (basics-tutorial-file)))

(defun save-name (suffix)
  "Working copies are kept per tutorial: Lem's keeps its historical name."
  (let ((name (pathname-name (tutorial-text))))
    (if (equal name "tutorial-basics")
        (format nil "lem-tutor-saves/lem-tutor-~A" suffix)
        (format nil "lem-tutor-saves/~A-~A" name suffix))))

(defun tutorial-save-file ()
  (merge-pathnames (save-name "save.txt") (lem-home)))

(defun tutorial-progress ()
  (merge-pathnames (save-name "progress.lisp") (lem-home)))

(define-command tutorial () ()
  "Learn Lem interactively with guided exercises and progress tracking."
  (tutorial-mode t))

(defun open-tutorial (file)
  "Open the tutorial FILE (a working copy keeps edits and the cursor position)."
  (let ((*tutorial-file* file))
    (tutorial-enable)))

(defun tutorial-save-progress (buffer &optional (progress (tutorial-progress)))
  "Create or update a save file with the cursor position, for easy continuation of the tutorial"
  (let* ((point (buffer-point buffer))
        (line (line-number-at-point point))
        (column (point-column point)))
    (with-open-file (stream progress
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)
    (format stream "(:line ~D :column ~D)" line column))))

(defun tutorial-load-progress ()
  "Load cursor position from progress file and restore cursor position."
  (handler-case
      (when (probe-file (tutorial-progress))
        (with-open-file (stream (tutorial-progress)
                                :direction :input)
          (let* ((plist (read stream))
                 (line (getf plist :line))
                 (column (getf plist :column))
                 (point (buffer-point (find-file-buffer (tutorial-save-file)))))
            (move-to-line point line)
            (move-to-column point column))))
    (error (e)
      (declare (ignore e))
      (editor-error "Could not restore latest savepoint. Starting at the top, previously made edits are preserved"))))

(defun copy-tutorial-text (source destination)
  "Copy SOURCE to DESTINATION without its leading header (lines starting with ;;;;
and the blank line after them), e.g. a license notice."
  (let* ((lines (uiop:read-file-lines source))
         (body (member-if-not (lambda (line) (uiop:string-prefix-p ";;;;" line)) lines)))
    (when (and (not (eq body lines)) body (string= (first body) ""))
      (setf body (rest body)))
    (with-open-file (out destination :direction :output :if-exists :supersede)
      (format out "~{~A~%~}" body))))

(defun tutorial-enable ()
  "Enable tutorial mode: ensure save directory exists, initialize working copy if needed,
  open the save file, store the buffer and hook into after-save for progress tracking."
  (ensure-directories-exist (tutorial-save-file))
  (unless (probe-file  (tutorial-save-file))
    (copy-tutorial-text (tutorial-text) (tutorial-save-file)))
  (let ((buffer (find-file-buffer (tutorial-save-file))))
    (switch-to-buffer buffer)
    (tutorial-load-progress)
    ;; the buffer remembers its own progress file (several tutorials can be open)
    (setf (buffer-value buffer 'tutorial-progress) (tutorial-progress))
    (add-hook (variable-value 'after-save-hook :buffer buffer)
            'save-buffer-progress)))

(defun save-buffer-progress (buffer)
  (tutorial-save-progress buffer (or (buffer-value buffer 'tutorial-progress) (tutorial-progress))))

(defun tutorial-disable ()
    "Save progress when tutorial mode is disabled."
  (tutorial-save-progress (find-file-buffer (tutorial-save-file))))

;; (define-command tutorial-rescan () ()
  ;; "Force a syntax rescan of the tutorial buffer"
  ;; (let ((buffer (find-file-buffer (tutorial-save-file))))
    ;; (lem-tutor/syntax-parser:scan-region 
     ;; (buffer-start-point buffer)
     ;; (buffer-end-point buffer))))

(define-minor-mode tutorial-mode
    (:name "Lem-tutor"
     :description "A tutorial for the lem editor."
     :enable-hook  #'tutorial-enable
     :disable-hook #'tutorial-disable))
