(defpackage :lem-tutor
  (:use :cl :lem)
  (:export #:tutorial :tutorial-rescan))

(in-package :lem-tutor)
 
(defun tutorial-text ()
  "Set correct paths to core and save files"
  (merge-pathnames "tutorial-basics.txt" (asdf:system-source-directory :lem-tutor)))

(defun tutorial-save-file ()
  (merge-pathnames "lem-tutor-saves/lem-tutor-save.txt" (lem-home)))

(defun tutorial-progress ()
  (merge-pathnames "lem-tutor-saves/lem-tutor-progress.lisp" (lem-home)))

(define-command tutorial () ()
  "Learn Lem interactively with guided exercises and progress tracking."
  (tutorial-mode t))

(defun tutorial-save-progress (buffer)
  "Create or update a save file with the cursor position, for easy continuation of the tutorial"
  (let* ((point (buffer-point buffer))
        (line (line-number-at-point point))
        (column (point-column point)))
    (with-open-file (stream (tutorial-progress)
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

(defun tutorial-enable ()
  "Enable tutorial mode: ensure save directory exists, initialize working copy if needed,
  open the save file, store the buffer and hook into after-save for progress tracking."
  (ensure-directories-exist (tutorial-save-file))
  (unless (probe-file  (tutorial-save-file))
    (uiop:copy-file (tutorial-text) (tutorial-save-file)))
  (let ((buffer (find-file-buffer (tutorial-save-file))))
    (switch-to-buffer buffer)
    (tutorial-load-progress)
    (add-hook (variable-value 'after-save-hook :buffer buffer)
            #'tutorial-save-progress)))

(defun tutorial-disable ()
    "Save progress when tutorial mode is disabled."
  (tutorial-save-progress (find-file-buffer (tutorial-save-file))))

(define-command tutorial-rescan () ()
  "Force a syntax rescan of the tutorial buffer"
  (let ((buffer (find-file-buffer (tutorial-save-file))))
    (lem-tutor/syntax-parser:scan-region 
     (buffer-start-point buffer)
     (buffer-end-point buffer))))

(define-minor-mode tutorial-mode
    (:name "Lem-tutor"
     :description "A tutorial for the lem editor."
     :enable-hook  #'tutorial-enable
     :disable-hook #'tutorial-disable))
