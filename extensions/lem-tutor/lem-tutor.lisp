(defpackage :lem-tutor
  (:use :cl :lem)
  (:export #:tutorial))

(in-package :lem-tutor)
(defparameter *tutorial-text* (merge-pathnames "original.txt" (asdf:system-source-directory :lem-tutor)))
(defparameter *save-file* (merge-pathnames "lem-tutor-saves/lem-tutor-save.txt" (lem-home)))
(defparameter *progress-file* (merge-pathnames "lem-tutor-saves/lem-tutor-progress.lisp" (lem-home)))
(defparameter *tutorial-buffer* nil
  "Active tutorial buffer,set when tutorial mode is enabled")

(define-command tutorial () ()
  "Open the Lem interactive tutorial"
  (tutorial-mode t))

(defun tutorial-save-progress (&optional buffer)
  "Create or update a save file with the cursor position, for easy continuation of the tutorial"
  (let* ((buffer *tutorial-buffer*)
        (point (buffer-point buffer))
        (line (line-number-at-point point))
        (column (point-column point)))
    (with-open-file (stream *progress-file*
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create)
    (format stream "(:line ~D :column ~D)" line column))))

(defun tutorial-load-progress ()
  "Load cursor position from *progress-file* and restore it in *tutorial-buffer*."
  (when (probe-file *progress-file*)
    (with-open-file (stream *progress-file*
      :direction :input)
    (let* ((plist (read stream))
           (line (getf plist :line))
           (column (getf plist :column))
           (point (buffer-point *tutorial-buffer*)))
      (move-to-line point line)
      (move-to-column point column)))))
             
  
(defun tutorial-enable ()
  "Enable tutorial mode: ensure save directory exists, initialize working copy if needed,
  open the save file, store the buffer and hook into after-save for progress tracking."
  (ensure-directories-exist *save-file*)
  (unless (probe-file *save-file*)
    (uiop:copy-file *tutorial-text* *save-file*))
  (let ((buffer (find-file-buffer *save-file*)))
    (setf *tutorial-buffer* buffer)
    (tutorial-load-progress)
    (add-hook (variable-value 'after-save-hook :buffer buffer)
            #'tutorial-save-progress)
    (switch-to-buffer buffer)))

(defun tutorial-disable ()
    "Save progress when tutorial mode is disabled."
  (tutorial-save-progress)))

(define-minor-mode tutorial-mode
    (:name "Lem-tutor"
     :description "A tutorial for the lem editor."
     :enable-hook  #'tutorial-enable
     :disable-hook #'tutorial-disable))