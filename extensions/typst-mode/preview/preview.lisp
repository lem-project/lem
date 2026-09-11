(uiop:define-package :lem-typst-mode/preview/preview
  (:use :cl :lem)
  (:export :typst-preview
   :typst-preview-stop
           :typst-set-preview-root
   :typst-export-file))
(in-package :lem-typst-mode/preview/preview)

;;; Variables
(defvar *preview-sessions* (make-hash-table :test 'equal))
(defvar *pdf-root* "")
(defvar *preview-default-url* "http://127.0.0.1:23625")

;;; Structures
(defstruct typst-preview-session
  process
  url
  filepath)

;;; Internal Functions
(defun stop-preview-process (filepath)
  "Stop the active preview process for FILEPATH if it exists."
  (let ((session (gethash filepath *preview-sessions*)))
    (when session
      (let ((proc (typst-preview-session-process session)))
        (when (and proc (uiop:process-alive-p proc))
          (ignore-errors (uiop:terminate-process proc :urgent t))))
      (remhash filepath *preview-sessions*))))

(defun cleanup-preview-processes ()
  "Kill active tinymist process launched by Lem before closing."
  (maphash (lambda (filepath session)
             (declare (ignore filepath))
             (let ((process (typst-preview-session-process session)))
               (when (and process (uiop:process-alive-p process))
                 (ignore-errors (uiop:terminate-process process :urgent t)))))
           *preview-sessions*)
  (clrhash *preview-sessions*))

;;; Commands
(define-command typst-set-preview-root () ()
  "Set Typst preview root to a directory for included files."
  (let ((dir (prompt-for-directory "Typst preview root: "
                                   :directory (buffer-directory))))
    (when dir
      (setf *pdf-root* (namestring dir))
      (message "Typst root directory set to: ~A" *pdf-root*))))

(define-command typst-export-file (output-pdf)
  ((prompt-for-string "PDF Name: "
                      :initial-value (concatenate 'string (namestring (buffer-directory)) "out.pdf")))
  "Export current Typst file to PDF at OUTPUT-PDF path."
  (let ((input (buffer-filename (current-buffer))))
    (if input
        (let ((cmd (append (list "typst" "compile" (namestring input))
                           (when (and *pdf-root* (not (string= *pdf-root* "")))
                             (list "--root" *pdf-root*))
                           (list output-pdf))))
          (uiop:run-program cmd)
          (message "Exported successfully to ~A" output-pdf))
        (editor-error "Current buffer is not associated with a saved file."))))

(define-command typst-preview () ()
  "Preview the current Typst buffer in the browser.
If the preview server is already running, reopens the URL in the browser without restarting.
Otherwise, starts a new tinymist preview server."
  (let ((file (buffer-filename (current-buffer))))
    (if file
        (let* ((filepath (namestring file))
               (session (gethash filepath *preview-sessions*)))
          (cond
            ;; Server is already running: reopen the browser at the existing URL without restarting
            ((and session
                  (typst-preview-session-process session)
                  (uiop:process-alive-p (typst-preview-session-process session)))
             (let ((url (or (typst-preview-session-url session) *preview-default-url*)))
               (lem:open-external-file url)
               (message "Reopened Typst preview at ~A" url)))
            ;; Server is not running: start a new tinymist preview instance
            (t
             (stop-preview-process filepath)
             (let* ((cmd (append (list "tinymist" "preview")
                                 (when (and *pdf-root* (not (string= *pdf-root* "")))
                                   (list "--root" *pdf-root*))
                                 (list "--open" filepath)))
                    (proc (uiop:launch-program cmd))
                    (url *preview-default-url*))
               (setf (gethash filepath *preview-sessions*)
                     (make-typst-preview-session :process proc
                                                 :url url
                                                 :filepath filepath))
               (message "Started Typst preview for ~A" (file-namestring filepath))))))
        (editor-error "Current buffer is not associated with a saved file."))))

(define-command typst-preview-stop () ()
  "Stop the Typst preview server for the current buffer."
  (let ((file (buffer-filename (current-buffer))))
    (if file
        (let ((filepath (namestring file)))
          (if (gethash filepath *preview-sessions*)
              (progn
                (stop-preview-process filepath)
                (message "Typst preview server stopped."))
              (message "No active preview server for this file: ~A" filepath)))
        (editor-error "Current buffer is not associated with a file."))))

;;; Hooks
(add-hook *exit-editor-hook* 'cleanup-preview-processes)

#+sbcl
(pushnew 'cleanup-preview-processes sb-ext:*exit-hooks*)
