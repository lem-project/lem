(defpackage :lem-skk-mode/dictionary
  (:use :cl)
  (:export :*skk-dictionary-paths*
           :*skk-user-dictionary-path*
           :load-skk-dictionary
           :lookup-candidates
           :dictionary-loaded-p))
(in-package :lem-skk-mode/dictionary)

(defvar *skk-dictionary-paths*
  '("~/.skk/SKK-JISYO.L"
    "/usr/share/skk/SKK-JISYO.L"
    "/usr/share/skk/SKK-JISYO.M"
    "/usr/local/share/skk/SKK-JISYO.L"
    "/usr/local/share/skk/SKK-JISYO.M")
  "List of paths to search for SKK dictionary files.
User dictionary (~/.skk/SKK-JISYO.L) is searched first.")

(defvar *skk-user-dictionary-path*
  "~/.skk/user-jisyo"
  "Path to user dictionary file.")

(defvar *skk-dictionary* nil
  "Hash table mapping reading (hiragana) to list of candidates.")

(defvar *dictionary-loaded-p* nil
  "Non-nil if dictionary has been loaded.")

(defun dictionary-loaded-p ()
  "Return non-nil if dictionary is loaded."
  *dictionary-loaded-p*)

(defun expand-path (path)
  "Expand ~ in PATH to home directory."
  (if (and (plusp (length path))
           (char= (char path 0) #\~))
      (merge-pathnames (subseq path 2)
                       (user-homedir-pathname))
      (pathname path)))

(defun find-dictionary-file ()
  "Find the first existing dictionary file from *skk-dictionary-paths*."
  (dolist (path *skk-dictionary-paths*)
    (let ((expanded (expand-path path)))
      (when (probe-file expanded)
        (return expanded)))))

(defun parse-dictionary-line (line)
  "Parse a single SKK dictionary line.
Returns (reading . candidates) or NIL for invalid/comment lines.
SKK-JISYO format: reading /candidate1/candidate2;annotation/.../"
  ;; Skip empty lines and comments (starting with ;;)
  (when (and (plusp (length line))
             (not (and (>= (length line) 2)
                       (char= (char line 0) #\;)
                       (char= (char line 1) #\;))))
    (let ((space-pos (position #\Space line)))
      (when space-pos
        (let ((reading (subseq line 0 space-pos))
              (rest (subseq line (1+ space-pos)))
              (candidates '()))
          ;; Parse /candidate1/candidate2;annotation/.../
          (when (and (plusp (length rest))
                     (char= (char rest 0) #\/))
            (let ((start 1))
              (loop :for end := (position #\/ rest :start start)
                    :while end
                    :do (let* ((entry (subseq rest start end))
                               ;; Remove annotation (after ;)
                               (semicolon-pos (position #\; entry))
                               (candidate (if semicolon-pos
                                              (subseq entry 0 semicolon-pos)
                                              entry)))
                          (when (plusp (length candidate))
                            (push candidate candidates))
                          (setf start (1+ end))))))
          (when candidates
            (cons reading (nreverse candidates))))))))

(defun load-dictionary-file (path table)
  "Load dictionary from PATH into TABLE."
  (when (probe-file path)
    (with-open-file (stream path
                            :direction :input
                            :external-format :utf-8
                            :if-does-not-exist nil)
      (when stream
        (loop :for line := (read-line stream nil nil)
              :while line
              :for entry := (parse-dictionary-line line)
              :when entry
                :do (let ((existing (gethash (car entry) table)))
                      (setf (gethash (car entry) table)
                            (if existing
                                (append existing (cdr entry))
                                (cdr entry)))))
        t))))

(defun load-skk-dictionary (&optional force)
  "Load SKK dictionary files. If FORCE is non-nil, reload even if already loaded."
  (when (or force (not *dictionary-loaded-p*))
    ;; Initialize or clear the dictionary
    (if *skk-dictionary*
        (clrhash *skk-dictionary*)
        (setf *skk-dictionary* (make-hash-table :test 'equal)))
    ;; Load main dictionary
    (let ((dict-path (find-dictionary-file))
          (loaded nil))
      (when dict-path
        (setf loaded (load-dictionary-file dict-path *skk-dictionary*)))
      ;; Load user dictionary (higher priority - loaded after main)
      (let ((user-path (expand-path *skk-user-dictionary-path*)))
        (when (load-dictionary-file user-path *skk-dictionary*)
          (setf loaded t)))
      ;; Only mark as loaded if at least one dictionary was found
      (setf *dictionary-loaded-p* loaded)
      (unless loaded
        (warn "SKK: No dictionary found. Searched paths: ~{~A~^, ~}"
              *skk-dictionary-paths*)))))

(defun lookup-candidates (reading)
  "Look up conversion candidates for READING.
Returns a list of candidate strings, or NIL if not found."
  (unless *dictionary-loaded-p*
    (load-skk-dictionary))
  (gethash reading *skk-dictionary*))
