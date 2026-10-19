;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise/journal
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :journal
           :open-journal
           :close-journal
           :*journal*
           :*journal-path*
           :*journal-max-bytes*
           :*journal-author*
           :*journal-tx-id*
           :with-journal-author
           :journal-append
           :journal-query
           :journal-tail
           :journal-since
           :journal-count
           :line-diff
           :record-file-edit
           :record-terminal-run
           :record-test-result
           :record-git-event
           :record-agent-event
           :record-eval
           :watch-directory
           :stop-watching
           :le-journal))
(in-package :yeditor-enterprise/journal)

;;;; yeditor-journal (plan T20): the append-only, observable memory of the workspace.
;;;; One JSON object per line; rotated by size. An in-memory index answers queries by
;;;; path, author, type and time. Agents read it before acting (T21/T22); humans browse
;;;; it in the Change Browser (T24).
;;;; Event types: file-edit, fs-event, terminal-run, test-result, git-event,
;;;; agent-event, eval-event. Every event has "type", "seq", "ts" (universal time),
;;;; "author" (human | agent:<session> | component:<name>) and an optional "tx_id".

(defparameter *journal-path* (uiop:xdg-data-home "yeditor/journal.jsonl"))
(defparameter *journal-max-bytes* (* 8 1024 1024) "Rotate the file beyond this size.")
(defparameter *journal-keep* 3 "Rotated files kept (journal.jsonl.1 ... .N).")
(defparameter *full-text-limit* 4096 "file-edit events carry the whole text below this size.")
(defparameter *output-limit* 4000 "terminal-run events keep this much output inline.")

(defvar *journal-author* "human")
(defvar *journal-tx-id* nil)

(defmacro with-journal-author ((author &key tx-id) &body body)
  `(let ((*journal-author* ,author)
         (*journal-tx-id* ,tx-id))
     ,@body))

(defclass journal ()
  ((path :initarg :path :reader journal-path)
   (stream :initform nil :accessor journal-stream)
   (events :initform (make-array 1024 :adjustable t :fill-pointer 0) :reader journal-events)
   (by-path :initform (make-hash-table :test (function equal)) :reader journal-by-path)
   (seq :initform 0 :accessor journal-seq)
   (lock :initform (bt2:make-lock :name "yeditor-journal") :reader journal-lock)))

(defvar *journal* nil "The open journal, or nil.")

(defun open-journal (&optional (path *journal-path*))
  "Open (append to) the journal at PATH and make it current."
  (ensure-directories-exist path)
  (let ((journal (make-instance (quote journal) :path path)))
    (setf (journal-stream journal)
          (open path :direction :output :if-exists :append :if-does-not-exist :create
                     :external-format :utf-8))
    (setf *journal* journal)))

(defun close-journal (&optional (journal *journal*))
  (when journal
    (bt2:with-lock-held ((journal-lock journal))
      (when (journal-stream journal) (close (journal-stream journal)) (setf (journal-stream journal) nil)))
    (when (eq journal *journal*) (setf *journal* nil))))

(defun rotate (journal)
  (let ((path (journal-path journal)))
    (close (journal-stream journal))
    (loop :for i :from (1- *journal-keep*) :downto 1
          :for from := (format nil "~A.~D" (namestring path) i)
          :when (probe-file from) :do (rename-file from (format nil "~A.~D" (namestring path) (1+ i))))
    (rename-file path (format nil "~A.1" (namestring path)))
    (setf (journal-stream journal)
          (open path :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf-8))))

(defun event-table (type fields)
  (let ((table (make-hash-table :test (function equal))))
    (loop :for (k v) :on fields :by (function cddr)
          :do (setf (gethash (string-downcase (substitute #\_ #\- (string k))) table) v))
    (setf (gethash "type" table) type
          (gethash "ts" table) (get-universal-time)
          (gethash "author" table) (or (gethash "author" table) *journal-author*))
    (when (and *journal-tx-id* (not (gethash "tx_id" table)))
      (setf (gethash "tx_id" table) *journal-tx-id*))
    table))

(defun journal-append (type &rest fields)
  "Append an event of TYPE with FIELDS (keyword plist) to the current journal; returns it
   (a hash table). No-op returning nil when no journal is open."
  (let ((journal *journal*))
    (when journal
      (let ((event (event-table type fields)))
        (bt2:with-lock-held ((journal-lock journal))
          (setf (gethash "seq" event) (incf (journal-seq journal)))
          (let ((stream (journal-stream journal)))
            (yason:encode event stream)
            (terpri stream)
            (finish-output stream)
            (when (> (file-length stream) *journal-max-bytes*) (rotate journal)))
          (vector-push-extend event (journal-events journal))
          (alexandria:when-let ((path (gethash "path" event)))
            (push event (gethash path (journal-by-path journal)))))
        event))))

;;; Queries (newest first)

(defun journal-count (&optional (journal *journal*))
  (if journal (fill-pointer (journal-events journal)) 0))

(defun journal-query (&key type path author since (limit 100) (journal *journal*))
  "Events matching every given criterion, newest first, at most LIMIT."
  (when journal
    (bt2:with-lock-held ((journal-lock journal))
      (let ((candidates (if path
                            (gethash path (journal-by-path journal))
                            (loop :with events := (journal-events journal)
                                  :for i :from (1- (fill-pointer events)) :downto 0
                                  :collect (aref events i))))
            (out nil) (n 0))
        (dolist (event candidates (nreverse out))
          (when (>= n limit) (return (nreverse out)))
          (when (and (or (null type) (equal (gethash "type" event) type))
                     (or (null author) (equal (gethash "author" event) author))
                     (or (null since) (>= (gethash "ts" event) since)))
            (push event out)
            (incf n)))))))

(defun journal-tail (&optional (n 20)) (journal-query :limit n))
(defun journal-since (time &key (limit 1000)) (journal-query :since time :limit limit))

;;; Diffs

(defun line-diff (old new)
  "One hunk covering the changed lines between OLD and NEW text: plist
   :start (1-based line) :removed (lines) :added (lines); nil when identical."
  (let* ((a (coerce (uiop:split-string old :separator (string #\Newline)) (quote vector)))
         (b (coerce (uiop:split-string new :separator (string #\Newline)) (quote vector)))
         (prefix (or (mismatch a b :test (function string=)) (min (length a) (length b))))
         (suffix 0))
    (unless (equalp a b)
      (loop :while (and (< suffix (- (length a) prefix)) (< suffix (- (length b) prefix))
                        (string= (aref a (- (length a) suffix 1)) (aref b (- (length b) suffix 1))))
            :do (incf suffix))
      (list :start (1+ prefix)
            :removed (coerce (subseq a prefix (- (length a) suffix)) (quote list))
            :added (coerce (subseq b prefix (- (length b) suffix)) (quote list))))))

;;; Recorders

(defvar *saved-text* (make-hash-table :test (function equal)) "path -> text at the last save or open.")

(defun full-buffer-text (buffer)
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun remember-file (buffer)
  (alexandria:when-let ((file (buffer-filename buffer)))
    (setf (gethash (namestring file) *saved-text*) (full-buffer-text buffer))))

(defun record-file-edit (buffer)
  "Journal the change of BUFFER's file since it was opened or last saved."
  (alexandria:when-let ((file (buffer-filename buffer)))
    (let* ((path (namestring file))
           (old (gethash path *saved-text* ""))
           (new (full-buffer-text buffer))
           (diff (line-diff old new)))
      (setf (gethash path *saved-text*) new)
      (when diff
        (apply (function journal-append) "file-edit"
               :path path
               :hunk (alexandria:plist-hash-table
                      (list "start" (getf diff :start) "removed" (getf diff :removed) "added" (getf diff :added))
                      :test (function equal))
               (when (< (length new) *full-text-limit*) (list :text new)))))))

(defun record-terminal-run (command directory exit-code output)
  "Journal a command run; long output is kept inline truncated, in full in a side file."
  (let ((log nil))
    (when (> (length output) *output-limit*)
      (setf log (merge-pathnames (format nil "logs/run-~D-~D.log" (get-universal-time) (random 100000))
                                 (uiop:pathname-directory-pathname (if *journal* (journal-path *journal*) *journal-path*))))
      (ensure-directories-exist log)
      (with-open-file (out log :direction :output :if-exists :supersede) (write-string output out)))
    (journal-append "terminal-run" :command command :cwd (namestring directory) :exit-code exit-code
                                   :output (subseq output 0 (min (length output) *output-limit*))
                                   :log (and log (namestring log)))))

(defun record-test-result (record)
  "Journal a test/build run (record plist :target :exit :failures :output)."
  (journal-append "test-result" :target (getf record :target) :exit-code (getf record :exit)
                                :failures (mapcar (lambda (f) (format nil "~A:~D" (car f) (cdr f)))
                                                  (getf record :failures))))

(defun record-eval (record)
  "Journal an evaluation (plan amendment 4: eval-event). RECORD plist: :code :language
   and optionally :context :stdout :stderr :value :traceback :duration."
  (apply (function journal-append) "eval-event"
         (loop :for key :in (list :code :language :context :stdout :stderr :value :traceback :duration)
               :for value := (getf record key)
               :when value :append (list key value))))

(defun git-output (root &rest args)
  (multiple-value-bind (out err code)
      (uiop:run-program (list* "git" "-C" (namestring root) args) :output :string :error-output :string
                                                                  :ignore-error-status t)
    (declare (ignore err))
    (and (zerop code) (string-trim '(#\Newline) out))))

(defun record-git-event (root operation)
  "Journal the repository state after OPERATION (branch, status, diff stat)."
  (journal-append "git-event" :operation operation :path (namestring root)
                              :branch (git-output root "rev-parse" "--abbrev-ref" "HEAD")
                              :status (git-output root "status" "--short")
                              :diff-stat (git-output root "diff" "--stat")))

(defun record-agent-event (session task &key plan tool-calls observation)
  "Journal an agent step (the session replay)."
  (journal-append "agent-event" :session session :task task :plan plan
                                :tool-calls tool-calls :observation observation
                                :author (format nil "agent:~A" session)))

;;; External file changes (fs-event)

(defvar *watcher* nil)

(defun file-mtimes (directory)
  (let ((table (make-hash-table :test (function equal))))
    (uiop:collect-sub*directories
     directory (constantly t)
     (lambda (dir) (not (member (car (last (pathname-directory dir))) (list ".git" ".venv" "node_modules")
                                :test (function equal))))
     (lambda (dir)
       (dolist (file (uiop:directory-files dir))
         (setf (gethash (namestring file) table) (ignore-errors (file-write-date file))))))
    table))

(defun watch-directory (directory &key (interval 2))
  "Poll DIRECTORY for files created, changed or deleted outside the editor."
  (stop-watching)
  (let ((before (file-mtimes directory)))
    (setf *watcher*
          (bt2:make-thread
           (lambda ()
             (loop
               (sleep interval)
               (let ((now (file-mtimes directory)))
                 (maphash (lambda (path time)
                            (let ((old (gethash path before :absent)))
                              (cond ((eq old :absent)
                                     (journal-append "fs-event" :path path :change "created" :author "external"))
                                    ((not (eql old time))
                                     (journal-append "fs-event" :path path :change "modified" :author "external")))))
                          now)
                 (maphash (lambda (path time)
                            (declare (ignore time))
                            (unless (nth-value 1 (gethash path now))
                              (journal-append "fs-event" :path path :change "deleted" :author "external")))
                          before)
                 (setf before now))))
           :name "yeditor-journal-watcher"))))

(defun stop-watching ()
  (when (and *watcher* (bt2:thread-alive-p *watcher*))
    (bt2:destroy-thread *watcher*))
  (setf *watcher* nil))

;;; Component

(defun install-listener (comp variable function)
  (yeditor-tx:install-effect comp :hook
                             (lambda () (push function (symbol-value variable)))
                             :undo-thunk (lambda () (setf (symbol-value variable)
                                                          (remove function (symbol-value variable))))))

(define-component le-journal
  :version "0.1.0"
  :install
  (lambda (comp ctx)
    (declare (ignore ctx))
    (list (yeditor-tx:install-effect comp :file-persist
                                     (lambda () (open-journal *journal-path*))
                                     :undo-thunk (lambda () (stop-watching) (close-journal)))
          (install-hook comp (quote *find-file-hook*) (quote remember-file))
          (yeditor-tx:install-effect comp :hook
                                     (lambda () (add-hook (variable-value (quote after-save-hook) :global)
                                                          (quote record-file-edit)))
                                     :undo-thunk (lambda () (remove-hook (variable-value (quote after-save-hook) :global)
                                                                         (quote record-file-edit))))
          (install-listener comp (quote yeditor-components/lang-python:*eval-listeners*) (quote record-eval))
          (install-listener comp (quote yeditor-components/lang-python:*test-listeners*) (quote record-test-result)))))
