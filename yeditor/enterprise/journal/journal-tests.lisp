;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-journal-tests
  (:use :cl :rove :yeditor-context :yeditor-enterprise/journal))
(in-package :yeditor-enterprise-journal-tests)

(defun temp-dir ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "yeditor-journal-~D/" (random 1000000)) (uiop:default-temporary-directory))))

(defun edit-and-save (path text)
  (let ((buffer (lem:find-file-buffer path)))
    (lem:switch-to-buffer buffer)
    (lem:insert-string (lem:buffer-end-point buffer) text)
    (lem:save-current-buffer)
    buffer))

(defun now-us ()
  (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ (* s 1000000) us)))

(deftest test-journal-records-workspace-activity
  (lem-fake-interface:with-fake-interface ()
   (lem/common/timer:with-timer-manager (make-instance 'lem/common/timer:timer-manager)
    (let* ((dir (temp-dir))
           (*journal-path* (merge-pathnames "journal.jsonl" dir)))
      (ensure-directories-exist dir)
      (dolist (name (list "a.py" "b.py" "c.py"))
        (with-open-file (o (merge-pathnames name dir) :direction :output) (format o "x = 1~%")))
      (install-component 'le-journal)
      (unwind-protect
           (let ((a (namestring (merge-pathnames "a.py" dir))))
             (edit-and-save (merge-pathnames "a.py" dir) (format nil "y = 2~%"))
             (edit-and-save (merge-pathnames "b.py" dir) (format nil "z = 3~%"))
             (with-journal-author ("agent:s1" :tx-id "tx-9")
               (edit-and-save (merge-pathnames "c.py" dir) (format nil "w = 4~%")))
             (record-terminal-run "make test" dir 0 "all good")
             (dolist (listener yeditor-components/lang-python:*eval-listeners*)
               (funcall listener (list :code "1 + 1" :language "python" :time 0)))
             (ok (= 3 (length (journal-query :type "file-edit"))) "three file edits")
             (ok (= 2 (length (journal-query :type "file-edit" :author "human"))) "two by the human")
             (let ((agent (journal-query :type "file-edit" :author "agent:s1")))
               (ok (= 1 (length agent)) "one by the agent")
               (ok (equal (gethash "tx_id" (first agent)) "tx-9") "agent edit tied to its transaction"))
             (let ((event (first (journal-query :path a))))
               (ok event "query by path")
               (ok (equal (gethash "added" (gethash "hunk" event)) (list "y = 2")) "hunk holds the added line")
               (ok (search "y = 2" (gethash "text" event)) "small files carry the full text"))
             (ok (= 1 (length (journal-query :type "terminal-run"))) "one terminal run")
             (ok (equal (gethash "code" (first (journal-query :type "eval-event"))) "1 + 1")
                 "evaluations from the Python REPL are journaled")
             (ok (equal (mapcar (lambda (e) (gethash "type" e)) (journal-tail 2)) (list "eval-event" "terminal-run"))
                 "tail returns the newest first")
             (let ((lines (uiop:read-file-lines *journal-path*)))
               (ok (= 5 (length lines)) "every event persisted as one JSON line")
               (ok (every (lambda (l) (hash-table-p (yason:parse l))) lines) "lines are valid JSON")))
        (uninstall-component 'le-journal)
        (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
      (ok (null *journal*) "uninstall closes the journal")
      (ok (null yeditor-components/lang-python:*eval-listeners*) "listeners removed")))))

(deftest test-line-diff
  (ok (null (line-diff "a" "a")) "identical texts")
  (let ((d (line-diff (format nil "a~%b~%c") (format nil "a~%B~%B2~%c"))))
    (ok (= 2 (getf d :start)) "hunk starts at the first changed line")
    (ok (equal (getf d :removed) (list "b")) "removed lines")
    (ok (equal (getf d :added) (list "B" "B2")) "added lines")))

(deftest test-journal-truncation-rotation-and-watch
  (let* ((dir (temp-dir))
         (*journal-max-bytes* 2000))
    (ensure-directories-exist dir)
    (open-journal (merge-pathnames "journal.jsonl" dir))
    (unwind-protect
         (progn
           (let ((event (record-terminal-run "long" dir 1 (make-string 10000 :initial-element #\x))))
             (ok (= 4000 (length (gethash "output" event))) "long output truncated inline")
             (ok (probe-file (gethash "log" event)) "full output kept in a log file"))
           (dotimes (i 20) (journal-append "agent-event" :session "s" :task (make-string 100 :initial-element #\t)))
           (ok (probe-file (merge-pathnames "journal.jsonl.1" dir)) "rotated beyond the size limit")
           (let ((watched (uiop:ensure-directory-pathname (merge-pathnames "src/" dir))))
             (ensure-directories-exist watched)
             (watch-directory watched :interval 0.2)
             (with-open-file (o (merge-pathnames "new.txt" watched) :direction :output) (write-string "hi" o))
             (ok (loop :repeat 100 :thereis (journal-query :type "fs-event") :do (sleep 0.05))
                 "a file created outside the editor is journaled")
             (ok (equal (gethash "change" (first (journal-query :type "fs-event"))) "created") "as created")
             (stop-watching)))
      (close-journal)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest test-journal-performance
  (let ((dir (temp-dir)))
    (ensure-directories-exist dir)
    (open-journal (merge-pathnames "journal.jsonl" dir))
    (unwind-protect
         (let* ((samples (loop :for i :below 10000
                               :collect (let ((t0 (now-us)))
                                          (journal-append "file-edit" :path (format nil "/p/f~D.py" (mod i 100)))
                                          (- (now-us) t0))))
                (median (nth 5000 (sort samples (function <))))
                (t0 (now-us))
                (hits (journal-query :path "/p/f42.py" :limit 1000))
                (query-us (- (now-us) t0)))
           (format t "~&journal append median ~D us; query by path over 10k: ~D us~%" median query-us)
           (ok (< median 50) "append under 50 us")
           (ok (= 100 (length hits)) "query by path finds every event")
           (ok (< query-us 10000) "query by path over 10k events under 10 ms"))
      (close-journal)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
