;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/hud
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:import-from :yeditor-context/reactivity #:set-work-predicate)
  (:export :le-hud
           :*hud-buffer-name*
           :*agent-tx*
           :*agent-tx-close-functions*
           :agent-tx-id
           :agent-tx-worker
           :agent-tx-task
           :agent-tx-edits
           :agent-tx-buffers
           :open-agent-tx
           :agent-tx-open-file
           :agent-tx-insert
           :agent-tx-replace
           :close-agent-tx
           :yeditor-agent-tx-commit
           :yeditor-agent-tx-rollback
           :yeditor-demo-agent))
(in-package :yeditor-components/hud)

(defparameter *hud-buffer-name* "*yeditor-hud*")

(define-attribute agent-insert-attribute
  (:light :background "#d8f0d8")
  (:dark :background "#2f4632"))

(defstruct agent-tx
  id worker task
  (edits nil)     ; newest first: plist :name :buffer :overlay :removed :text
  (buffers nil)   ; buffers the transaction opened (closed again on rollback)
  (touched nil))  ; every buffer it edited

(defvar *agent-tx* nil
  "The open agent transaction (worker edits pending commit or rollback), or nil.
   Each edit is a workspace side-transaction (kind 2) named <agent-tx-id>/<n>.")

(defvar *agent-tx-close-functions* nil
  "Functions called with the transaction and :commit or :rollback just before it closes
   (the journal and the agent tools use this to save and record the outcome).")

(defvar *agent-tx-counter* 0)

;;; HUD rendering

(defun edit-summary (edit)
  (let* ((text (getf edit :text))
         (line (string-trim " " (subseq text 0 (or (position #\Newline text) (length text))))))
    (format nil "  + ~A:~D  ~A"
            (buffer-name (getf edit :buffer))
            (line-number-at-point (overlay-start (getf edit :overlay)))
            (if (> (length line) 40) (concatenate (quote string) (subseq line 0 40) "...") line))))

(defun render-hud ()
  (let ((buffer (get-buffer *hud-buffer-name*)))
    (when (and buffer *agent-tx*)
      (erase-buffer buffer)
      (with-point ((p (buffer-start-point buffer) :left-inserting))
        (insert-string p (format nil " Agent transaction ~A~%~%  worker  ~A~%  task    ~A~%~%"
                                 (agent-tx-id *agent-tx*) (agent-tx-worker *agent-tx*) (agent-tx-task *agent-tx*)))
        (let ((edits (reverse (agent-tx-edits *agent-tx*))))
          (insert-string p (format nil " Pending edits (~D)~%" (length edits)))
          (dolist (edit edits)
            (insert-string p (format nil "~A~%" (edit-summary edit)))))
        (insert-string p (format nil "~% C-c C-a  commit~% C-c C-k  rollback~%"))))))

;;; Agent transaction lifecycle

(defun open-agent-tx (worker task)
  "Open an agent transaction. The :agent-tx-active coeffect becomes true, which
   mounts the HUD (le-hud) through the reactivity engine."
  (when *agent-tx*
    (editor-error "An agent transaction is already open"))
  (setf *agent-tx* (make-agent-tx :id (format nil "atx-~D" (incf *agent-tx-counter*))
                                  :worker worker :task task))
  (set-work-predicate :agent-tx-active t)
  (render-hud)
  *agent-tx*)

(defun require-agent-tx ()
  (or *agent-tx* (editor-error "No agent transaction is open")))

(defun agent-tx-open-file (path)
  "The buffer visiting PATH; a buffer opened here is remembered so rollback closes it."
  (let ((tx (require-agent-tx)))
    (or (get-file-buffer path)
        (let ((buffer (find-file-buffer path)))
          (push buffer (agent-tx-buffers tx))
          buffer))))

(defun agent-tx-replace (buffer start end text)
  "Apply a worker edit: replace the characters between positions START and END (1-based,
   END exclusive) of BUFFER with TEXT, highlighted as an inline diff. The edit is a
   workspace side-transaction whose undo restores the replaced text."
  (let* ((tx (require-agent-tx))
         (name (format nil "~A/~D" (agent-tx-id tx) (1+ (length (agent-tx-edits tx)))))
         (edit (list :name name :buffer buffer :text text :removed nil :overlay nil)))
    (yeditor-tx-side:side-transaction
     name
     (lambda ()
       (with-point ((s (buffer-start-point buffer))
                    (e (buffer-start-point buffer)))
         (move-to-position s start)
         (move-to-position e end)
         (setf (getf edit :removed) (points-to-string s e))
         (delete-between-points s e)
         (insert-string s text)
         (move-to-position s start)
         (move-to-position e (+ start (length text)))
         (setf (getf edit :overlay) (make-overlay s e (quote agent-insert-attribute)))))
     (lambda ()
       ;; the overlay's points follow later edits, so undo works on the current text
       (let ((overlay (getf edit :overlay)))
         (with-point ((s (overlay-start overlay))
                      (e (overlay-end overlay)))
           (delete-overlay overlay)
           (delete-between-points s e)
           (insert-string s (getf edit :removed))))))
    (push edit (agent-tx-edits tx))
    (pushnew buffer (agent-tx-touched tx))
    (render-hud)
    name))

(defun agent-tx-insert (buffer position text)
  "Apply a worker edit: insert TEXT at POSITION (1-based) of BUFFER."
  (agent-tx-replace buffer position position text))

(defun close-agent-tx (&key rollback)
  "Commit (keep the edits) or roll back (undo them, newest first) the open agent
   transaction. The coeffect becomes false and the HUD unmounts. Returns the edit count."
  (let ((tx (require-agent-tx)))
    (dolist (function *agent-tx-close-functions*)
      (funcall function tx (if rollback :rollback :commit)))
    (dolist (edit (agent-tx-edits tx))
      (cond (rollback (yeditor-tx-side:side-undo (getf edit :name)))
            (t (delete-overlay (getf edit :overlay))
               (yeditor-tx-side:side-forget (getf edit :name)))))
    (when rollback
      (dolist (buffer (agent-tx-buffers tx))
        (unless (deleted-buffer-p buffer)
          (buffer-unmark buffer)
          (delete-buffer buffer))))
    (setf *agent-tx* nil)
    (set-work-predicate :agent-tx-active nil)
    (length (agent-tx-edits tx))))

(define-command yeditor-agent-tx-commit () ()
  "Commit the open agent transaction (C-c C-a)."
  (message "Agent transaction committed (~D edits)" (close-agent-tx)))

(define-command yeditor-agent-tx-rollback () ()
  "Roll back the open agent transaction (C-c C-k)."
  (message "Agent transaction rolled back (~D edits undone)" (close-agent-tx :rollback t)))

;;; The HUD component: mounted only while :agent-tx-active is true.

(define-component le-hud
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (list (install-buffer comp *hud-buffer-name*)
                   (install-side-window comp *hud-buffer-name* :width 52)
                   (install-keybinding comp *global-keymap* "C-c C-a" (quote yeditor-agent-tx-commit))
                   (install-keybinding comp *global-keymap* "C-c C-k" (quote yeditor-agent-tx-rollback)))))

;;; Demo worker (stand-in until real workers exist, T17-T22)

(defparameter *demo-source*
  (format nil "def mean(xs):~%    return sum(xs) / len(xs)~%~%~%def variance(xs):~%    m = mean(xs)~%    return sum((x - m) ** 2 for x in xs) / len(xs)~%"))

(define-command yeditor-demo-agent () ()
  "Open a demo agent transaction that proposes two edits to *yeditor-demo*."
  (let ((buffer (or (get-buffer "*yeditor-demo*")
                    (let ((b (make-buffer "*yeditor-demo*")))
                      (insert-string (buffer-start-point b) *demo-source*)
                      b))))
    (switch-to-buffer buffer)
    (open-agent-tx "demo-worker (local)" "Document the module and guard empty input")
    (let ((guard-at (+ 1 (length (format nil "def mean(xs):~%")))))
      (agent-tx-insert buffer guard-at
                       (format nil "    if not xs:~%        raise ValueError(\"mean of empty sequence\")~%"))
      (agent-tx-insert buffer 1 (format nil "\"\"\"Basic statistics helpers.\"\"\"~%~%~%")))))
