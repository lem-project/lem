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
           :open-agent-tx
           :agent-tx-insert
           :close-agent-tx
           :yeditor-agent-tx-commit
           :yeditor-agent-tx-rollback
           :yeditor-demo-agent))
(in-package :yeditor-components/hud)

(defparameter *hud-buffer-name* "*yeditor-hud*")

(define-attribute agent-insert-attribute
  (:light :background "#d8f0d8")
  (:dark :background "#2f4632"))

(defstruct agent-tx worker task (edits nil))

(defvar *agent-tx* nil
  "The open agent transaction (worker edits pending commit or rollback), or nil.
   Minimal model for the T12 HUD; the real multiplexer is T22 (yeditor-enterprise).")

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
        (insert-string p (format nil " Agent transaction~%~%  worker  ~A~%  task    ~A~%~%"
                                 (agent-tx-worker *agent-tx*) (agent-tx-task *agent-tx*)))
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
  (setf *agent-tx* (make-agent-tx :worker worker :task task))
  (set-work-predicate :agent-tx-active t)
  (render-hud)
  *agent-tx*)

(defun agent-tx-insert (buffer position text)
  "Apply a worker edit: insert TEXT at POSITION (1-based) of BUFFER, highlighted as
   an inline diff, and record its inverse in the open agent transaction."
  (unless *agent-tx*
    (editor-error "No agent transaction is open"))
  (let (overlay)
    (with-point ((s (buffer-start-point buffer))
                 (e (buffer-start-point buffer)))
      (move-to-position s position)
      (insert-string s text)
      (move-to-position s position)
      (move-to-position e (+ position (length text)))
      (setf overlay (make-overlay s e (quote agent-insert-attribute))))
    (push (list :buffer buffer :position position :text text :overlay overlay)
          (agent-tx-edits *agent-tx*))
    (render-hud)))

(defun close-agent-tx (&key rollback)
  "Commit (keep the edits) or roll back (undo them, newest first) the open agent
   transaction. The coeffect becomes false and the HUD unmounts."
  (unless *agent-tx*
    (editor-error "No agent transaction is open"))
  (dolist (edit (agent-tx-edits *agent-tx*))
    (delete-overlay (getf edit :overlay))
    (when rollback
      (with-point ((p (buffer-start-point (getf edit :buffer))))
        (move-to-position p (getf edit :position))
        (delete-character p (length (getf edit :text))))))
  (let ((count (length (agent-tx-edits *agent-tx*))))
    (setf *agent-tx* nil)
    (set-work-predicate :agent-tx-active nil)
    count))

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
