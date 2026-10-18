;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components-tests
  (:use :cl :rove :lem :yeditor-context :yeditor-components)
  (:import-from :yeditor-components/zero-chrome #:*minimal-modeline-format*)
  (:import-from :yeditor-components/palette #:call-with-theme-preview #:*open-preview-sessions*)
  (:import-from :yeditor-components/hud
                #:*agent-tx* #:*hud-buffer-name* #:yeditor-demo-agent
                #:yeditor-agent-tx-commit #:yeditor-agent-tx-rollback))
(in-package :yeditor-components-tests)

(defun binding (keyspec)
  (let ((prefix (keymap-find *global-keymap* (parse-keyspec keyspec))))
    (and prefix (prefix-suffix prefix))))

(defun demo-text ()
  (let ((buffer (get-buffer "*yeditor-demo*")))
    (and buffer (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))))

(defun reset-demo ()
  (when *agent-tx* (yeditor-agent-tx-rollback))
  (alexandria:when-let ((b (get-buffer "*yeditor-demo*"))) (kill-buffer b)))

(deftest test-zero-chrome-basal-state-and-restore
  (lem-fake-interface:with-fake-interface ()
    (let ((fm (variable-value (quote lem/frame-multiplexer:frame-multiplexer) :global))
          (modeline (variable-value (quote modeline-format) :global)))
      (dotimes (i 100)
        (install-component (quote le-zero-chrome))
        (ok (null (variable-value (quote lem/frame-multiplexer:frame-multiplexer) :global))
            "tab bar off in the basal state")
        (ok (equal (variable-value (quote modeline-format) :global) *minimal-modeline-format*)
            "one-line minimal modeline")
        (uninstall-component (quote le-zero-chrome))
        (ok (eq (variable-value (quote lem/frame-multiplexer:frame-multiplexer) :global) fm)
            "tab bar setting restored")
        (ok (equal (variable-value (quote modeline-format) :global) modeline)
            "modeline restored")))))

(deftest test-palette-preview-abort-restores-theme
  (lem-fake-interface:with-fake-interface ()
    (load-theme (first (color-theme-names)) nil)
    (let* ((names (color-theme-names))
           (original (current-theme))
           (others (remove original names :test (function equal))))
      (ok (> (length others) 3) "several themes available to preview")
      (dotimes (i 1000)
        (handler-case
            (call-with-theme-preview
             (lambda (preview)
               (funcall preview (nth (mod i (length others)) others))
               (funcall preview (nth (mod (1+ i) (length others)) others))
               (error (quote editor-abort))))
          (editor-abort () nil)))
      (ok (equal (current-theme) original) "abort (C-g) restores the original theme after 1000 sessions")
      (ok (zerop *open-preview-sessions*) "no preview session leaks")
      (ok (null (call-with-theme-preview
                 (lambda (preview) (funcall preview (first others)) nil)))
          "a nil choice cancels")
      (ok (equal (current-theme) original) "and restores the original theme"))))

(deftest test-hud-mounts-and-unmounts-with-agent-tx
  (lem-fake-interface:with-fake-interface ()
   (lem/common/timer:with-timer-manager (make-instance (quote lem/common/timer:timer-manager))
    (reset-demo)
    (let ((commit-key (binding "C-c C-a"))
          (rollback-key (binding "C-c C-k")))
      (enable-ephemeral-ui)
      (unwind-protect
           (progn
             (yeditor-demo-agent)
             (let ((original (format nil "~A" yeditor-components/hud::*demo-source*))
                   (windows (length (window-list))))
               ;; the demo opens the tx after creating the buffer: undo once to measure the basal layout
               (yeditor-agent-tx-rollback)
               (ok (equal (demo-text) original) "rollback restores the source exactly")
               (ok (null (get-buffer *hud-buffer-name*)) "HUD buffer removed on rollback")
               (ok (= (length (window-list)) (1- windows)) "HUD side window removed")
               (ok (eq (binding "C-c C-a") commit-key) "C-c C-a binding restored")
               (dotimes (i 100)
                 (yeditor-demo-agent)
                 (ok (and *agent-tx* (get-buffer *hud-buffer-name*)) "HUD mounted while tx is open")
                 (ok (= (length (window-list)) windows) "exactly one side window")
                 (ok (eq (binding "C-c C-a") (quote yeditor-agent-tx-commit)) "C-c C-a bound while open")
                 (ok (search "raise ValueError" (demo-text)) "worker edit applied")
                 (if (evenp i)
                     (progn (yeditor-agent-tx-rollback)
                            (ok (equal (demo-text) original) "rollback: source unchanged"))
                     (progn (yeditor-agent-tx-commit)
                            (ok (search "Basic statistics helpers" (demo-text)) "commit: edits kept")
                            (kill-buffer (get-buffer "*yeditor-demo*"))))
                 (ok (null *agent-tx*) "tx closed")
                 (ok (null (get-buffer *hud-buffer-name*)) "HUD unmounted")
                 (ok (= (length (window-list)) (1- windows)) "basal layout: no residual split")
                 (ok (eq (binding "C-c C-k") rollback-key) "C-c C-k binding restored"))))
        (reset-demo)
        (disable-ephemeral-ui))))))
