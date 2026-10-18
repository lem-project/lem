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

;;; T11 — ergonomic theme pack and policy

(defparameter *snapshot-attributes*
  (list (quote syntax-keyword-attribute) (quote syntax-string-attribute) (quote syntax-comment-attribute)
        (quote syntax-function-name-attribute) (quote region) (quote modeline)))

(defun attribute-snapshot ()
  (list (current-theme)
        (mapcar (lambda (name)
                  (let ((a (ensure-attribute name nil)))
                    (list (attribute-foreground a) (attribute-background a))))
                *snapshot-attributes*)))

(deftest test-ergonomic-pack-passes-policy
  (let ((pack yeditor-components/themes-ergonomic:*ergonomic-themes*))
    (ok (= (length pack) 11) "11 curated themes (plan Cap. 12-A.2)")
    (dolist (entry pack)
      (ok (null (yeditor-components/theme-policy:theme-violations (cdr entry)))
          (format nil "~A passes the ergonomic policy" (car entry)))
      (ok (find-color-theme (car entry)) (format nil "~A is a Lem color theme" (car entry))))))

(deftest test-policy-rejects-neon-and-pure-black-white
  (let ((neon (list :bg "#282c34" :fg "#abb2bf" :keyword "#00ff00" :string "#ff00ff" :function "#00ffff"
                    :constant "#ff0000" :type "#ffff00" :variable "#0000ff" :builtin "#39ff14"))
        (harsh (list :bg "#000000" :fg "#ffffff" :keyword "#c678dd" :string "#98c379" :function "#61afef"
                     :constant "#d19a66" :type "#e5c07b" :variable "#e06c75" :builtin "#56b6c2")))
    (ok (= 7 (length (remove-if-not (lambda (m) (or (search "chroma" m) (search "neon" m)))
                                    (yeditor-components/theme-policy:theme-violations neon))))
        "every neon syntax color is rejected, including light cyan")
    (ok (search "contrast" (format nil "~{~A ~}" (yeditor-components/theme-policy:theme-violations neon)))
        "low text contrast is rejected")
    (let ((v (yeditor-components/theme-policy:theme-violations harsh)))
      (ok (find "background is pure black" v :test (function equal)) "pure black background rejected")
      (ok (find "text is pure white" v :test (function equal)) "pure white text rejected"))))

(deftest test-ergonomic-pack-toggle-restores-faces
  (lem-fake-interface:with-fake-interface ()
    (load-theme "lem-default" nil)
    (let ((before (attribute-snapshot))
          (source yeditor-components/palette:*palette-themes-function*))
      (dotimes (i 200)
        (install-component (quote le-themes-ergonomic))
        (ok (equal (current-theme) "ergo-gruvbox-soft") "default theme is Gruvbox Soft")
        (ok (equal (funcall yeditor-components/palette:*palette-themes-function*)
                   (yeditor-components/themes-ergonomic:ergonomic-theme-names))
            "palette offers only the curated pack")
        (uninstall-component (quote le-themes-ergonomic))
        (ok (equal (attribute-snapshot) before) "theme and faces restored exactly")
        (ok (eq yeditor-components/palette:*palette-themes-function* source) "palette source restored"))
      (install-component (quote le-theme-extended))
      (ok (> (length (funcall yeditor-components/palette:*palette-themes-function*)) 11)
          "le-theme-extended offers every installed theme")
      (uninstall-component (quote le-theme-extended)))))
