;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-navigation-tests (:use :cl :rove :lem))
(in-package :yeditor-navigation-tests)

(defun action (name &rest args)
  (apply #'yeditor-components/navigation:navigation-action name args))

(defun with-navigation (function)
  (save-excursion
    (lem/buffer/buffer-list-manager:with-current-buffers ()
      (lem-fake-interface:with-fake-interface ()
        (setf (current-buffer) (window-buffer (current-window)))
        (unwind-protect (funcall function)
          (ignore-errors (yeditor-context:uninstall-component 'yeditor-components/navigation:le-navigation)))))))

(defun fixture ()
  (let ((root (merge-pathnames (format nil "yeditor-navigation-~D/" (random 100000000)) #p"/tmp/")))
    (ensure-directories-exist (merge-pathnames "src/" root))
    (dolist (file '("src/a.lisp" "src/b.lisp"))
      (with-open-file (s (merge-pathnames file root) :direction :output :if-exists :supersede)
        (write-line "original" s)))
    root))

(defun install-nav () (yeditor-context:install-component 'yeditor-components/navigation:le-navigation))

(deftest navigation-100-install-uninstall-cycles
  (with-navigation
   (lambda ()
     (let ((buffers (copy-list (buffer-list)))
           (headers (copy-list (frame-header-windows (current-frame))))
           (hooks (copy-list *post-command-hook*))
           (splash *splash-function*)
           (f2 (binding "F2")) (f3 (binding "F3"))
           (size (list (window-width (current-window)) (window-height (current-window)))))
       (dotimes (i 100)
         (install-nav)
         (ok (= (1+ (length headers)) (length (frame-header-windows (current-frame)))))
         (yeditor-context:uninstall-component 'yeditor-components/navigation:le-navigation)
         (ok (equal headers (frame-header-windows (current-frame))))
         (ok (equal hooks *post-command-hook*))
         (ok (eq splash *splash-function*))
         (ok (equal buffers (buffer-list)))
         (ok (eq f2 (binding "F2")))
         (ok (eq f3 (binding "F3")))
         (ok (equal size (list (window-width (current-window)) (window-height (current-window))))))))))

(deftest project-preview-pin-and-dirty-close
  (with-navigation
   (lambda ()
     (let ((root (fixture)) (a nil) (b nil))
       (unwind-protect
            (progn
              (install-nav)
              (action "project" :path (namestring root))
              (ok (equal (namestring root) (cdr (assoc "project" (yeditor-components/navigation:navigation-status) :test #'equal))))
              (action "preview" :path "src/a.lisp")
              (setf a (current-buffer))
              (action "preview" :path "src/b.lisp")
              (ok (deleted-buffer-p a) "clean owned preview is replaced")
              (action "pin")
              (setf b (current-buffer))
              (action "preview" :path "src/a.lisp")
              (ok (not (deleted-buffer-p b)) "pinned file survives another preview")
              (setf a (current-buffer))
              (insert-string (buffer-end-point a) "human edit")
              (action "close")
              (ok (not (deleted-buffer-p a)) "dirty file waits for a visible decision")
              (action "cancel")
              (ok (search "human edit" (buffer-text a)))
              (action "close" :path (namestring (buffer-filename a)))
              (action "discard")
              (ok (deleted-buffer-p a))
              (ok (equal (format nil "original~%") (uiop:read-file-string (merge-pathnames "src/a.lisp" root)))))
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(deftest cancel-project-selector-and-missing-file
  (with-navigation
   (lambda ()
     (install-nav)
     (let ((before (current-buffer)))
       (action "browse")
       (action "cancel")
       (ok (eq before (current-buffer)) "cancelling project selection restores the editor"))
     (ok (signals (action "open" :path "/tmp/yeditor-nonexistent-file-928348.lisp") 'error))
     (ok (not (get-file-buffer "/tmp/yeditor-nonexistent-file-928348.lisp")) "no phantom buffer for stale recent file"))))

(defun drain-until (predicate)
  (loop :repeat 200 :until (funcall predicate) :do (receive-event 0.01))
  (ok (funcall predicate) "background operation completed on the UI queue"))

(defun nav-state () yeditor-components/navigation::*navigation*)

(defun binding (key)
  (let ((prefix (keymap-find *global-keymap* (parse-keyspec key))))
    (and prefix (prefix-suffix prefix))))

(deftest asynchronous-search-buttons-and-position
  (with-navigation
   (lambda ()
     (let ((root (fixture)))
       (unwind-protect
            (progn
              (install-nav)
              (action "project" :path (namestring root))
              (drain-until (lambda () (not (yeditor-components/navigation::navigation-loading (nav-state)))))
              (ok (equal '("src/a.lisp" "src/b.lisp") (yeditor-components/navigation::navigation-candidates (nav-state))))
              (action "search" :query "src/a")
              (ok (search "[src/a.lisp]" (buffer-text (current-buffer))))
              (ok (not (search "[src/b.lisp]" (buffer-text (current-buffer)))))
              ;; Native button callback is the exact entry point used by mouse clicks.
              (search-forward (current-point) "[src/a.lisp]")
              (character-offset (current-point) -5)
              (lem/button:button-action (lem/button:button-at (current-point)))
              (let ((file (current-buffer)))
                (ok (buffer-filename file))
                (character-offset (current-point) 3)
                (let ((offset (point-charpos (current-point))))
                  (action "home")
                  (action "cancel")
                  (ok (eq file (current-buffer)))
                  (ok (= offset (point-charpos (current-point))))))
              (action "browse" :path (namestring root))
              (drain-until (lambda () (not (yeditor-components/navigation::navigation-browser-loading (nav-state)))))
              (ok (search "[src]" (buffer-text (current-buffer)))))
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(deftest save-close-and-borrowed-preview
  (with-navigation
   (lambda ()
     (let* ((root (fixture)) (a-path (merge-pathnames "src/a.lisp" root))
            (borrowed (find-file-buffer a-path)))
       (unwind-protect
            (progn
              (install-nav)
              (action "project" :path (namestring root))
              (action "preview" :path "src/a.lisp")
              (action "preview" :path "src/b.lisp")
              (ok (not (deleted-buffer-p borrowed)) "preview never owns a preexisting file buffer")
              (let ((preview (current-buffer)))
                (action "pin" :path "src/a.lisp")
                (ok (eq preview (yeditor-components/navigation::navigation-preview (nav-state))) "pin another tab does not pin the preview")
                (insert-string (buffer-end-point preview) "saved edit")
                (yeditor-components/navigation::after-navigation-command)
                (ok (null (yeditor-components/navigation::navigation-preview (nav-state))) "editing pins a preview")
                (action "close")
                (ok (search "Salvar e fechar" (buffer-text (current-buffer))))
                (action "save-close")
                (ok (deleted-buffer-p preview))
                (ok (search "saved edit" (uiop:read-file-string (merge-pathnames "src/b.lisp" root))))))
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(deftest borrowed-sidebar-focus-and-queued-work-cleanup
  (with-navigation
   (lambda ()
     (let ((root (fixture)) (borrowed (make-buffer "*navigation borrowed side*" :temporary t)))
       (unwind-protect
            (progn
              (make-leftside-window borrowed :width 20)
              (install-nav)
              (action "project" :path (namestring root))
              (action "focus")
              (ok (null (frame-leftside-window (current-frame))))
              (action "tree")
              (ok (eq (window-buffer (frame-leftside-window (current-frame)))
                      (yeditor-components/navigation::navigation-tree (nav-state))))
              (let ((state (nav-state)))
                (yeditor-context:uninstall-component 'yeditor-components/navigation:le-navigation)
                (receive-event 0.05)
                (ok (not (yeditor-components/navigation::navigation-alive state))))
              (ok (eq borrowed (window-buffer (frame-leftside-window (current-frame)))))
              (ok (= 20 (window-width (frame-leftside-window (current-frame))))))
         (when (frame-leftside-window (current-frame)) (delete-leftside-window))
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))

(deftest visible-controls-render-in-reading-order
  (with-navigation
   (lambda ()
     (install-nav)
     (let* ((bar (yeditor-components/navigation::navigation-bar (nav-state)))
            (text (buffer-text bar)))
       (ok (uiop:string-prefix-p "[Abrir pasta] [Arquivos (F2)] [Abrir arquivo (F3)] [Foco]" text))
       (ok (< (search "Projeto:" text) (search "[Abas]" text)))
       (ok (= 1 (line-number-at-point (buffer-point bar)))))
     (action "home")
     (ok (uiop:string-prefix-p "Yeditor — Arquivos" (buffer-text (current-buffer))))
     (buffer-start (current-point))
     (yeditor-components/navigation::navigation-next)
     (ok (lem/button:button-at (current-point)))
     (yeditor-components/navigation::navigation-activate)
     (ok (yeditor-components/navigation::navigation-browser (nav-state)))
     (yeditor-components/navigation::navigation-cancel)
     (ok (null (yeditor-components/navigation::navigation-browser (nav-state)))))))

(deftest returning-to-file-preserves-selection-and-scroll
  (with-navigation
   (lambda ()
     (let ((root (fixture)))
       (unwind-protect
            (progn
              (with-open-file (stream (merge-pathnames "src/a.lisp" root) :direction :output :if-exists :supersede)
                (dotimes (line 100) (format stream "; line ~D~%" line)))
              (install-nav)
              (action "project" :path (namestring root))
              (action "open" :path "src/a.lisp")
              (let ((buffer (current-buffer)))
                (move-to-line (current-point) 45)
                (move-to-line (window-view-point (current-window)) 40)
                (with-point ((mark (current-point)))
                  (line-offset mark 1)
                  (set-current-mark mark))
                (action "home")
                (action "cancel")
                (ok (eq buffer (current-buffer)))
                (ok (= 45 (line-number-at-point (current-point))))
                (ok (= 40 (line-number-at-point (window-view-point (current-window)))))
                (ok (buffer-mark-p buffer))
                (ok (= 46 (line-number-at-point (buffer-mark buffer))))))
         (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))))
