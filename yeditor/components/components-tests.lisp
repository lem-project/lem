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

;;; T13 — le-lsp: language server sessions as transactions

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; define-language-spec inspects the mode at macroexpansion time
  (define-major-mode yeditor-fake-lang-mode lem/language-mode:language-mode
      (:name "FakeLang"))
  (define-major-mode yeditor-missing-lang-mode lem/language-mode:language-mode
      (:name "MissingLang")))

(defparameter *fake-lsp-server*
  (namestring (asdf:system-relative-pathname "yeditor-components" "test-data/fake-lsp-server.py")))

(lem-lsp-mode:define-language-spec (yeditor-fake-spec yeditor-fake-lang-mode)
  :language-id "yeditor-fake"
  :command (list "python3" *fake-lsp-server*)
  :install-command "pip install yeditor-fake-server"
  :connection-mode :stdio)

(lem-lsp-mode:define-language-spec (yeditor-missing-spec yeditor-missing-lang-mode)
  :language-id "yeditor-missing"
  :command (list "yeditor-no-such-language-server")
  :install-command "npm install -g yeditor-no-such-language-server"
  :connection-mode :stdio)

(defun lang-buffer (name mode)
  (let ((buffer (make-buffer name)))
    (let ((lem-lsp-mode:*disable* t))
      (change-buffer-mode buffer mode))
    buffer))

(deftest test-le-lsp-session-start-stop
  (lem-fake-interface:with-fake-interface ()
    (let ((disable-before lem-lsp-mode:*disable*)
          (buffer (lang-buffer "main.fake" (quote yeditor-fake-lang-mode))))
      (install-component (quote le-lsp))
      (ok (eq lem-lsp-mode:*disable* t) "Lem's untracked auto-start is suppressed while installed")
      (dotimes (i 20)
        (let ((workspace (yeditor-components/lsp:lsp-start-buffer buffer)))
          (ok (lem-lsp-mode:lsp-session-alive-p workspace) "server process running after start")
          (ok (eq workspace (yeditor-components/lsp:lsp-start-buffer buffer))
              "second start for the same language reuses the session")
          (ok (= 1 (length yeditor-components/lsp:*lsp-sessions*)) "one session per language")
          (yeditor-components/lsp:lsp-stop-all)
          (sleep 0.05)
          (ok (not (lem-lsp-mode:lsp-session-alive-p workspace)) "server process gone after stop")
          (ok (null yeditor-components/lsp:*lsp-sessions*) "no session left")))
      (let ((workspace (yeditor-components/lsp:lsp-start-buffer buffer)))
        (uninstall-component (quote le-lsp))
        (sleep 0.05)
        (ok (not (lem-lsp-mode:lsp-session-alive-p workspace)) "uninstall stops running servers")
        (ok (null yeditor-components/lsp:*lsp-sessions*) "uninstall clears sessions")
        (ok (eq lem-lsp-mode:*disable* disable-before) "auto-start setting restored"))
      (kill-buffer buffer))))

(deftest test-le-lsp-missing-server-reports-install-command
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lang-buffer "main.missing" (quote yeditor-missing-lang-mode))))
      (install-component (quote le-lsp))
      (let ((condition (handler-case (progn (yeditor-components/lsp:lsp-start-buffer buffer) nil)
                         (lem-lsp-mode:not-found-program (c) c))))
        (ok condition "missing server signals not-found-program")
        (ok (search "npm install -g yeditor-no-such-language-server" (format nil "~A" condition))
            "the message carries the install command"))
      (ok (null yeditor-components/lsp:*lsp-sessions*) "nothing left running")
      (uninstall-component (quote le-lsp))
      (kill-buffer buffer))))

(deftest test-le-lsp-modeline-segment
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (lang-buffer "seg.fake" (quote yeditor-fake-lang-mode))))
      (install-component (quote le-lsp))
      (switch-to-buffer buffer)
      (ok (equal (yeditor-components/lsp:lsp-modeline-segment (current-window)) " LSP:off ") "off before start")
      (yeditor-components/lsp:lsp-start-buffer buffer)
      (ok (equal (yeditor-components/lsp:lsp-modeline-segment (current-window)) " LSP:yeditor-fake ")
          "shows the running language server")
      (uninstall-component (quote le-lsp))
      (kill-buffer buffer))))

;;; T14 — package onboarding under transaction

(defun make-dummy-project ()
  "A local ASDF project in a fresh temp root; returns the root directory."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "yeditor-t14-~D/" (random 1000000))
                                 (uiop:default-temporary-directory))))
         (dir (merge-pathnames "yeditor-dummy-pkg/" root)))
    (ensure-directories-exist dir)
    (with-open-file (o (merge-pathnames "yeditor-dummy-pkg.asd" dir) :direction :output :if-exists :supersede)
      (write-string "(defsystem \"yeditor-dummy-pkg\" :components ((:file \"main\")))" o))
    (with-open-file (o (merge-pathnames "main.lisp" dir) :direction :output :if-exists :supersede)
      (write-string "(defpackage :yeditor-dummy-pkg (:use :cl) (:export :hello))
(in-package :yeditor-dummy-pkg)
(defun hello () \"hello\")" o))
    root))

(defun log-events (path component)
  (with-open-file (in path)
    (loop :for line := (read-line in nil) :while line
          :for rec := (yason:parse line)
          :when (search (symbol-name component) (gethash "component" rec))
            :collect (gethash "event" rec))))

(deftest test-package-install-uninstall-cycle
  (let* ((root (make-dummy-project))
         (log (merge-pathnames "effects.log" root))
         (registry-before (copy-list asdf:*central-registry*))
         (component (yeditor-components/packages:package-component-name "yeditor-dummy-pkg")))
    (yeditor-context/durability:enable-durability :path log)
    (unwind-protect
         (progn
           (ok (assoc "yeditor-dummy-pkg" (yeditor-components/packages:scan-local-systems (list root))
                      :test (function string=))
               "scanner finds the local system")
           (ok (null (yeditor-components/packages:install-package
                      "yeditor-dummy-pkg" :roots (list root) :confirm (constantly nil)))
               "declining the confirmation installs nothing")
           (ok (null (find-package :yeditor-dummy-pkg)) "nothing loaded after decline")
           (dotimes (i 20)
             (yeditor-components/packages:install-package "yeditor-dummy-pkg" :roots (list root))
             (ok (equal (uiop:symbol-call :yeditor-dummy-pkg :hello) "hello") "package loaded and usable")
             (ok (member "yeditor-dummy-pkg" (yeditor-components/packages:installed-packages)
                         :test (function string=))
                 "listed as installed")
             (yeditor-components/packages:uninstall-package "yeditor-dummy-pkg")
             (ok (null (find-package :yeditor-dummy-pkg)) "package deleted on uninstall")
             (ok (null (asdf:registered-system "yeditor-dummy-pkg")) "ASDF system cleared")
             (ok (equal asdf:*central-registry* registry-before) "source registry restored")
             (ok (null (gethash component *component-registry*)) "runtime component unregistered"))
           (let ((events (log-events log component)))
             (ok (= 20 (count "install-commit" events :test (function string=))) "every install logged")
             (ok (= 20 (count "uninstall" events :test (function string=))) "every uninstall logged")))
      (yeditor-context/durability:disable-durability)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(deftest test-package-missing-system
  (ok (handler-case (progn (yeditor-components/packages:install-package
                            "yeditor-no-such-system" :roots (list (uiop:default-temporary-directory)))
                           nil)
        (editor-error () t))
      "a missing system is an editor error, nothing registered"))

;;; T15 — inherited features as toggleable components

(defun feature-state ()
  (list (mapcar (lambda (entry) (mapcar (lambda (k) (binding (car k))) (cdr entry)))
                yeditor-components/features:*feature-keys*)
        (and (find (quote lem/filer:sync-filer-to-buffer-directory) *switch-to-buffer-hook* :key (function car))
             t)))

(deftest test-features-toggle-without-residue
  (let ((lem-defaults (feature-state)))
    (ok (binding "C-x g") "Lem binds C-x g at load time")
    (install-component (quote le-inherited-entry-points-off))
    (ok (every (lambda (keys) (every (function null) keys)) (first (feature-state)))
        "basal state: no inherited entry-point bindings")
    (ok (null (second (feature-state))) "basal state: no filer hook")
    (dolist (feature (list (quote le-git) (quote le-project) (quote le-tree)))
      (dotimes (i 100)
        (install-component feature)
        (uninstall-component feature))
      (ok (every (lambda (keys) (every (function null) keys)) (first (feature-state)))
          (format nil "~A: 100 toggles leave no binding" feature))
      (ok (null (second (feature-state))) (format nil "~A: 100 toggles leave no hook" feature)))
    (install-component (quote le-git))
    (install-component (quote le-project))
    (install-component (quote le-tree))
    (ok (eq (binding "C-x g") (quote lem/legit:legit-status)) "le-git binds C-x g")
    (ok (eq (binding "C-x p f") (quote lem-core/commands/project:project-find-file)) "le-project binds C-x p f")
    (ok (eq (binding "C-x d") (quote lem/filer:filer)) "le-tree binds C-x d")
    (ok (second (feature-state)) "le-tree installs the filer hook")
    (dolist (feature (list (quote le-tree) (quote le-project) (quote le-git)))
      (uninstall-component feature))
    (uninstall-component (quote le-inherited-entry-points-off))
    (ok (equal (feature-state) lem-defaults) "uninstalling the basal state restores Lem's defaults exactly")))
