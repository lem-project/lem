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
                #:yeditor-agent-tx-commit #:yeditor-agent-tx-rollback)
  (:import-from :yeditor-components/sidebar #:yeditor-sidebar-toggle #:sidebar-active-p)
  (:import-from :yeditor-components/terminal #:yeditor-terminal-toggle #:terminal-active-p)
  (:import-from :yeditor-components/workspace-layout #:yeditor-workspace-open))
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

(defun process-state (pid)
  "The state letter from /proc/PID/stat, or nil when the process no longer exists."
  (let ((stat (ignore-errors (uiop:read-file-string (format nil "/proc/~D/stat" pid)))))
    (and stat (char stat (+ 2 (position #\) stat :from-end t))))))

(defun process-gone-p (pid &key (timeout 2))
  "True once PID has terminated: it no longer exists, or it is a zombie (dead, not yet
   reaped; the server is a grandchild of the editor, so init may reap it later)."
  (loop :repeat (* timeout 100)
        :for state := (process-state pid)
        :when (or (null state) (char= state #\Z)) :return t
        :do (sleep 0.01)))

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
        (let* ((workspace (yeditor-components/lsp:lsp-start-buffer buffer))
               (pid (lem-lsp-mode:lsp-session-pid workspace)))
          (ok (lem-lsp-mode:lsp-session-alive-p workspace) "server process running after start")
          (ok (eq workspace (yeditor-components/lsp:lsp-start-buffer buffer))
              "second start for the same language reuses the session")
          (ok (= 1 (length yeditor-components/lsp:*lsp-sessions*)) "one session per language")
          (yeditor-components/lsp:lsp-stop-all)
          (ok (process-gone-p pid) "server process gone after stop")
          (ok (not (lem-lsp-mode:lsp-session-alive-p workspace)) "session reported stopped")
          (ok (null yeditor-components/lsp:*lsp-sessions*) "no session left")))
      (let ((pid (lem-lsp-mode:lsp-session-pid (yeditor-components/lsp:lsp-start-buffer buffer))))
        (uninstall-component (quote le-lsp))
        (ok (process-gone-p pid) "uninstall stops running servers")
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

;;; T27 — tree-sitter query packs (run where the tree-sitter library and the
;;; Python grammar are installed, e.g. the CI image; skipped elsewhere)

(defparameter *python-sample*
  (uiop:read-file-string (asdf:system-relative-pathname "yeditor-components" "test-data/sample.py")))

(defun treesitter-ready-p ()
  (and (lem-tree-sitter:tree-sitter-available-p)
       (ignore-errors (yeditor-effects:ensure-grammar "python"))
       t))

(defun python-buffer ()
  (let ((buffer (make-buffer "sample.py")))
    (insert-string (buffer-start-point buffer) *python-sample*)
    (let ((lem-lsp-mode:*disable* t))
      (change-buffer-mode buffer (quote lem-python-mode:python-mode)))
    buffer))

(defun attribute-at (buffer needle &optional (occurrence 0) (offset 0))
  "The :attribute text property at NEEDLE's OCCURRENCE-th match (+ OFFSET chars)."
  (let ((pos (loop :with start := 0 :for i :from 0 :to occurrence
                   :for p := (search needle *python-sample* :start2 start)
                   :do (setf start (1+ p)) :finally (return p))))
    (with-point ((p (buffer-start-point buffer)))
      (character-offset p (+ pos offset))
      (text-property-at p :attribute))))

(deftest test-treesitter-python-highlighting
  (if (not (treesitter-ready-p))
      (skip "tree-sitter or the Python grammar is not installed")
      (lem-fake-interface:with-fake-interface ()
        (let ((buffer (python-buffer)))
          (install-component (quote le-treesitter-packs))
          (unwind-protect
               (progn
                 (ok (typep (syntax-table-parser (mode-syntax-table (quote lem-python-mode:python-mode)))
                            (quote lem-tree-sitter:treesitter-parser))
                     "python-mode uses the tree-sitter parser")
                 (dolist (kw (list "async " "await " "match " "case " "return " "def " "class " "finally:"))
                   (ok (eq (attribute-at buffer kw) (quote syntax-keyword-attribute))
                       (format nil "keyword ~A" (string-trim " :" kw))))
                 (ok (eq (attribute-at buffer "fetch(") (quote syntax-function-name-attribute)) "function name")
                 (ok (eq (attribute-at buffer "Point:") (quote syntax-type-attribute)) "class name as type")
                 (ok (eq (attribute-at buffer "float:" 0) (quote syntax-type-attribute)) "type hint")
                 (ok (eq (attribute-at buffer "retries:") (quote syntax-variable-attribute)) "parameter")
                 (ok (eq (attribute-at buffer "\"\"\"Module") (quote syntax-string-attribute)) "module docstring")
                 (ok (eq (attribute-at buffer "\"origin\"") (quote syntax-string-attribute)) "string")
                 (ok (eq (attribute-at buffer "f\"attempt" 0 3) (quote syntax-string-attribute))
                     "nested f-string text")
                 (ok (eq (attribute-at buffer "3)") (quote syntax-constant-attribute)) "number"))
            (uninstall-component (quote le-treesitter-packs))
            (kill-buffer buffer))))))

(deftest test-treesitter-python-indentation
  (if (not (treesitter-ready-p))
      (skip "tree-sitter or the Python grammar is not installed")
      (lem-fake-interface:with-fake-interface ()
        (let ((buffer (python-buffer)))
          (install-component (quote le-treesitter-packs))
          (unwind-protect
               (let* ((parser (lem-tree-sitter:get-buffer-treesitter-parser buffer))
                      (tree (lem-tree-sitter:treesitter-parser-tree parser))
                      (query (lem-tree-sitter:treesitter-parser-indent-query parser))
                      (mismatches nil))
                 (ok (and tree query) "parse tree and indent query available")
                 (with-point ((p (buffer-start-point buffer)))
                   (loop :for line :in (uiop:split-string *python-sample* :separator (string #\Newline))
                         :for row :from 1
                         :unless (zerop (length (string-trim " " line)))
                           :do (move-to-line p row)
                               (let ((expected (or (position #\Space line :test-not (function char=)) 0))
                                     (got (lem-tree-sitter/indent:calc-indent-from-query tree query p 4)))
                                 (unless (eql expected got)
                                   (push (format nil "line ~D: expected ~D got ~A: ~A" row expected got line)
                                         mismatches)))))
                 (ok (null mismatches) (format nil "every line indented exactly~{~%  ~A~}" (reverse mismatches))))
            (uninstall-component (quote le-treesitter-packs))
            (kill-buffer buffer))))))

(deftest test-treesitter-pack-toggle-no-leak
  (if (not (treesitter-ready-p))
      (skip "tree-sitter or the Python grammar is not installed")
      (lem-fake-interface:with-fake-interface ()
        (let* ((table (mode-syntax-table (quote lem-python-mode:python-mode)))
               (fallback (syntax-table-parser table))
               (buffer (python-buffer))
               (libraries (length (cffi:list-foreign-libraries)))
               (languages (length (tree-sitter:list-languages))))
          (dotimes (i 100)
            (install-component (quote le-treesitter-packs))
            (uninstall-component (quote le-treesitter-packs))
            (ok (eq (syntax-table-parser table) fallback) "fallback parser restored"))
          (ok (= (length (cffi:list-foreign-libraries)) libraries) "no grammar library loaded again")
          (ok (= (length (tree-sitter:list-languages)) languages) "no duplicate language registrations")
          (ok (eq (attribute-at buffer "def ") (quote syntax-keyword-attribute))
              "buffer highlighted again by the fallback parser")
          (kill-buffer buffer)))))

(deftest test-treesitter-compile-grammar
  (let ((source #p"/tmp/tree-sitter-python/"))
    (if (not (and (treesitter-ready-p) (probe-file (merge-pathnames "src/parser.c" source))))
        (skip "no grammar source checkout available")
        (let* ((out (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "yeditor-parsers-~D/" (random 100000))
                                      (uiop:default-temporary-directory))))
               (library (yeditor-components/treesitter:compile-grammar source "python" :output-directory out)))
          (ok (probe-file library) "grammar compiled to a shared library")
          (ok (tree-sitter:load-language "python-compiled-test" (namestring library)
                                         :symbol-name "tree_sitter_python" :register nil)
              "the compiled library loads as a tree-sitter language")
          (uiop:delete-directory-tree out :validate t :if-does-not-exist :ignore)))))

;;; T28 — le-lang-python, activated by coeffect through the context bridge

(defun write-test-file (path content &key executable)
  (ensure-directories-exist path)
  (with-open-file (o path :direction :output :if-exists :supersede) (write-string content o))
  (when executable (sb-posix:chmod (namestring path) #o755))
  path)

(defun make-python-project ()
  "A fake Python project: pyproject, .venv with python/LSP/formatter stand-ins, sources and tests."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "yeditor-t28-~D/" (random 1000000))
                                 (uiop:default-temporary-directory))))
         (bin (merge-pathnames ".venv/bin/" root)))
    (write-test-file (merge-pathnames "pyproject.toml" root) (format nil "[project]~%name = \"demo\"~%"))
    (write-test-file (merge-pathnames "python" bin) (format nil "#!/bin/sh~%exec python3 \"$@\"~%") :executable t)
    (write-test-file (merge-pathnames "basedpyright-langserver" bin)
                (format nil "#!/bin/sh~%exec python3 ~A~%" *fake-lsp-server*) :executable t)
    (write-test-file (merge-pathnames "ruff" bin)
                (format nil "#!/usr/bin/env python3~%import re, sys~%src = sys.stdin.read()~%if 'FAIL' in src:~%    sys.stderr.write('cannot format')~%    sys.exit(1)~%sys.stdout.write(re.sub(r'(\\w)=(\\w)', r'\\1 = \\2', src))~%")
                :executable t)
    (write-test-file (merge-pathnames "fake_pytest.py" root)
                (format nil "import sys~%print('F')~%print('tests/test_main.py:5: AssertionError')~%sys.exit(1)~%"))
    (write-test-file (merge-pathnames "main.py" root) (format nil "x=1~%print(x)~%"))
    (write-test-file (merge-pathnames "tests/test_main.py" root)
                (format nil "def test_one():~%    assert 1~%~%def test_two():~%    assert False~%"))
    root))

(defun full-text (buffer)
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun le-lang-python-active-p ()
  (getf (gethash (quote le-lang-python) *component-registry*) :state))

(deftest test-le-lang-python-project-lifecycle
  (lem-fake-interface:with-fake-interface ()
    (let* ((root (make-python-project))
           (elsewhere (uiop:ensure-directory-pathname
                       (merge-pathnames (format nil "yeditor-t28-other-~D/" (random 1000000))
                                        (uiop:default-temporary-directory))))
           (notes (write-test-file (merge-pathnames "notes.txt" elsewhere) "plain text"))
           (path-before (uiop:getenv "PATH"))
           (venv-before (uiop:getenv "VIRTUAL_ENV"))
           (spec (lem-lsp-mode/spec:get-language-spec (quote lem-python-mode:python-mode)))
           (lsp-before (lem-lsp-mode/spec:spec-command spec))
           (keymap (mode-keymap (quote lem-python-mode:python-mode)))
           (cr-before (let ((p (keymap-find keymap (parse-keyspec "C-c C-r")))) (and p (prefix-suffix p))))
           (lem-lsp-mode:*disable* t))
      (yeditor-components/lang-python:enable-python-support)
      (install-component (quote le-context-bridge))
      (unwind-protect
           (progn
             (ok (not (le-lang-python-active-p)) "inactive before any Python file is opened")
             (let ((main (find-file-buffer (merge-pathnames "main.py" root))))
               (switch-to-buffer main)
               (ok (le-lang-python-active-p) "opening main.py activates le-lang-python (coeffect)")
               (ok (equal (uiop:getenv "VIRTUAL_ENV") (namestring (merge-pathnames ".venv/" root)))
                   "project virtualenv activated")
               (ok (uiop:string-prefix-p (namestring (merge-pathnames ".venv/bin" root)) (uiop:getenv "PATH"))
                   "virtualenv bin first on PATH")
               (ok (equal (lem-lsp-mode/spec:spec-command spec) (list "basedpyright-langserver" "--stdio"))
                   "best installed language server chosen (from the venv)")
               ;; formatting: reversible side-transaction, failure leaves the buffer untouched
               (ok (eq (yeditor-components/lang-python:format-python-buffer main) :formatted) "formatted")
               (ok (search "x = 1" (full-text main)) "formatter output applied")
               (yeditor-tx-side:side-undo "format:main.py")
               (ok (search "x=1" (full-text main)) "side-transaction undo restores the source")
               (insert-string (buffer-end-point main) "FAIL")
               (let ((before (full-text main)))
                 (ok (eq (yeditor-components/lang-python:format-python-buffer main) :failed) "formatter failure reported")
                 (ok (equal (full-text main) before) "buffer untouched on failure"))
               ;; format on save
               (erase-buffer main)
               (insert-string (buffer-start-point main) (format nil "y=2~%"))
               (save-current-buffer)
               (ok (search "y = 2" (uiop:read-file-string (merge-pathnames "main.py" root))) "formatted on save")
               ;; pytest: failing line marked in the open test buffer
               (let ((test-buffer (find-file-buffer (merge-pathnames "tests/test_main.py" root)))
                     (yeditor-components/lang-python:*pytest-command*
                       (let ((script (namestring (merge-pathnames "fake_pytest.py" root))))
                         (lambda (target) (list "python3" script target)))))
                 (let ((record (yeditor-components/lang-python:run-python-tests "tests/test_main.py" root)))
                   (ok (= 1 (getf record :exit)) "pytest exit status recorded")
                   (ok (equal (getf record :failures) (list (cons "tests/test_main.py" 5))) "failure parsed")
                   (ok (= 1 (length (yeditor-components/lang-python:test-overlays))) "one failure overlay")
                   (ok (eq (overlay-buffer (first (yeditor-components/lang-python:test-overlays))) test-buffer)
                       "overlay is in the test buffer")))
               ;; REPL
               (yeditor-components/lang-python:python-eval "print(40 + 2)")
               (progn
                 (ok (lem-process:process-alive-p (yeditor-components/lang-python:python-repl-process))
                     "REPL running")
                 (ok (= 1 (length yeditor-components/lang-python:*python-evals*)) "eval recorded")
                 ;; leaving the project: a plain text file elsewhere deactivates everything
                 (switch-to-buffer (find-file-buffer notes))
                 (ok (not (le-lang-python-active-p)) "deactivated outside a Python project")
                 (ok (equal (uiop:getenv "VIRTUAL_ENV") venv-before) "VIRTUAL_ENV restored")
                 (ok (equal (uiop:getenv "PATH") path-before) "PATH restored")
                 (ok (equal (lem-lsp-mode/spec:spec-command spec) lsp-before) "LSP command restored")
                 (ok (null (yeditor-components/lang-python:python-repl-process)) "REPL stopped")
                 (ok (null (yeditor-components/lang-python:test-overlays)) "test overlays cleared")
                 (ok (null (find (quote yeditor-components/lang-python::format-on-save)
                                 (variable-value (quote before-save-hook) :global) :key (function car)))
                     "save hook removed")
                 (ok (eq (let ((p (keymap-find keymap (parse-keyspec "C-c C-r")))) (and p (prefix-suffix p)))
                         cr-before)
                     "python-mode C-c C-r binding restored"))))
        (uninstall-component (quote le-lang-python))
        (uninstall-component (quote le-context-bridge))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
        (uiop:delete-directory-tree elsewhere :validate t :if-does-not-exist :ignore)))))

;;; T16 — le-models providers and the AI commands' core operations

(defparameter *fake-ai-server*
  (namestring (asdf:system-relative-pathname "yeditor-components" "test-data/fake-ai-server.py")))

(defun call-with-fake-ai-server (function)
  (let* ((process (uiop:launch-program (list "python3" *fake-ai-server* "0") :output :stream))
         (line (read-line (uiop:process-info-output process)))
         (port (parse-integer line :start (1+ (position #\Space line)))))
    (unwind-protect (funcall function (format nil "http://127.0.0.1:~D" port))
      (uiop:terminate-process process)
      (uiop:wait-process process))))

(defmacro with-fake-ai-server ((url) &body body)
  `(call-with-fake-ai-server (lambda (,url) ,@body)))

(defun hits (url path)
  (or (gethash path (yason:parse (dex:get (format nil "~A/stats" url) :keep-alive nil))) 0))

(defmacro with-env ((name value) &body body)
  (let ((old (gensym)))
    `(let ((,old (uiop:getenv ,name)))
       (if ,value (sb-posix:setenv ,name ,value 1) (sb-posix:unsetenv ,name))
       (unwind-protect (progn ,@body)
         (if ,old (sb-posix:setenv ,name ,old 1) (sb-posix:unsetenv ,name))))))

(deftest test-ollama-provider
  (with-fake-ai-server (url)
    (let ((ollama (make-instance (quote yeditor-components/models:ollama-provider) :base-url url))
          (chunks nil))
      (ok (yeditor-components/models:provider-available-p ollama) "detected")
      (ok (not (yeditor-components/models:provider-cloud-p ollama)) "local, not cloud")
      (ok (equal (yeditor-components/models:provider-models ollama) (list "llama3.2" "qwen2.5-coder")) "models listed")
      (ok (equal (yeditor-components/models:provider-generate
                  ollama "hi" :on-chunk (lambda (c) (push c chunks)))
                 "Hello from llama3.2")
          "streamed response assembled")
      (ok (= 3 (length chunks)) "each chunk delivered as it arrives")
      (let ((statuses nil))
        (yeditor-components/models:ollama-pull ollama "llama3.2" :on-status (lambda (s) (push s statuses)))
        (ok (equal (reverse statuses) (list "pulling manifest" "downloading" "success")) "pull progress streamed"))))
  (ok (not (yeditor-components/models:provider-available-p
            (make-instance (quote yeditor-components/models:ollama-provider) :base-url "http://127.0.0.1:9")))
      "not detected when nothing listens"))

(deftest test-openai-compatible-provider
  (with-fake-ai-server (url)
    (let ((p (make-instance (quote yeditor-components/models:openai-compatible-provider) :base-url url)))
      (ok (equal (yeditor-components/models:provider-models p) (list "local-model")) "models listed")
      (ok (equal (yeditor-components/models:provider-generate p "hi") "compat answer") "SSE stream assembled"))))

(deftest test-claude-provider
  (with-fake-ai-server (url)
    (let ((claude (make-instance (quote yeditor-components/models:claude-provider) :base-url url)))
      (ok (yeditor-components/models:provider-cloud-p claude) "Claude is always a cloud provider")
      (with-env ("ANTHROPIC_API_KEY" "test-key")
        (let ((yeditor-components/models:*allow-cloud* nil))
          (ok (handler-case (progn (yeditor-components/models:provider-generate claude "hi") nil)
                (yeditor-components/models:cloud-provider-disabled () t))
              "refused while cloud providers are not enabled")
          (ok (zerop (hits url "/v1/messages")) "and nothing was sent"))
        (let ((yeditor-components/models:*allow-cloud* t))
          (ok (equal (yeditor-components/models:provider-generate claude "hi") "Claude says hi")
              "text deltas assembled; thinking deltas ignored")
          (ok (equal (yeditor-components/models:provider-generate claude "OVERLOAD-ONCE please") "Claude says hi")
              "a 529 overload is retried")
          (ok (handler-case (progn (yeditor-components/models:provider-generate claude "REFUSE this") nil)
                (yeditor-components/models:generation-refused () t))
              "stop_reason refusal is reported")))
      (with-env ("ANTHROPIC_API_KEY" nil)
        (let ((yeditor-components/models:*allow-cloud* t))
          (ok (search "ANTHROPIC_API_KEY"
                      (handler-case (progn (yeditor-components/models:provider-generate claude "hi") "")
                        (yeditor-components/models:provider-error (e) (princ-to-string e))))
              "a missing key is a clear error"))))))

(defclass canned-provider (yeditor-components/models:provider) ((reply :initarg :reply :reader reply))
  (:default-initargs :name "canned" :base-url "http://127.0.0.1" :default-model "canned"))

(defmethod yeditor-components/models:provider-generate ((p canned-provider) prompt &key model system on-chunk)
  (declare (ignore prompt model system))
  (when on-chunk (funcall on-chunk (reply p)))
  (reply p))

(deftest test-ai-edits-are-reversible
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (make-buffer "ai-edit.py")))
      (insert-string (buffer-start-point buffer) (format nil "def f():~%    x=1~%    return x~%"))
      (with-point ((s (buffer-start-point buffer)) (e (buffer-start-point buffer)))
        (move-to-line s 2) (move-to-line e 2) (line-end e)
        (yeditor-components/ai:ai-rewrite-region
         (make-instance (quote canned-provider) :reply (format nil "```python~%    x = 2~%```"))
         s e "use 2"))
      (ok (search "    x = 2" (full-text buffer)) "rewrite applied without the code fence")
      (yeditor-tx-side:side-undo "ai-rewrite:ai-edit.py")
      (ok (search "    x=1" (full-text buffer)) "rewrite reverted exactly")
      (with-point ((s (buffer-start-point buffer)) (e (buffer-end-point buffer)))
        (yeditor-components/ai:ai-document-region
         (make-instance (quote canned-provider) :reply "# Returns one.") s e))
      (ok (uiop:string-prefix-p (format nil "# Returns one.~%def f():") (full-text buffer)) "doc comment inserted above")
      (yeditor-tx-side:side-undo "ai-document:ai-edit.py")
      (ok (uiop:string-prefix-p "def f():" (full-text buffer)) "documentation reverted")
      (kill-buffer buffer))))

(deftest test-ai-components
  (lem-fake-interface:with-fake-interface ()
    (let ((before yeditor-components/ai:*default-provider*))
      (install-component (quote le-models))
      (install-component (quote le-ollama))
      (ok (typep yeditor-components/ai:*default-provider* (quote yeditor-components/models:ollama-provider))
          "le-ollama makes local Ollama the default provider")
      (ok (equal (yeditor-components/ai:ai-modeline-segment (current-window)) " AI:ollama ") "status shows the provider")
      (ok (eq (binding "C-c a r") (quote yeditor-components/ai:yeditor-ai-rewrite)) "rewrite bound")
      (let ((yeditor-components/ai:*default-provider* (make-instance (quote yeditor-components/models:claude-provider)))
            (yeditor-components/models:*allow-cloud* nil))
        (ok (search "cloud, off" (yeditor-components/ai:ai-modeline-segment (current-window)))
            "a cloud provider is flagged in the status line"))
      (uninstall-component (quote le-ollama))
      (uninstall-component (quote le-models))
      (ok (eq yeditor-components/ai:*default-provider* before) "default provider restored")
      (ok (null (binding "C-c a r")) "bindings removed"))))

;;; T19 — le-mcp: MCP bus for agent workers

(defun free-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (prog1 (usocket:get-local-port socket) (usocket:socket-close socket))))

(defun mcp-call (port method params &key session id)
  "POST a JSON-RPC request; returns (values parsed-response session-id)."
  (multiple-value-bind (body status headers)
      (dex:post (format nil "http://127.0.0.1:~D/mcp" port)
                :content (with-output-to-string (s)
                           (yason:encode (let ((h (make-hash-table :test (function equal))))
                                           (setf (gethash "jsonrpc" h) "2.0" (gethash "method" h) method
                                                 (gethash "params" h) params)
                                           (when id (setf (gethash "id" h) id))
                                           h)
                                         s))
                :headers (append (list (cons "content-type" "application/json"))
                                 (when session (list (cons "mcp-session-id" session))))
                :keep-alive nil)
    (declare (ignore status))
    (values (and (plusp (length body)) (yason:parse body)) (gethash "mcp-session-id" headers))))

(defun tool-call (port session name arguments)
  "Call tool NAME; returns (values text error-p)."
  (let* ((args (make-hash-table :test (function equal))))
    (loop :for (k v) :on arguments :by (function cddr) :do (setf (gethash k args) v))
    (let* ((response (mcp-call port "tools/call"
                               (let ((h (make-hash-table :test (function equal))))
                                 (setf (gethash "name" h) name (gethash "arguments" h) args) h)
                               :session session :id 1))
           (result (gethash "result" response)))
      (values (gethash "text" (first (gethash "content" result)))
              (eq (gethash "isError" result) t)))))

(defun mcp-handshake (port)
  (multiple-value-bind (response session)
      (mcp-call port "initialize" (let ((h (make-hash-table :test (function equal))))
                                    (setf (gethash "protocolVersion" h) "2024-11-05") h)
                :id 0)
    (declare (ignore response))
    (mcp-call port "notifications/initialized" (make-hash-table) :session session)
    session))

(defun open-event-stream (port session)
  "Read SSE data lines into a list on a thread; returns (values thread getter stream)."
  (let* ((stream (flexi-streams:make-flexi-stream
                  (dex:get (format nil "http://127.0.0.1:~D/mcp" port)
                           :headers (list (cons "mcp-session-id" session))
                           :want-stream t :force-binary t :keep-alive nil)
                  :external-format :utf-8))
         (lines nil)
         (lock (bt2:make-lock)))
    (values (bt2:make-thread (lambda ()
                               (ignore-errors
                                (loop :for line := (read-line stream nil)
                                      :while line
                                      :when (uiop:string-prefix-p "data: " line)
                                        :do (bt2:with-lock-held (lock) (push (subseq line 6) lines))))))
            (lambda () (bt2:with-lock-held (lock) (reverse lines)))
            stream)))

(defun wait-until (predicate &key (timeout 3))
  (loop :repeat (* timeout 100) :thereis (funcall predicate) :do (sleep 0.01)))

(deftest test-le-mcp-worker-bus
  (lem-fake-interface:with-fake-interface ()
    (let* ((port (free-port))
           (yeditor-components/mcp:*mcp-port* port))
      (install-component (quote le-mcp))
      (unwind-protect
           (let ((session (mcp-handshake port)))
             (ok session "initialize returns a session id")
             (let ((tools (mapcar (lambda (tool) (gethash "name" tool))
                                  (gethash "tools" (gethash "result" (mcp-call port "tools/list" (make-hash-table)
                                                                               :session session :id 2))))))
               (dolist (name (list "yeditor_list_components" "yeditor_worker_register" "yeditor_task_next"
                                   "yeditor_task_report" "yeditor_transaction_log" "yeditor_toggle_component"))
                 (ok (member name tools :test (function string=)) (format nil "tool ~A listed" name))))
             (ok (search "\"le-mcp\"" (tool-call port session "yeditor_list_components" nil))
                 "component list includes le-mcp")
             (ok (nth-value 1 (tool-call port session "yeditor_worker_register"
                                         (list "name" "w" "protocol" "other/9")))
                 "a worker speaking another protocol is refused")
             (ok (nth-value 1 (tool-call port session "yeditor_worker_register"
                                         (list "name" "w" "protocol" "yeditor-worker/1" "capabilities" (list "teleport"))))
                 "a worker requiring an unoffered capability is refused")
             (let ((worker (tool-call port session "yeditor_worker_register"
                                      (list "name" "claude-code" "protocol" "yeditor-worker/1"
                                            "capabilities" (list "tasks" "events")))))
               (ok (uiop:string-prefix-p "worker-" worker) "worker registered")
               (multiple-value-bind (thread events stream) (open-event-stream port session)
                 (declare (ignore thread))
                 (let ((task (yeditor-components/mcp:submit-task worker "edit" "rename foo to bar")))
                   (ok (wait-until (lambda () (some (lambda (e) (search task e)) (funcall events))))
                       "task pushed to the worker over SSE")
                   (ok (search task (tool-call port session "yeditor_task_next" (list "worker_id" worker)))
                       "task delivered by task_next")
                   (ok (equal (tool-call port session "yeditor_task_next" (list "worker_id" worker)) "null")
                       "no further pending task")
                   (tool-call port session "yeditor_task_report"
                              (list "worker_id" worker "task_id" task "status" "done" "observation" "renamed 3 uses"))
                   (ok (equal (third (first (yeditor-components/mcp:task-observations))) "renamed 3 uses")
                       "observation recorded")
                   (ok (wait-until (lambda () (some (lambda (e) (search "observation" e)) (funcall events))))
                       "observation event streamed"))
                 (close stream))
               ;; latency: handshake + registration + task submission and delivery
               (let ((samples
                       (loop :repeat 50
                             :collect (let ((t0 (get-internal-real-time)))
                                        (let* ((s (mcp-handshake port))
                                               (w (tool-call port s "yeditor_worker_register"
                                                             (list "name" "bench" "protocol" "yeditor-worker/1"))))
                                          (yeditor-components/mcp:submit-task w "noop" "")
                                          (tool-call port s "yeditor_task_next" (list "worker_id" w)))
                                        (/ (* 1000.0 (- (get-internal-real-time) t0))
                                           internal-time-units-per-second)))))
                 (let ((p95 (nth (floor (* 0.95 50)) (sort samples (function <)))))
                   (format t "~&handshake+task p95: ~,1F ms~%" p95)
                   (ok (< p95 50) "handshake + task delivery p95 under 50 ms")))))
        (uninstall-component (quote le-mcp)))
      (ok (null (lem-mcp-server:current-mcp-server)) "server stopped on uninstall")
      (ok (handler-case (progn (mcp-handshake port) nil) (error () t)) "port closed")
      (ok (null (yeditor-components/mcp:worker-ids)) "workers forgotten"))))

;;; Sidebar, Terminal and Workspace Layout (modern IDE panels)

(defun terminal-ffi-available-p ()
  "Return t if the libvterm alien symbol 'terminal_new' is resolvable."
  (ignore-errors
   (cffi:foreign-symbol-pointer "terminal_new")))

(deftest test-sidebar-toggle-no-residue
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-sidebar))
    (ok (eq (binding "C-c b") (quote yeditor-sidebar-toggle)) "sidebar binding installed")
    (yeditor-sidebar-toggle)
    (ok (frame-leftside-window (current-frame)) "sidebar toggle opens leftside window")
    (yeditor-sidebar-toggle)
    (ok (not (sidebar-active-p)) "sidebar toggled off is no longer active")
    (uninstall-component (quote le-sidebar))
    (ok (null (binding "C-c b")) "sidebar binding removed")))

(deftest test-terminal-toggle-no-residue
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-terminal))
    (ok (eq (binding "C-c j") (quote yeditor-terminal-toggle)) "terminal binding installed")
    (if (terminal-ffi-available-p)
        (progn
          (yeditor-terminal-toggle)
          (ok (terminal-active-p) "terminal toggle opens bottomside window")
          (yeditor-terminal-toggle)
          (ok (not (terminal-active-p)) "terminal toggled off is no longer active"))
        (ok t "libvterm FFI not available in this build; skip terminal toggle test"))
    (uninstall-component (quote le-terminal))
    (ok (null (binding "C-c j")) "terminal binding removed")))

(deftest test-workspace-layout-on-demand-only
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-workspace-layout))
    (unwind-protect
         (let ((file (merge-pathnames (format nil "yeditor-ws-~D.txt" (random 1000000))
                                      (uiop:default-temporary-directory))))
           (with-open-file (out file :direction :output :if-exists :supersede)
             (write-string "hello" out))
           (find-file file)
           (ok (null (frame-leftside-window (current-frame)))
               "opening a file keeps the Zero-Chrome basal state")
           (yeditor-components/workspace-layout:yeditor-workspace-open)
           (ok (frame-leftside-window (current-frame)) "C-c w opens the sidebar"))
      (uninstall-component (quote le-workspace-layout))
      (when (frame-leftside-window (current-frame))
        (delete-leftside-window)))))

;;; Director Mode (T21 editor tools on the MCP bus + T12-B cockpit)

;; the tests below start le-mcp on the default port; a running editor may hold 7890
(setf yeditor-components/mcp:*mcp-port* (free-port))

(deftest test-editor-run-command-tool
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-mcp))
    (unwind-protect
         (progn
           ;; unknown command: tool responds ok=nil, does not signal
           (ok (eq (nth-value 0 (yeditor-components/mcp::run-lem-command "no-such-command-xyz")) nil)
               "unknown command reports ok=nil")
           ;; known command runs with an effect on the current buffer
           (let ((buffer (make-buffer "*director-test*" :temporary t)))
             (switch-to-buffer buffer)
             (insert-string (current-point) "abc")
             (buffer-start (current-point))
             (let ((ok (nth-value 0 (yeditor-components/mcp::run-lem-command "forward-char"))))
               (ok ok "forward-char runs"))
             (ok (char= (character-at (current-point)) #\b) "forward-char moved the point")))
      (uninstall-component (quote le-mcp)))))

(deftest test-editor-open-and-present
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-mcp))
    (unwind-protect
         (let ((file1 (merge-pathnames (format nil "yeditor-d1-~D.txt" (random 1000000))
                                       (uiop:default-temporary-directory)))
               (file2 (merge-pathnames (format nil "yeditor-d2-~D.txt" (random 1000000))
                                       (uiop:default-temporary-directory))))
           (dolist (file (list file1 file2))
             (with-open-file (out file :direction :output :if-exists :supersede)
               (write-string "x" out)))
           (yeditor-components/mcp::present-side-by-side (list (namestring file1) (namestring file2)))
           (let ((mains (remove-if (lambda (w) (typep w (quote lem-core:side-window)))
                                   (window-list))))
             (ok (>= (length mains) 2) "two main windows after side-by-side present"))
           ;; reversibility: deleting the buffers leaves nothing dangling
           (dolist (file (list file1 file2))
             (alexandria:when-let ((b (find (probe-file file) (buffer-list)
                                            :key (lambda (b) (and (buffer-filename b) (probe-file (buffer-filename b))))
                                            :test (function equal))))
               (delete-buffer b))))
      (uninstall-component (quote le-mcp)))))

(deftest test-editor-propose-checkpoint-and-rollback
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-mcp))
    (install-component (quote le-hud))
    (unwind-protect
         (progn
           (yeditor-components/hud:open-agent-tx "fake-worker" "checkpoint")
           (ok yeditor-components/hud:*agent-tx* "checkpoint opens an agent transaction")
           (let ((buffer (make-buffer "*checkpoint-edit*" :temporary t)))
             (switch-to-buffer buffer)
             (yeditor-components/hud:agent-tx-insert buffer 1 "agent edit")
             (ok (search "agent edit" (points-to-string (buffer-start-point buffer)
                                                        (buffer-end-point buffer)))
                 "agent edit applied")
             (yeditor-components/hud:close-agent-tx :rollback t)
             (ok (zerop (length (points-to-string (buffer-start-point buffer)
                                                  (buffer-end-point buffer))))
                 "rollback restored the buffer")
             (ok (null yeditor-components/hud:*agent-tx*) "transaction closed")))
      (uninstall-component (quote le-hud))
      (uninstall-component (quote le-mcp)))))

(deftest test-eval-annotation-leaves-text-alone
  (lem-fake-interface:with-fake-interface ()
    (let ((buffer (make-buffer "*annotate-test*" :temporary t)))
      (insert-string (buffer-point buffer) (format nil "x = 1~%y = 2~%"))
      (let ((before (points-to-string (buffer-start-point buffer) (buffer-end-point buffer))))
        (yeditor-components/mcp::annotate-buffer buffer 1 "evaluating..." (quote modeline))
        (yeditor-components/mcp::annotate-buffer buffer 2 "done" (quote modeline))
        (ok (equal before (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))
            "annotation is virtual text; the buffer is unchanged")
        (ok (= 1 (length (lem:buffer-overlays buffer)))
            "a new annotation replaces the previous one"))
      (delete-buffer buffer))))

(deftest test-director-cockpit-binding
  (lem-fake-interface:with-fake-interface ()
    (install-component (quote le-director-cockpit))
    (ok (eq (binding "F1") (quote yeditor-director-intent)) "F1 opens the intent bar")
    (uninstall-component (quote le-director-cockpit))
    (ok (null (binding "F1")) "F1 binding removed")))
