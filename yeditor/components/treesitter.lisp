;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/treesitter
  (:use :cl :lem :yeditor-context :yeditor-effects)
  (:export :le-treesitter-packs
           :*parsers-directory*
           :*packs*
           :pack-query-path
           :grammar-library-path
           :compile-grammar
           :treesitter-install-grammar))
(in-package :yeditor-components/treesitter)

(defvar *parsers-directory* (uiop:xdg-data-home "yeditor/parsers/")
  "Where treesitter-install-grammar puts compiled grammar libraries.")

(defparameter *packs*
  (list (list :language "python" :mode (quote lem-python-mode:python-mode)))
  "Language packs: tree-sitter language name and the Lem major mode it serves.
   Queries live in treesitter/<language>/{highlights,indents}.scm.")

(defun pack-query-path (language file)
  (asdf:system-relative-pathname "yeditor-components" (format nil "treesitter/~A/~A" language file)))

(defun grammar-library-path (language)
  "The grammar compiled by treesitter-install-grammar, or nil to use the system library path."
  (let ((path (merge-pathnames (format nil "libtree-sitter-~A.so" language) *parsers-directory*)))
    (and (probe-file path) path)))

(defun grammar-available-p (language)
  (and (lem-tree-sitter:tree-sitter-available-p)
       (ignore-errors (ensure-grammar language (grammar-library-path language)))
       t))

(define-component le-treesitter-packs
  :version "0.1.0"
  :install (lambda (comp ctx)
             (declare (ignore ctx))
             (loop :for pack :in *packs*
                   :for language := (getf pack :language)
                   :if (grammar-available-p language)
                     :collect (install-treesitter-grammar
                               comp (mode-syntax-table (getf pack :mode)) language
                               (pack-query-path language "highlights.scm")
                               :indents-path (pack-query-path language "indents.scm")
                               :library-path (grammar-library-path language))
                   :else
                     :do (message "tree-sitter grammar for ~A not installed (M-x treesitter-install-grammar)"
                                  language))))

;;; Grammar installation

(defun run (command &key directory)
  (multiple-value-bind (out err code)
      (uiop:run-program command :directory directory :output :string :error-output :string
                                :ignore-error-status t)
    (declare (ignore out))
    (unless (zerop code)
      (editor-error "~{~A~^ ~} failed: ~A" command err))))

(defun compile-grammar (source-directory language &key (output-directory *parsers-directory*))
  "Compile the tree-sitter grammar in SOURCE-DIRECTORY (src/parser.c plus an
   optional src/scanner.c or src/scanner.cc) into
   OUTPUT-DIRECTORY/libtree-sitter-LANGUAGE.so. Returns the library path."
  (let* ((source (uiop:ensure-directory-pathname source-directory))
         (src (merge-pathnames "src/" source))
         (output (merge-pathnames (format nil "libtree-sitter-~A.so" language)
                                  (uiop:ensure-directory-pathname output-directory)))
         (scanner-c (probe-file (merge-pathnames "scanner.c" src)))
         (scanner-cc (probe-file (merge-pathnames "scanner.cc" src))))
    (unless (probe-file (merge-pathnames "parser.c" src))
      (editor-error "~A has no src/parser.c" source))
    (ensure-directories-exist output)
    (run (append (list (if scanner-cc "c++" "cc") "-shared" "-fPIC" "-O2" "-I" (namestring src)
                       "-o" (namestring output) (namestring (merge-pathnames "parser.c" src)))
                 (when scanner-c (list (namestring scanner-c)))
                 (when scanner-cc (list (namestring scanner-cc)))))
    output))

(define-command treesitter-install-grammar (language) ((:string "Language: "))
  "Download (git clone) and compile the tree-sitter grammar for LANGUAGE into
~/.local/share/yeditor/parsers/."
  (let* ((url (prompt-for-string "Grammar repository: "
                                 :initial-value (format nil "https://github.com/tree-sitter/tree-sitter-~A" language)))
         (checkout (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "yeditor-grammar-~A-~D/" language (random 1000000))
                                     (uiop:default-temporary-directory)))))
    (unwind-protect
         (progn
           (run (list "git" "clone" "--depth" "1" url (namestring checkout)))
           (message "Installed ~A" (compile-grammar checkout language)))
      (uiop:delete-directory-tree checkout :validate t :if-does-not-exist :ignore))))
