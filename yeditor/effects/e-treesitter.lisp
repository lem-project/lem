;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-effects/e-treesitter
  (:use :cl :lem :yeditor-tx)
  (:export :install-treesitter-grammar :ensure-grammar))
(in-package :yeditor-effects/e-treesitter)

(defun ensure-grammar (language &optional library-path)
  "Register the tree-sitter LANGUAGE, loading its grammar library once (from
   LIBRARY-PATH, or the system library path). Returns the language object.
   A registered language is reused, so repeated installs never load the library again."
  (or (tree-sitter:get-language language)
      (if library-path
          (tree-sitter:load-language language (namestring library-path))
          (tree-sitter:load-language-from-system language))))

(defun rescan-buffers (syntax-table)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-syntax-table buffer) syntax-table)
      (syntax-scan-region (buffer-start-point buffer) (buffer-end-point buffer)))))

(defun install-treesitter-grammar (comp syntax-table language highlights-path
                                   &key indents-path library-path)
  "Attach a tree-sitter parser for LANGUAGE (with HIGHLIGHTS-PATH and optional
   INDENTS-PATH queries) to SYNTAX-TABLE as a transaction effect (kind 16, Cap. 5.3).
   Undo restores the parser the syntax table had before (the regex/fallback
   highlighting) and rescans its buffers.
   The grammar library stays loaded and registered after undo: unloading a shared
   library that tree-sitter objects awaiting finalization may still reference is
   unsafe. ENSURE-GRAMMAR reuses it, so toggling never loads it again."
  (let ((previous nil))
    (let ((handle (install-effect comp :treesitter-grammar
                                  (lambda ()
                                    (unless (lem-tree-sitter:tree-sitter-available-p)
                                      (error "The tree-sitter library is not available"))
                                    (ensure-grammar language library-path)
                                    (setf previous (syntax-table-parser syntax-table))
                                    (unless (lem-tree-sitter:enable-tree-sitter-for-mode
                                             syntax-table language highlights-path
                                             :indent-query-path indents-path)
                                      (error "Could not enable tree-sitter for ~A" language))
                                    (rescan-buffers syntax-table))
                                  :undo-thunk
                                  (lambda ()
                                    (set-syntax-parser syntax-table previous)
                                    (rescan-buffers syntax-table)))))
      (push (cons :language language) handle)
      handle)))
