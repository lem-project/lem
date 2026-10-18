;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defpackage :yeditor-effects/e-hook
  (:use :cl :lem :yeditor-tx :yeditor-context)
  (:export :install-hook))
(in-package :yeditor-effects/e-hook)

(defun install-hook (comp hook-var fn)
  "Add FN to the hook list stored in HOOK-VAR (a symbol) as a transaction effect.
   Undo removes FN. add-hook and remove-hook are macros over a place, so the
   place is (symbol-value HOOK-VAR)."
  (let ((handle (install-effect comp :hook
                                 (lambda () (add-hook (symbol-value hook-var) fn))
                                 :undo-thunk
                                 (lambda () (remove-hook (symbol-value hook-var) fn)))))
    (push (cons :hook hook-var) handle)
    (push (cons :fn fn) handle)
    handle))
