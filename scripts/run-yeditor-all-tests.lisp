;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

;;; Run every Yeditor test suite; exit 1 if any fails.
(load ".qlot/setup.lisp")
(require :asdf)
(dolist (asd '("tx/yeditor-tx.asd" "tx/tx-tests.asd"
               "context/yeditor-context.asd" "context/registry-tests.asd"
               "context/yeditor-context-reactivity.asd" "context/reactivity-tests.asd"
               "context/yeditor-context-loader.asd" "context/loader-tests.asd"
               "context/yeditor-context-durability.asd" "context/durability-tests.asd"
               "effects/effects.asd" "effects/effects-tests.asd"
               "effects/effects-t5-tests.asd" "effects/effects-t6-tests.asd"))
  (asdf:load-asd (merge-pathnames (concatenate 'string "yeditor/" asd) (uiop:getcwd))))
(defparameter *systems*
  '(:yeditor-tx-tests :registry-tests :yeditor-context-reactivity-tests :loader-tests :durability-tests
    :yeditor-effects-tests :yeditor-effects-t5-tests :yeditor-effects-t6-tests))
(dolist (s *systems*) (asdf:load-system s))
(uiop:quit (if (every #'rove:run *systems*) 0 1))
