;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

;;; Run every Yeditor test suite; exit 1 if any fails.
(load ".qlot/setup.lisp")
(require :asdf)
(dolist (asd '("core/tx/yeditor-tx.asd" "core/tx/yeditor-tx-tests.asd"
               "core/context/yeditor-context.asd" "core/context/registry-tests.asd"
               "core/context/yeditor-context-reactivity.asd" "core/context/yeditor-context-reactivity-tests.asd"
               "core/context/yeditor-context-loader.asd" "core/context/loader-tests.asd"
               "core/context/yeditor-context-durability.asd" "core/context/durability-tests.asd"
               "effects/yeditor-effects.asd" "effects/yeditor-effects-tests.asd"
               "effects/yeditor-effects-t5-tests.asd" "effects/yeditor-effects-t6-tests.asd"))
  (asdf:load-asd (merge-pathnames (concatenate 'string "yeditor/" asd) (uiop:getcwd))))
(defparameter *systems*
  '(:yeditor-tx-tests :registry-tests :yeditor-context-reactivity-tests :loader-tests :durability-tests
    :yeditor-effects-tests :yeditor-effects-t5-tests :yeditor-effects-t6-tests :yeditor-components-tests))
(dolist (s *systems*) (asdf:load-system s))
(uiop:quit (if (every #'rove:run *systems*) 0 1))
