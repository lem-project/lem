;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(load ".qlot/setup.lisp")

(require :asdf)

(asdf:load-asd (merge-pathnames "yeditor/core/tx/yeditor-tx.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/core/tx/yeditor-tx-tests.asd" (uiop:getcwd)))
(asdf:load-system :yeditor-tx)

(asdf:test-system :yeditor-tx-tests)
(quit)