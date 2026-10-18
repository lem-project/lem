;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(load ".qlot/setup.lisp")
(asdf:load-asd (merge-pathnames "yeditor/core/context/yeditor-context.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/core/context/yeditor-context-reactivity.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/core/context/yeditor-context-reactivity-tests.asd" (uiop:getcwd)))
(asdf:load-system :yeditor-context-reactivity-tests)
(rove:run :yeditor-context-reactivity-tests)
(uiop:quit)
