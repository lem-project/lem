;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(load ".qlot/setup.lisp")
(require :asdf)
(push #p"./yeditor/core/context/" asdf:*central-registry*)
(push #p"./yeditor/core/tx/" asdf:*central-registry*)
(asdf:load-asd (merge-pathnames "yeditor/core/context/yeditor-context.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/core/context/yeditor-context-loader.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/core/context/loader-tests.asd" (uiop:getcwd)))
(asdf:load-system :loader-tests)
(rove:run :loader-tests)
(uiop:quit)
