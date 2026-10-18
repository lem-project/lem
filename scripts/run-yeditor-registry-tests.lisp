;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(load ".qlot/setup.lisp")
(require :asdf)
(asdf:load-asd (merge-pathnames "yeditor/tx/yeditor-tx.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/context/yeditor-context.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/context/registry-tests.asd" (uiop:getcwd)))
(asdf:load-system :registry-tests)
(asdf:test-system :registry-tests)
(quit)
