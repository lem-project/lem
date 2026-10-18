;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(load ".qlot/setup.lisp")
(require :asdf)
(push #p"./yeditor/context/" asdf:*central-registry*)
(push #p"./yeditor/tx/" asdf:*central-registry*)
(asdf:load-asd (merge-pathnames "yeditor/context/yeditor-context.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/context/yeditor-context-loader.asd" (uiop:getcwd)))
(asdf:load-asd (merge-pathnames "yeditor/context/loader-tests.asd" (uiop:getcwd)))
(asdf:load-system :loader-tests)
(rove:run :loader-tests)
(uiop:quit)
