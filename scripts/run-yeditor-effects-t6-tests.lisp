;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

;;; Run T6 effects tests (process, fdefinition — integration).
;;; Requires python3 in PATH for the process test.
(require :asdf)
(push #p"./yeditor/effects/" asdf:*central-registry*)
(push #p"./yeditor/tx/"      asdf:*central-registry*)
(push #p"./yeditor/context/" asdf:*central-registry*)
(asdf:test-system "yeditor-effects-t6-tests")
