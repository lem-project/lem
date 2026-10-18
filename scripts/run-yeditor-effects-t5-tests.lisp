;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

;;; Run T5 effects tests (keymap, variable, command, overlay, timer).
(require :asdf)
(push #p"./yeditor/effects/" asdf:*central-registry*)
(push #p"./yeditor/tx/"      asdf:*central-registry*)
(push #p"./yeditor/context/" asdf:*central-registry*)
(asdf:test-system "yeditor-effects-t5-tests")
