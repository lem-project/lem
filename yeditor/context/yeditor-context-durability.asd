;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-context-durability"
  :description "T9 durability: JSONL effects log, boot replay and crash recovery"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "LicenseRef-Yeditor-Proprietary"
  :version "0.1.0"
  :depends-on ("yeditor-tx" "yeditor-context" "yason")
  :components ((:file "durability")))
