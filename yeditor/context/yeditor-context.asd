;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

(defsystem "yeditor-context"
  :description "Component registry and lifecycle for Yeditor (L2)"
  :author "Yuri Flores Albuquerque <yuri-albuquerque@users.noreply.github.com>"
  :license "LicenseRef-Yeditor-Proprietary"
  :version "0.1.0"
  :depends-on ("yeditor-tx")
  :components ((:file "registry")))
