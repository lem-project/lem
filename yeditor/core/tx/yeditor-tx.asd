;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defsystem "yeditor-tx"
  :license "FSL-1.1-MIT"
  :description "Transactional core for Yeditor"
  :depends-on ()
  :components ((:file "tx")
               (:file "context")
               (:file "side")))
