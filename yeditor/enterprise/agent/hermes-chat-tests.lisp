;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Enterprise
;;;; Proprietary and Confidential — Yeditor Agentic & Forge Suite (see LICENSE-ENTERPRISE).

(defpackage :yeditor-enterprise-tests/hermes-chat
  (:use :cl :rove :yeditor-context))
(in-package :yeditor-enterprise-tests/hermes-chat)

(deftest test-hermes-chat-dispatcher-is-reversible
  (lem-fake-interface:with-fake-interface ()
    (let ((before yeditor-components/director-cockpit:*intent-dispatcher*))
      (install-component (quote yeditor-enterprise/hermes-chat:le-hermes-chat))
      (ok (functionp yeditor-components/director-cockpit:*intent-dispatcher*)
          "F1 intents go to Hermes while the chat is installed")
      (uninstall-component (quote yeditor-enterprise/hermes-chat:le-hermes-chat))
      (ok (eq before yeditor-components/director-cockpit:*intent-dispatcher*)
          "uninstall restores the previous dispatcher")
      (let ((keys nil))
        (lem:traverse-keymap yeditor-enterprise/hermes-chat::*hermes-chat-keymap*
                             (lambda (sequence command)
                               (declare (ignore command))
                               (push (format nil "~{~A~^ ~}" sequence) keys)))
        (ok (not (member "C-c C-a" keys :test (function string=)))
            "C-c C-a is left to the HUD commit binding")
        (ok (member "C-c C-e" keys :test (function string=)) "C-c C-e returns to the editor")))))
