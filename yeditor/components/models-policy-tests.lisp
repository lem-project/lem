;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(defpackage :yeditor-components/models-policy-tests
  (:use :cl :rove :yeditor-components/models))
(in-package :yeditor-components/models-policy-tests)

(deftest remote-metadata-and-downloads-require-opt-in
  (let ((*allow-cloud* nil)
        (yeditor-tx:*tx-pending* nil) (yeditor-tx:*tx-committed* nil)
        (requests nil))
    (yeditor-effects:install-fdefinition 'policy 'dex:get
      (lambda (&rest args) (push args requests) "{\"models\":[],\"data\":[]}"))
    (yeditor-effects:install-fdefinition 'policy 'dex:post
      (lambda (&rest args)
        (push args requests)
        (flexi-streams:make-in-memory-input-stream
         (flexi-streams:string-to-octets (format nil "{\"status\":\"done\"}~%") :external-format :utf-8))))
    (yeditor-tx:tx-commit)
    (unwind-protect
         (dolist (type '(ollama-provider openai-compatible-provider))
           (let ((provider (make-instance type :base-url "https://example.invalid")))
             (ok (signals (provider-models provider) 'cloud-provider-disabled))
             (ok (null (provider-available-p provider)))
             (when (eq type 'ollama-provider)
               (ok (signals (ollama-pull provider "test-model") 'cloud-provider-disabled)))
             (ok (null requests) "no HTTP before consent")
             (let ((*allow-cloud* t))
               (ok (null (provider-models provider)))
               (ok requests "explicit consent permits the request"))
             (setf requests nil)))
      (yeditor-tx:tx-rollback))))

(deftest ai-palette-restores-its-source-100-times
  (lem-fake-interface:with-fake-interface ()
    (let ((before yeditor-components/palette:*palette-actions*))
      (dotimes (i 100)
        (yeditor-context:install-component 'yeditor-components/ai:le-models)
        (dolist (label '("AI: chat" "AI: explain selection" "AI: rewrite selection"
                         "AI: document selection" "AI: download model" "AI: toggle cloud access"))
          (ok (assoc label yeditor-components/palette:*palette-actions* :test #'equal)))
        (yeditor-context:uninstall-component 'yeditor-components/ai:le-models)
        (ok (eq before yeditor-components/palette:*palette-actions*))))))
