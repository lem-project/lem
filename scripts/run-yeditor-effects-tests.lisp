;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.

(load ".qlot/setup.lisp")
(require :asdf)
(defparameter *effects-dir* (merge-pathnames "yeditor/effects/" (uiop:getcwd)))
(dolist (asd '("yeditor-effects.asd" "yeditor-effects-tests.asd" "yeditor-effects-t5-tests.asd" "yeditor-effects-t6-tests.asd"))
  (asdf:load-asd (merge-pathnames asd *effects-dir*)))
(asdf:load-system :yeditor-effects-tests)
(asdf:load-system :yeditor-effects-t5-tests)
(asdf:load-system :yeditor-effects-t6-tests)
(dolist (sys '(:yeditor-effects-tests :yeditor-effects-t5-tests :yeditor-effects-t6-tests))
  (rove:run sys))
(uiop:quit)
