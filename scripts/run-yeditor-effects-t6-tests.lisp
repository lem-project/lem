;;; Run T6 effects tests (process, fdefinition — integration).
;;; Requires python3 in PATH for the process test.
(require :asdf)
(push #p"./yeditor/effects/" asdf:*central-registry*)
(push #p"./yeditor/tx/"      asdf:*central-registry*)
(push #p"./yeditor/context/" asdf:*central-registry*)
(asdf:test-system "yeditor-effects-t6-tests")
