;;; Run T5 effects tests (keymap, variable, command, overlay, timer).
(require :asdf)
(push #p"./yeditor/effects/" asdf:*central-registry*)
(push #p"./yeditor/tx/"      asdf:*central-registry*)
(push #p"./yeditor/context/" asdf:*central-registry*)
(asdf:test-system "yeditor-effects-t5-tests")
