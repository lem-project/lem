;;; Run T7 reactivity tests (coeffects, context-event dispatch, work-cycle predicates).
(require :asdf)
(push #p"./yeditor/context/" asdf:*central-registry*)
(push #p"./yeditor/tx/"      asdf:*central-registry*)
(asdf:test-system "yeditor-context-reactivity-tests")
