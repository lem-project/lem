(defsystem "lem-vue-mode"
  :depends-on ("lem"
               "lem-lsp-mode"
               "lem-js-mode")
  :serial t
  :components ((:file "vue-mode")
               (:file "lsp-config")))

