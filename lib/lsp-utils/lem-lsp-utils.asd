(defsystem "lem-lsp-utils"
  :class :package-inferred-system
  :depends-on ("lem-lsp-utils/json"
               "lem-lsp-utils/type"
               "lem-lsp-utils/json-lsp-utils"
               #+(or)"lem-lsp-utils/protocol-generator"
               "lem-lsp-utils/protocol"
               "lem-lsp-utils/uri"))
