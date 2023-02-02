(lem-lsp-server/defpackage:defpackage :lem-lsp-server/test/initialize
  (:use :cl
        :rove
        :lem-lsp-server/server
        :lem-lsp-server/test/test-server)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol-3-15)
                    (:json :lem-lsp-utils/json))
  (:export :initialize-request))
(in-package :lem-lsp-server/test/initialize)

(defparameter *client-capabilities* "{
  \"workspace\":{
  },
  \"textDocument\":{
    \"synchronization\":{
      \"didSave\": true
    },
    \"publishDiagnostics\": {
      \"relatedInformation\": true
    },
    \"completion\":{
      \"completionItem\":{
      },
      \"contextSupport\":true
    },
    \"hover\":{
    },
    \"signatureHelp\":{
      \"signatureInformation\":{
        \"documentationFormat\":[
          \"plaintext\"
        ],
        \"parameterInformation\":{
          \"labelOffsetSupport\":false
        }
      }
    },
    \"definition\":{
      \"linkSupport\":false
    },
    \"typeDefinition\":{
      \"linkSupport\":false
    },
    \"implementation\":{
      \"linkSupport\":false
    },
    \"references\":{
    },
    \"documentSymbol\":{
      \"hierarchicalDocumentSymbolSupport\":true
    },
    \"codeAction\":{
    },
    \"formatting\":{
    },
    \"rangeFormatting\":{
    },
    \"onTypeFormatting\":{
    },
    \"rename\":{
    }
  }
}
")

(defparameter *expected-initialize-result* "{
  \"capabilities\":{
    \"textDocumentSync\":{
      \"openClose\":null,
      \"change\":2,
      \"willSave\":null,
      \"willSaveWaitUntil\":null,
      \"save\":null
    },
    \"completionProvider\":{
      \"workDoneProgress\":null,
      \"triggerCharacters\":[
        \"a\",
        \"b\",
        \"c\",
        \"d\",
        \"e\",
        \"f\",
        \"g\",
        \"h\",
        \"i\",
        \"j\",
        \"k\",
        \"l\",
        \"m\",
        \"n\",
        \"o\",
        \"p\",
        \"q\",
        \"r\",
        \"s\",
        \"t\",
        \"u\",
        \"v\",
        \"w\",
        \"x\",
        \"y\",
        \"z\"
      ],
      \"allCommitCharacters\":[
      ],
      \"resolveProvider\":null
    },
    \"hoverProvider\":{
      \"workDoneProgress\":null
    },
    \"signatureHelpProvider\":{
      \"workDoneProgress\":null,
      \"triggerCharacters\":[
        \" \"
      ],
      \"retriggerCharacters\":[
      ]
    },
    \"declarationProvider\":null,
    \"definitionProvider\":{
      \"workDoneProgress\":null
    },
    \"typeDefinitionProvider\":null,
    \"implementationProvider\":null,
    \"referencesProvider\":{
      \"workDoneProgress\":null
    },
    \"documentHighlightProvider\":{
      \"workDoneProgress\":null
    },
    \"codeActionProvider\":null,
    \"colorProvider\":null,
    \"documentFormattingProvider\":{
      \"workDoneProgress\":null
    },
    \"documentRangeFormattingProvider\":{
      \"workDoneProgress\":null
    },
    \"documentOnTypeFormattingProvider\":{
      \"firstTriggerCharacter\":\")\"
    },
    \"renameProvider\":{
      \"workDoneProgress\":null,
      \"prepareProvider\":null
    },
    \"foldingRangeProvider\":null,
    \"selectionRangeProvider\":null,
    \"workspaceSymbolProvider\":null
  },
  \"serverInfo\":{
    \"name\":\"cl-lsp\"
  }
}")

(defun initialize-request (server)
  (call-lsp-method server
                   "initialize"
                   (json:object-to-json
                    (make-instance
                     'protocol:initialize-params
                     :process-id 1234
                     :client-info (json:make-json :name "lem")
                     :root-uri "file:///Users/user/"
                     :capabilities (json:coerce-json
                                    (yason:parse *client-capabilities*)
                                    'protocol:client-capabilities)
                     :trace "off"
                     :workspace-folders (json:json-null)))))

#+(or)
(deftest initialize
  (let ((server (make-instance 'test-server)))
    (server-listen server)
    (let* ((response (initialize-request server))
           (result (json:coerce-json response 'protocol:initialize-result)))
      (ok (string= *expected-initialize-result*
                   (with-output-to-string (out)
                     (yason:encode (json:object-to-json result)
                                   (yason:make-json-output-stream out))))))))
