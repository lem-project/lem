;;; Code generated based on '/Users/user/common-lisp/lem/language-server-protocol/_specifications/lsp/3.17/metaModel/metaModel.json'; DO NOT EDIT.

(common-lisp:defpackage :lem-language-server/protocol-3-17
  (:use)
  (:export :*version*
           :semantic-token-types
           :namespace
           :type
           :class
           :enum
           :interface
           :struct
           :typeparameter
           :parameter
           :variable
           :property
           :enummember
           :event
           :function
           :method
           :macro
           :keyword
           :modifier
           :comment
           :string
           :number
           :regexp
           :operator
           :decorator
           :semantic-token-modifiers
           :declaration
           :definition
           :readonly
           :static
           :deprecated
           :abstract
           :async
           :modification
           :documentation
           :defaultlibrary
           :document-diagnostic-report-kind
           :full
           :unchanged
           :error-codes
           :parse-error
           :invalid-request
           :method-not-found
           :invalid-params
           :internal-error
           :server-not-initialized
           :unknown-error-code
           :l-s-p-error-codes
           :request-failed
           :server-cancelled
           :content-modified
           :request-cancelled
           :folding-range-kind
           :imports
           :region
           :symbol-kind
           :file
           :module
           :package
           :field
           :constructor
           :constant
           :boolean
           :array
           :object
           :key
           :null
           :enum-member
           :type-parameter
           :symbol-tag
           :uniqueness-level
           :document
           :project
           :group
           :scheme
           :global
           :moniker-kind
           :import
           :export
           :local
           :inlay-hint-kind
           :message-type
           :error
           :warning
           :info
           :log
           :text-document-sync-kind
           :none
           :incremental
           :text-document-save-reason
           :manual
           :after-delay
           :focus-out
           :completion-item-kind
           :text
           :unit
           :value
           :snippet
           :color
           :reference
           :folder
           :completion-item-tag
           :insert-text-format
           :plain-text
           :insert-text-mode
           :asis
           :adjustindentation
           :document-highlight-kind
           :read
           :write
           :code-action-kind
           :empty
           :quick-fix
           :refactor
           :refactor-extract
           :refactor-inline
           :refactor-rewrite
           :source
           :source-organize-imports
           :source-fix-all
           :trace-values
           :off
           :messages
           :verbose
           :markup-kind
           :markdown
           :position-encoding-kind
           :u-t-f8
           :u-t-f16
           :u-t-f32
           :file-change-type
           :created
           :changed
           :deleted
           :watch-kind
           :create
           :change
           :delete
           :diagnostic-severity
           :information
           :hint
           :diagnostic-tag
           :unnecessary
           :completion-trigger-kind
           :invoked
           :trigger-character
           :trigger-for-incomplete-completions
           :signature-help-trigger-kind
           :content-change
           :code-action-trigger-kind
           :automatic
           :file-operation-pattern-kind
           :notebook-cell-kind
           :markup
           :code
           :resource-operation-kind
           :rename
           :failure-handling-kind
           :abort
           :transactional
           :text-only-transactional
           :undo
           :prepare-support-default-behavior
           :identifier
           :token-format
           :relative
           :implementation-params
           :location
           :uri
           :range
           :implementation-registration-options
           :type-definition-params
           :type-definition-registration-options
           :workspace-folder
           :name
           :did-change-workspace-folders-params
           :configuration-params
           :items
           :document-color-params
           :textdocument
           :color-information
           :document-color-registration-options
           :color-presentation-params
           :color-presentation
           :label
           :textedit
           :additionaltext-edits
           :work-done-progress-options
           :workdone-progress
           :text-document-registration-options
           :documentselector
           :folding-range-params
           :folding-range
           :startline
           :startcharacter
           :endline
           :endcharacter
           :kind
           :collapsedtext
           :folding-range-registration-options
           :declaration-params
           :declaration-registration-options
           :selection-range-params
           :positions
           :selection-range
           :parent
           :selection-range-registration-options
           :work-done-progress-create-params
           :token
           :work-done-progress-cancel-params
           :call-hierarchy-prepare-params
           :call-hierarchy-item
           :tags
           :detail
           :selectionrange
           :data
           :call-hierarchy-registration-options
           :call-hierarchy-incoming-calls-params
           :item
           :call-hierarchy-incoming-call
           :from
           :fromranges
           :call-hierarchy-outgoing-calls-params
           :call-hierarchy-outgoing-call
           :to
           :semantic-tokens-params
           :semantic-tokens
           :resultid
           :semantic-tokens-partial-result
           :semantic-tokens-registration-options
           :semantic-tokens-delta-params
           :previousresult-id
           :semantic-tokens-delta
           :edits
           :semantic-tokens-delta-partial-result
           :semantic-tokens-range-params
           :show-document-params
           :external
           :takefocus
           :selection
           :show-document-result
           :success
           :linked-editing-range-params
           :linked-editing-ranges
           :ranges
           :wordpattern
           :linked-editing-range-registration-options
           :create-files-params
           :files
           :workspace-edit
           :changes
           :documentchanges
           :changeannotations
           :file-operation-registration-options
           :filters
           :rename-files-params
           :delete-files-params
           :moniker-params
           :moniker
           :unique
           :moniker-registration-options
           :type-hierarchy-prepare-params
           :type-hierarchy-item
           :type-hierarchy-registration-options
           :type-hierarchy-supertypes-params
           :type-hierarchy-subtypes-params
           :inline-value-params
           :context
           :inline-value-registration-options
           :inlay-hint-params
           :inlay-hint
           :position
           :textedits
           :tooltip
           :paddingleft
           :paddingright
           :inlay-hint-registration-options
           :document-diagnostic-params
           :document-diagnostic-report-partial-result
           :relateddocuments
           :diagnostic-server-cancellation-data
           :retriggerrequest
           :diagnostic-registration-options
           :workspace-diagnostic-params
           :previousresult-ids
           :workspace-diagnostic-report
           :workspace-diagnostic-report-partial-result
           :did-open-notebook-document-params
           :notebookdocument
           :celltext-documents
           :did-change-notebook-document-params
           :did-save-notebook-document-params
           :did-close-notebook-document-params
           :registration-params
           :registrations
           :unregistration-params
           :unregisterations
           :initialize-params
           :initialize-result
           :capabilities
           :serverinfo
           :initialize-error
           :retry
           :initialized-params
           :did-change-configuration-params
           :settings
           :did-change-configuration-registration-options
           :section
           :show-message-params
           :message
           :show-message-request-params
           :actions
           :message-action-item
           :title
           :log-message-params
           :did-open-text-document-params
           :did-change-text-document-params
           :contentchanges
           :text-document-change-registration-options
           :synckind
           :did-close-text-document-params
           :did-save-text-document-params
           :text-document-save-registration-options
           :will-save-text-document-params
           :reason
           :text-edit
           :newtext
           :did-change-watched-files-params
           :did-change-watched-files-registration-options
           :watchers
           :publish-diagnostics-params
           :version
           :diagnostics
           :completion-params
           :completion-item
           :labeldetails
           :preselect
           :sorttext
           :filtertext
           :inserttext
           :inserttext-format
           :inserttext-mode
           :textedit-text
           :commitcharacters
           :command
           :completion-list
           :isincomplete
           :itemdefaults
           :completion-registration-options
           :hover-params
           :hover
           :contents
           :hover-registration-options
           :signature-help-params
           :signature-help
           :signatures
           :activesignature
           :activeparameter
           :signature-help-registration-options
           :definition-params
           :definition-registration-options
           :reference-params
           :reference-registration-options
           :document-highlight-params
           :document-highlight
           :document-highlight-registration-options
           :document-symbol-params
           :symbol-information
           :document-symbol
           :children
           :document-symbol-registration-options
           :code-action-params
           :arguments
           :code-action
           :ispreferred
           :disabled
           :edit
           :code-action-registration-options
           :workspace-symbol-params
           :query
           :workspace-symbol
           :workspace-symbol-registration-options
           :code-lens-params
           :code-lens
           :code-lens-registration-options
           :document-link-params
           :document-link
           :target
           :document-link-registration-options
           :document-formatting-params
           :options
           :document-formatting-registration-options
           :document-range-formatting-params
           :document-range-formatting-registration-options
           :document-on-type-formatting-params
           :ch
           :document-on-type-formatting-registration-options
           :rename-params
           :newname
           :rename-registration-options
           :prepare-rename-params
           :execute-command-params
           :execute-command-registration-options
           :apply-workspace-edit-params
           :apply-workspace-edit-result
           :applied
           :failurereason
           :failedchange
           :work-done-progress-begin
           :cancellable
           :percentage
           :work-done-progress-report
           :work-done-progress-end
           :set-trace-params
           :log-trace-params
           :cancel-params
           :id
           :progress-params
           :text-document-position-params
           :work-done-progress-params
           :workdone-token
           :partial-result-params
           :partialresult-token
           :location-link
           :originselection-range
           :targeturi
           :targetrange
           :targetselection-range
           :start
           :end
           :implementation-options
           :static-registration-options
           :type-definition-options
           :workspace-folders-change-event
           :added
           :removed
           :configuration-item
           :scopeuri
           :text-document-identifier
           :red
           :green
           :blue
           :alpha
           :document-color-options
           :folding-range-options
           :declaration-options
           :line
           :character
           :selection-range-options
           :call-hierarchy-options
           :semantic-tokens-options
           :legend
           :semantic-tokens-edit
           :deletecount
           :linked-editing-range-options
           :file-create
           :text-document-edit
           :create-file
           :rename-file
           :olduri
           :newuri
           :delete-file
           :change-annotation
           :needsconfirmation
           :description
           :file-operation-filter
           :pattern
           :file-rename
           :file-delete
           :moniker-options
           :type-hierarchy-options
           :inline-value-context
           :frameid
           :stoppedlocation
           :inline-value-text
           :inline-value-variable-lookup
           :variablename
           :casesensitive-lookup
           :inline-value-evaluatable-expression
           :expression
           :inline-value-options
           :inlay-hint-label-part
           :markup-content
           :inlay-hint-options
           :resolveprovider
           :related-full-document-diagnostic-report
           :related-unchanged-document-diagnostic-report
           :full-document-diagnostic-report
           :unchanged-document-diagnostic-report
           :diagnostic-options
           :interfile-dependencies
           :workspacediagnostics
           :previous-result-id
           :notebook-document
           :notebooktype
           :metadata
           :cells
           :text-document-item
           :languageid
           :versioned-notebook-document-identifier
           :notebook-document-change-event
           :notebook-document-identifier
           :registration
           :registeroptions
           :unregistration
           :_initialize-params
           :processid
           :clientinfo
           :locale
           :rootpath
           :rooturi
           :initializationoptions
           :trace
           :workspace-folders-initialize-params
           :workspacefolders
           :server-capabilities
           :positionencoding
           :textdocument-sync
           :notebookdocument-sync
           :completionprovider
           :hoverprovider
           :signaturehelp-provider
           :declarationprovider
           :definitionprovider
           :typedefinition-provider
           :implementationprovider
           :referencesprovider
           :documenthighlight-provider
           :documentsymbol-provider
           :codeaction-provider
           :codelens-provider
           :documentlink-provider
           :colorprovider
           :workspacesymbol-provider
           :documentformatting-provider
           :documentrange-formatting-provider
           :documenton-type-formatting-provider
           :renameprovider
           :foldingrange-provider
           :selectionrange-provider
           :executecommand-provider
           :callhierarchy-provider
           :linkedediting-range-provider
           :semantictokens-provider
           :monikerprovider
           :typehierarchy-provider
           :inlinevalue-provider
           :inlayhint-provider
           :diagnosticprovider
           :workspace
           :experimental
           :versioned-text-document-identifier
           :save-options
           :includetext
           :file-event
           :file-system-watcher
           :globpattern
           :diagnostic
           :severity
           :codedescription
           :relatedinformation
           :completion-context
           :triggerkind
           :triggercharacter
           :completion-item-label-details
           :insert-replace-edit
           :insert
           :replace
           :completion-options
           :triggercharacters
           :allcommit-characters
           :completionitem
           :hover-options
           :signature-help-context
           :isretrigger
           :activesignature-help
           :signature-information
           :parameters
           :signature-help-options
           :retriggercharacters
           :definition-options
           :reference-context
           :includedeclaration
           :reference-options
           :document-highlight-options
           :base-symbol-information
           :containername
           :document-symbol-options
           :code-action-context
           :only
           :code-action-options
           :codeaction-kinds
           :workspace-symbol-options
           :code-lens-options
           :document-link-options
           :formatting-options
           :tabsize
           :insertspaces
           :trimtrailing-whitespace
           :insertfinal-newline
           :trimfinal-newlines
           :document-formatting-options
           :document-range-formatting-options
           :document-on-type-formatting-options
           :firsttrigger-character
           :moretrigger-character
           :rename-options
           :prepareprovider
           :execute-command-options
           :commands
           :semantic-tokens-legend
           :tokentypes
           :tokenmodifiers
           :optional-versioned-text-document-identifier
           :annotated-text-edit
           :annotationid
           :resource-operation
           :create-file-options
           :overwrite
           :ignoreif-exists
           :rename-file-options
           :delete-file-options
           :recursive
           :ignoreif-not-exists
           :file-operation-pattern
           :glob
           :matches
           :workspace-full-document-diagnostic-report
           :workspace-unchanged-document-diagnostic-report
           :notebook-cell
           :executionsummary
           :notebook-cell-array-change
           :client-capabilities
           :window
           :general
           :text-document-sync-options
           :openclose
           :willsave
           :willsave-wait-until
           :save
           :notebook-document-sync-options
           :notebookselector
           :notebook-document-sync-registration-options
           :workspace-folders-server-capabilities
           :supported
           :changenotifications
           :file-operation-options
           :didcreate
           :willcreate
           :didrename
           :willrename
           :diddelete
           :willdelete
           :code-description
           :href
           :diagnostic-related-information
           :parameter-information
           :notebook-cell-text-document-filter
           :notebook
           :language
           :file-operation-pattern-options
           :ignorecase
           :execution-summary
           :executionorder
           :workspace-client-capabilities
           :applyedit
           :workspaceedit
           :didchange-configuration
           :didchange-watched-files
           :symbol
           :executecommand
           :configuration
           :semantictokens
           :codelens
           :fileoperations
           :inlinevalue
           :inlayhint
           :text-document-client-capabilities
           :synchronization
           :completion
           :signaturehelp
           :typedefinition
           :implementation
           :references
           :documenthighlight
           :documentsymbol
           :codeaction
           :documentlink
           :formatting
           :rangeformatting
           :ontype-formatting
           :foldingrange
           :publishdiagnostics
           :callhierarchy
           :linkedediting-range
           :typehierarchy
           :notebook-document-client-capabilities
           :window-client-capabilities
           :showmessage
           :showdocument
           :general-client-capabilities
           :stalerequest-support
           :regularexpressions
           :positionencodings
           :relative-pattern
           :baseuri
           :workspace-edit-client-capabilities
           :resourceoperations
           :failurehandling
           :normalizesline-endings
           :changeannotation-support
           :did-change-configuration-client-capabilities
           :dynamicregistration
           :did-change-watched-files-client-capabilities
           :relativepattern-support
           :workspace-symbol-client-capabilities
           :symbolkind
           :tagsupport
           :resolvesupport
           :execute-command-client-capabilities
           :semantic-tokens-workspace-client-capabilities
           :refreshsupport
           :code-lens-workspace-client-capabilities
           :file-operation-client-capabilities
           :inline-value-workspace-client-capabilities
           :inlay-hint-workspace-client-capabilities
           :diagnostic-workspace-client-capabilities
           :text-document-sync-client-capabilities
           :didsave
           :completion-client-capabilities
           :completionitem-kind
           :contextsupport
           :completionlist
           :hover-client-capabilities
           :contentformat
           :signature-help-client-capabilities
           :signatureinformation
           :declaration-client-capabilities
           :linksupport
           :definition-client-capabilities
           :type-definition-client-capabilities
           :implementation-client-capabilities
           :reference-client-capabilities
           :document-highlight-client-capabilities
           :document-symbol-client-capabilities
           :hierarchicaldocument-symbol-support
           :labelsupport
           :code-action-client-capabilities
           :codeaction-literal-support
           :ispreferred-support
           :disabledsupport
           :datasupport
           :honorschange-annotations
           :code-lens-client-capabilities
           :document-link-client-capabilities
           :tooltipsupport
           :document-color-client-capabilities
           :document-formatting-client-capabilities
           :document-range-formatting-client-capabilities
           :document-on-type-formatting-client-capabilities
           :rename-client-capabilities
           :preparesupport
           :preparesupport-default-behavior
           :folding-range-client-capabilities
           :rangelimit
           :linefolding-only
           :foldingrange-kind
           :selection-range-client-capabilities
           :publish-diagnostics-client-capabilities
           :versionsupport
           :codedescription-support
           :call-hierarchy-client-capabilities
           :semantic-tokens-client-capabilities
           :requests
           :formats
           :overlappingtoken-support
           :multilinetoken-support
           :servercancel-support
           :augmentssyntax-tokens
           :linked-editing-range-client-capabilities
           :moniker-client-capabilities
           :type-hierarchy-client-capabilities
           :inline-value-client-capabilities
           :inlay-hint-client-capabilities
           :diagnostic-client-capabilities
           :relateddocument-support
           :notebook-document-sync-client-capabilities
           :executionsummary-support
           :show-message-request-client-capabilities
           :messageaction-item
           :show-document-client-capabilities
           :support
           :regular-expressions-client-capabilities
           :engine
           :markdown-client-capabilities
           :parser
           :allowedtags
           :definition-link
           :l-s-p-array
           :l-s-p-any
           :declaration-link
           :inline-value
           :document-diagnostic-report
           :prepare-rename-result
           :document-selector
           :progress-token
           :change-annotation-identifier
           :workspace-document-diagnostic-report
           :text-document-content-change-event
           :marked-string
           :document-filter
           :l-s-p-object
           :glob-pattern
           :text-document-filter
           :notebook-document-filter))
(common-lisp:in-package :lem-language-server/protocol-3-17)

(lem-language-server/protocol-generator::define-enum semantic-token-types
    ((namespace "namespace")
     (type "type" :documentation
      "Represents a generic type. Acts as a fallback for types which can't be mapped to
a specific type like class or enum.")
     (class "class") (enum "enum") (interface "interface") (struct "struct")
     (typeparameter "typeParameter") (parameter "parameter") (variable "variable")
     (property "property") (enummember "enumMember") (event "event") (function "function")
     (method "method") (macro "macro") (keyword "keyword") (modifier "modifier")
     (comment "comment") (string "string") (number "number") (regexp "regexp")
     (operator "operator") (decorator "decorator" :documentation "@since 3.17.0" :since "3.17.0"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A set of predefined token types. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum semantic-token-modifiers
    ((declaration "declaration") (definition "definition") (readonly "readonly") (static "static")
     (deprecated "deprecated") (abstract "abstract") (async "async") (modification "modification")
     (documentation "documentation") (defaultlibrary "defaultLibrary"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A set of predefined token modifiers. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum document-diagnostic-report-kind
    ((full "full" :documentation "A diagnostic report with a full
set of problems.")
     (unchanged "unchanged" :documentation "A report indicating that the last
returned report is still accurate."))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "The document diagnostic report kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-language-server/protocol-generator::define-enum error-codes
    ((parse-error -32700) (invalid-request -32600) (method-not-found -32601)
     (invalid-params -32602) (internal-error -32603)
     (server-not-initialized -32002 :documentation
      "Error code indicating that a server received a notification or
request before the server has received the `initialize` request.")
     (unknown-error-code -32001))
  (:type lem-language-server/protocol-generator::lsp-integer)
  :since
  "Predefined error codes.")

(lem-language-server/protocol-generator::define-enum l-s-p-error-codes
    ((request-failed -32803 :documentation
      "A request failed but it was syntactically correct, e.g the
method name was known and the parameters were valid. The error
message should contain human readable information about why
the request failed.

@since 3.17.0"
      :since "3.17.0")
     (server-cancelled -32802 :documentation
      "The server cancelled the request. This error code should
only be used for requests that explicitly support being
server cancellable.

@since 3.17.0"
      :since "3.17.0")
     (content-modified -32801 :documentation "The server detected that the content of a document got
modified outside normal conditions. A server should
NOT send this error code if it detects a content change
in it unprocessed messages. The result even computed
on an older state might still be useful for the client.

If a client decides that a result is not of any use anymore
the client should cancel the request.")
     (request-cancelled -32800 :documentation
      "The client has canceled a request and a server as detected
the cancel."))
  (:type lem-language-server/protocol-generator::lsp-integer))

(lem-language-server/protocol-generator::define-enum folding-range-kind
    ((comment "comment" :documentation "Folding range for a comment")
     (imports "imports" :documentation "Folding range for an import or include")
     (region "region" :documentation "Folding range for a region (e.g. `#region`)"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A set of predefined range kinds.")

(lem-language-server/protocol-generator::define-enum symbol-kind
    ((file 1) (module 2) (namespace 3) (package 4) (class 5) (method 6) (property 7) (field 8)
     (constructor 9) (enum 10) (interface 11) (function 12) (variable 13) (constant 14) (string 15)
     (number 16) (boolean 17) (array 18) (object 19) (key 20) (null 21) (enum-member 22)
     (struct 23) (event 24) (operator 25) (type-parameter 26))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "A symbol kind.")

(lem-language-server/protocol-generator::define-enum symbol-tag
    ((deprecated 1 :documentation "Render a symbol as obsolete, usually using a strike-out."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Symbol tags are extra annotations that tweak the rendering of a symbol.

@since 3.16"
  :since
  "3.16")

(lem-language-server/protocol-generator::define-enum uniqueness-level
    ((document "document" :documentation "The moniker is only unique inside a document")
     (project "project" :documentation
      "The moniker is unique inside a project for which a dump got created")
     (group "group" :documentation
      "The moniker is unique inside the group to which a project belongs")
     (scheme "scheme" :documentation "The moniker is unique inside the moniker scheme.")
     (global "global" :documentation "The moniker is globally unique"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "Moniker uniqueness level to define scope of the moniker.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum moniker-kind
    ((import "import" :documentation
      "The moniker represent a symbol that is imported into a project")
     (export "export" :documentation
      "The moniker represents a symbol that is exported from a project")
     (local "local" :documentation
      "The moniker represents a symbol that is local to a project (e.g. a local
variable of a function, a class not visible outside the project, ...)"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "The moniker kind.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum inlay-hint-kind
    ((type 1 :documentation "An inlay hint that for a type annotation.")
     (parameter 2 :documentation "An inlay hint that is for a parameter."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Inlay hint kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-language-server/protocol-generator::define-enum message-type
    ((error 1 :documentation "An error message.") (warning 2 :documentation "A warning message.")
     (info 3 :documentation "An information message.") (log 4 :documentation "A log message."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The message type")

(lem-language-server/protocol-generator::define-enum text-document-sync-kind
    ((none 0 :documentation "Documents should not be synced at all.")
     (full 1 :documentation "Documents are synced by always sending the full content
of the document.")
     (incremental 2 :documentation "Documents are synced by sending the full content on open.
After that only incremental updates to the document are
send."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Defines how the host (editor) should sync
document changes to the language server.")

(lem-language-server/protocol-generator::define-enum text-document-save-reason
    ((manual 1 :documentation
      "Manually triggered, e.g. by the user pressing save, by starting debugging,
or by an API call.")
     (after-delay 2 :documentation "Automatic after a delay.")
     (focus-out 3 :documentation "When the editor lost focus."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Represents reasons why a text document is saved.")

(lem-language-server/protocol-generator::define-enum completion-item-kind
    ((text 1) (method 2) (function 3) (constructor 4) (field 5) (variable 6) (class 7)
     (interface 8) (module 9) (property 10) (unit 11) (value 12) (enum 13) (keyword 14)
     (snippet 15) (color 16) (file 17) (reference 18) (folder 19) (enum-member 20) (constant 21)
     (struct 22) (event 23) (operator 24) (type-parameter 25))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The kind of a completion entry.")

(lem-language-server/protocol-generator::define-enum completion-item-tag
    ((deprecated 1 :documentation "Render a completion as obsolete, usually using a strike-out."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Completion item tags are extra annotations that tweak the rendering of a completion
item.

@since 3.15.0"
  :since
  "3.15.0")

(lem-language-server/protocol-generator::define-enum insert-text-format
    ((plain-text 1 :documentation "The primary text to be inserted is treated as a plain string.")
     (snippet 2 :documentation "The primary text to be inserted is treated as a snippet.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Placeholders with equal identifiers are linked,
that is typing in one will update others too.

See also: https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax"))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "Defines whether the insert text in a completion item should be interpreted as
plain text or a snippet.")

(lem-language-server/protocol-generator::define-enum insert-text-mode
    ((asis 1 :documentation "The insertion or replace strings is taken as it is. If the
value is multi line the lines below the cursor will be
inserted using the indentation defined in the string value.
The client will not apply any kind of adjustments to the
string.")
     (adjustindentation 2 :documentation "The editor adjusts leading whitespace of new lines so that
they match the indentation up to the cursor of the line for
which the item is accepted.

Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
multi line completion item is indented using 2 tabs and all
following lines inserted will be indented using 2 tabs as well."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "How whitespace and indentation is handled during completion
item insertion.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum document-highlight-kind
    ((text 1 :documentation "A textual occurrence.")
     (read 2 :documentation "Read-access of a symbol, like reading a variable.")
     (write 3 :documentation "Write-access of a symbol, like writing to a variable."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "A document highlight kind.")

(lem-language-server/protocol-generator::define-enum code-action-kind
    ((empty "" :documentation "Empty kind.")
     (quick-fix "quickfix" :documentation "Base kind for quickfix actions: 'quickfix'")
     (refactor "refactor" :documentation "Base kind for refactoring actions: 'refactor'")
     (refactor-extract "refactor.extract" :documentation
      "Base kind for refactoring extraction actions: 'refactor.extract'

Example extract actions:

- Extract method
- Extract function
- Extract variable
- Extract interface from class
- ...")
     (refactor-inline "refactor.inline" :documentation
      "Base kind for refactoring inline actions: 'refactor.inline'

Example inline actions:

- Inline function
- Inline variable
- Inline constant
- ...")
     (refactor-rewrite "refactor.rewrite" :documentation
      "Base kind for refactoring rewrite actions: 'refactor.rewrite'

Example rewrite actions:

- Convert JavaScript function to class
- Add or remove parameter
- Encapsulate field
- Make method static
- Move method to base class
- ...")
     (source "source" :documentation "Base kind for source actions: `source`

Source code actions apply to the entire file.")
     (source-organize-imports "source.organizeImports" :documentation
      "Base kind for an organize imports source action: `source.organizeImports`")
     (source-fix-all "source.fixAll" :documentation
      "Base kind for auto-fix source actions: `source.fixAll`.

Fix all actions automatically fix errors that have a clear fix that do not require user input.
They should not suppress errors or perform unsafe fixes such as generating new types or classes.

@since 3.15.0"
      :since "3.15.0"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A set of predefined code action kinds")

(lem-language-server/protocol-generator::define-enum trace-values
    ((off "off" :documentation "Turn tracing off.")
     (messages "messages" :documentation "Trace messages only.")
     (verbose "verbose" :documentation "Verbose message tracing."))
  (:type lem-language-server/protocol-generator::lsp-string))

(lem-language-server/protocol-generator::define-enum markup-kind
    ((plain-text "plaintext" :documentation "Plain text is supported as a content format")
     (markdown "markdown" :documentation "Markdown is supported as a content format"))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "Describes the content type that a client supports in various
result literals like `Hover`, `ParameterInfo` or `CompletionItem`.

Please note that `MarkupKinds` must not start with a `$`. This kinds
are reserved for internal usage.")

(lem-language-server/protocol-generator::define-enum position-encoding-kind
    ((u-t-f8 "utf-8" :documentation "Character offsets count UTF-8 code units.")
     (u-t-f16 "utf-16" :documentation "Character offsets count UTF-16 code units.

This is the default and must always be supported
by servers")
     (u-t-f32 "utf-32" :documentation "Character offsets count UTF-32 code units.

Implementation note: these are the same as Unicode code points,
so this `PositionEncodingKind` may also be used for an
encoding-agnostic representation of character offsets."))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A set of predefined position encoding kinds.

@since 3.17.0"
  :since
  "3.17.0")

(lem-language-server/protocol-generator::define-enum file-change-type
    ((created 1 :documentation "The file got created.")
     (changed 2 :documentation "The file got changed.")
     (deleted 3 :documentation "The file got deleted."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The file event type")

(lem-language-server/protocol-generator::define-enum watch-kind
    ((create 1 :documentation "Interested in create events.")
     (change 2 :documentation "Interested in change events")
     (delete 4 :documentation "Interested in delete events"))
  (:type lem-language-server/protocol-generator::lsp-uinteger))

(lem-language-server/protocol-generator::define-enum diagnostic-severity
    ((error 1 :documentation "Reports an error.") (warning 2 :documentation "Reports a warning.")
     (information 3 :documentation "Reports an information.")
     (hint 4 :documentation "Reports a hint."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The diagnostic's severity.")

(lem-language-server/protocol-generator::define-enum diagnostic-tag
    ((unnecessary 1 :documentation "Unused or unnecessary code.

Clients are allowed to render diagnostics with this tag faded out instead of having
an error squiggle.")
     (deprecated 2 :documentation "Deprecated or obsolete code.

Clients are allowed to rendered diagnostics with this tag strike through."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The diagnostic tags.

@since 3.15.0"
  :since
  "3.15.0")

(lem-language-server/protocol-generator::define-enum completion-trigger-kind
    ((invoked 1 :documentation "Completion was triggered by typing an identifier (24x7 code
complete), manual invocation (e.g Ctrl+Space) or via API.")
     (trigger-character 2 :documentation
      "Completion was triggered by a trigger character specified by
the `triggerCharacters` properties of the `CompletionRegistrationOptions`.")
     (trigger-for-incomplete-completions 3 :documentation
      "Completion was re-triggered as current completion list is incomplete"))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "How a completion was triggered")

(lem-language-server/protocol-generator::define-enum signature-help-trigger-kind
    ((invoked 1 :documentation "Signature help was invoked manually by the user or by a command.")
     (trigger-character 2 :documentation "Signature help was triggered by a trigger character.")
     (content-change 3 :documentation
      "Signature help was triggered by the cursor moving or by the document content changing."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "How a signature help was triggered.

@since 3.15.0"
  :since
  "3.15.0")

(lem-language-server/protocol-generator::define-enum code-action-trigger-kind
    ((invoked 1 :documentation
      "Code actions were explicitly requested by the user or by an extension.")
     (automatic 2 :documentation "Code actions were requested automatically.

This typically happens when current selection in a file changes, but can
also be triggered when file content changes."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "The reason why code actions were requested.

@since 3.17.0"
  :since
  "3.17.0")

(lem-language-server/protocol-generator::define-enum file-operation-pattern-kind
    ((file "file" :documentation "The pattern matches a file only.")
     (folder "folder" :documentation "The pattern matches a folder only."))
  (:type lem-language-server/protocol-generator::lsp-string)
  :since
  "A pattern kind describing if a glob pattern matches a file a folder or
both.

@since 3.16.0"
  :since
  "3.16.0")

(lem-language-server/protocol-generator::define-enum notebook-cell-kind
    ((markup 1 :documentation "A markup-cell is formatted source that is used for display.")
     (code 2 :documentation "A code-cell is source code."))
  (:type lem-language-server/protocol-generator::lsp-uinteger)
  :since
  "A notebook cell kind.

@since 3.17.0"
  :since
  "3.17.0")

(lem-language-server/protocol-generator::define-enum resource-operation-kind
    ((create "create" :documentation "Supports creating new files and folders.")
     (rename "rename" :documentation "Supports renaming existing files and folders.")
     (delete "delete" :documentation "Supports deleting existing files and folders."))
  (:type lem-language-server/protocol-generator::lsp-string))

(lem-language-server/protocol-generator::define-enum failure-handling-kind
    ((abort "abort" :documentation
      "Applying the workspace change is simply aborted if one of the changes provided
fails. All operations executed before the failing operation stay executed.")
     (transactional "transactional" :documentation
      "All operations are executed transactional. That means they either all
succeed or no changes at all are applied to the workspace.")
     (text-only-transactional "textOnlyTransactional" :documentation
      "If the workspace edit contains only textual file changes they are executed transactional.
If resource changes (create, rename or delete file) are part of the change the failure
handling strategy is abort.")
     (undo "undo" :documentation
      "The client tries to undo the operations already executed. But there is no
guarantee that this is succeeding."))
  (:type lem-language-server/protocol-generator::lsp-string))

(lem-language-server/protocol-generator::define-enum prepare-support-default-behavior
    ((identifier 1 :documentation "The client's default behavior is to select the identifier
according the to language's syntax rule."))
  (:type lem-language-server/protocol-generator::lsp-uinteger))

(lem-language-server/protocol-generator::define-enum token-format
    ((relative "relative"))
  (:type lem-language-server/protocol-generator::lsp-string))

(lem-language-server/protocol-generator::define-class implementation-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class location
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri))
   (range :type range :initform (alexandria:required-argument :range)))
  (:documentation "Represents a location inside a resource, such as a line
inside a text file.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class implementation-registration-options
    (text-document-registration-options implementation-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-definition-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-definition-registration-options
    (text-document-registration-options type-definition-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-folder
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :uri) :documentation
    "The associated URI for this workspace folder.")
   (name :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :name) :documentation
    "The name of the workspace folder. Used to refer to this
workspace folder in the user interface."))
  (:documentation "A workspace folder inside a client.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-workspace-folders-params
    common-lisp:nil
  ((event :type workspace-folders-change-event :initform (alexandria:required-argument :event)
    :documentation "The actual workspace folder change event."))
  (:documentation "The parameters of a `workspace/didChangeWorkspaceFolders` notification.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class configuration-params
    common-lisp:nil
  ((items :type (lem-language-server/protocol-generator::lsp-array configuration-item) :initform
    (alexandria:required-argument :items)))
  (:documentation "The parameters of a configuration request.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-color-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document."))
  (:documentation "Parameters for a {@link DocumentColorRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class color-information
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range in the document where this color appears.")
   (color :type color :initform (alexandria:required-argument :color) :documentation
    "The actual color value for this color range."))
  (:documentation "Represents a color range from a document.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-color-registration-options
    (text-document-registration-options document-color-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class color-presentation-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (color :type color :initform (alexandria:required-argument :color) :documentation
    "The color to request presentations for.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range where the color would be inserted. Serves as a context."))
  (:documentation "Parameters for a {@link ColorPresentationRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class color-presentation
    common-lisp:nil
  ((label :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :label) :documentation
    "The label of this color presentation. It will be shown on the color
picker header. By default this is also the text that is inserted when selecting
this color presentation.")
   (textedit :type text-edit :documentation
    "An {@link TextEdit edit} which is applied to a document when selecting
this presentation for the color.  When `falsy` the {@link ColorPresentation.label label}
is used.")
   (additionaltext-edits :type (lem-language-server/protocol-generator::lsp-array text-edit)
    :documentation
    "An optional array of additional {@link TextEdit text edits} that are applied when
selecting this color presentation. Edits must not overlap with the main {@link ColorPresentation.textEdit edit} nor with themselves."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-options
    common-lisp:nil
  ((workdone-progress :type lem-language-server/protocol-generator::lsp-boolean))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-registration-options
    common-lisp:nil
  ((documentselector :type
    (common-lisp:or document-selector lem-language-server/protocol-generator::lsp-null) :initform
    (alexandria:required-argument :documentselector) :documentation
    "A document selector to identify the scope of the registration. If set to null
the document selector provided on the client side will be used."))
  (:documentation "General text document registration options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class folding-range-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document."))
  (:documentation "Parameters for a {@link FoldingRangeRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class folding-range
    common-lisp:nil
  ((startline :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :startline) :documentation
    "The zero-based start line of the range to fold. The folded area starts after the line's last character.
To be valid, the end must be zero or larger and smaller than the number of lines in the document.")
   (startcharacter :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.")
   (endline :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :endline) :documentation
    "The zero-based end line of the range to fold. The folded area ends with the line's last character.
To be valid, the end must be zero or larger and smaller than the number of lines in the document.")
   (endcharacter :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.")
   (kind :type folding-range-kind :documentation
    "Describes the kind of the folding range such as `comment' or 'region'. The kind
is used to categorize folding ranges and used by commands like 'Fold all comments'.
See {@link FoldingRangeKind} for an enumeration of standardized kinds.")
   (collapsedtext :type lem-language-server/protocol-generator::lsp-string :since "3.17.0"
    :documentation "The text that the client should show when the specified range is
collapsed. If not defined or not supported by the client, a default
will be chosen by the client.

@since 3.17.0"))
  (:documentation
   "Represents a folding range. To be valid, start and end line must be bigger than zero and smaller
than the number of lines in the document. Clients are free to ignore invalid ranges.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class folding-range-registration-options
    (text-document-registration-options folding-range-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class declaration-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class declaration-registration-options
    (declaration-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class selection-range-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (positions :type (lem-language-server/protocol-generator::lsp-array position) :initform
    (alexandria:required-argument :positions) :documentation
    "The positions inside the text document."))
  (:documentation "A parameter literal used in selection range requests.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class selection-range
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The {@link Range range} of this selection range.")
   (parent :type selection-range :documentation
    "The parent selection range containing this range. Therefore `parent.range` must contain `this.range`."))
  (:documentation "A selection range represents a part of a selection hierarchy. A selection range
may have a parent selection range that contains it.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class selection-range-registration-options
    (selection-range-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-create-params
    common-lisp:nil
  ((token :type progress-token :initform (alexandria:required-argument :token) :documentation
    "The token to be used to report progress."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-cancel-params
    common-lisp:nil
  ((token :type progress-token :initform (alexandria:required-argument :token) :documentation
    "The token to be used to report progress."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-prepare-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "The parameter of a `textDocument/prepareCallHierarchy` request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-item
    common-lisp:nil
  ((name :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :name) :documentation "The name of this item.")
   (kind :type symbol-kind :initform (alexandria:required-argument :kind) :documentation
    "The kind of this item.")
   (tags :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :documentation
    "Tags for this item.")
   (detail :type lem-language-server/protocol-generator::lsp-string :documentation
    "More detail for this item, e.g. the signature of a function.")
   (uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The resource identifier of this item.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range enclosing this symbol not including leading/trailing whitespace but everything else, e.g. comments and code.")
   (selectionrange :type range :initform (alexandria:required-argument :selectionrange)
    :documentation
    "The range that should be selected and revealed when this symbol is being picked, e.g. the name of a function.
Must be contained by the {@link CallHierarchyItem.range `range`}.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved between a call hierarchy prepare and
incoming calls or outgoing calls requests."))
  (:since "3.16.0")
  (:documentation "Represents programming constructs like functions or constructors in the context
of call hierarchy.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-registration-options
    (text-document-registration-options call-hierarchy-options static-registration-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "Call hierarchy options used during static or dynamic registration.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-incoming-calls-params
    (work-done-progress-params partial-result-params)
  ((item :type call-hierarchy-item :initform (alexandria:required-argument :item)))
  (:since "3.16.0")
  (:documentation "The parameter of a `callHierarchy/incomingCalls` request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-incoming-call
    common-lisp:nil
  ((from :type call-hierarchy-item :initform (alexandria:required-argument :from) :documentation
    "The item that makes the call.")
   (fromranges :type (lem-language-server/protocol-generator::lsp-array range) :initform
    (alexandria:required-argument :fromranges) :documentation
    "The ranges at which the calls appear. This is relative to the caller
denoted by {@link CallHierarchyIncomingCall.from `this.from`}."))
  (:since "3.16.0")
  (:documentation "Represents an incoming call, e.g. a caller of a method or constructor.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-outgoing-calls-params
    (work-done-progress-params partial-result-params)
  ((item :type call-hierarchy-item :initform (alexandria:required-argument :item)))
  (:since "3.16.0")
  (:documentation "The parameter of a `callHierarchy/outgoingCalls` request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-outgoing-call
    common-lisp:nil
  ((to :type call-hierarchy-item :initform (alexandria:required-argument :to) :documentation
    "The item that is called.")
   (fromranges :type (lem-language-server/protocol-generator::lsp-array range) :initform
    (alexandria:required-argument :fromranges) :documentation
    "The range at which this item is called. This is the range relative to the caller, e.g the item
passed to {@link CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls `provideCallHierarchyOutgoingCalls`}
and not {@link CallHierarchyOutgoingCall.to `this.to`}."))
  (:since "3.16.0")
  (:documentation
   "Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens
    common-lisp:nil
  ((resultid :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional result id. If provided and clients support delta updating
the client will include the result id in the next semantic token request.
A server can then instead of computing all semantic tokens again simply
send a delta.")
   (data :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-uinteger)
    :initform (alexandria:required-argument :data) :documentation "The actual tokens."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-partial-result
    common-lisp:nil
  ((data :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-uinteger)
    :initform (alexandria:required-argument :data)))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-registration-options
    (text-document-registration-options semantic-tokens-options static-registration-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-delta-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (previousresult-id :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :previousresult-id) :documentation
    "The result id of a previous response. The result Id can either point to a full response
or a delta response depending on what was received last."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-delta
    common-lisp:nil
  ((resultid :type lem-language-server/protocol-generator::lsp-string)
   (edits :type (lem-language-server/protocol-generator::lsp-array semantic-tokens-edit) :initform
    (alexandria:required-argument :edits) :documentation
    "The semantic token edits to transform a previous result into a new result."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-delta-partial-result
    common-lisp:nil
  ((edits :type (lem-language-server/protocol-generator::lsp-array semantic-tokens-edit) :initform
    (alexandria:required-argument :edits)))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-range-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range the semantic tokens are requested for."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-document-params
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :uri) :documentation "The document uri to show.")
   (external :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Indicates to show the resource in an external program.
To show for example `https://code.visualstudio.com/`
in the default WEB browser set `external` to `true`.")
   (takefocus :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "An optional property to indicate whether the editor
showing the document should take focus or not.
Clients might ignore this property if an external
program is started.")
   (selection :type range :documentation "An optional selection range if the document is a text
document. Clients might ignore the property if an
external program is started or the file is not a text
file."))
  (:since "3.16.0")
  (:documentation "Params to show a document.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-document-result
    common-lisp:nil
  ((success :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :success) :documentation
    "A boolean indicating if the show was successful."))
  (:since "3.16.0")
  (:documentation "The result of a showDocument request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class linked-editing-range-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class linked-editing-ranges
    common-lisp:nil
  ((ranges :type (lem-language-server/protocol-generator::lsp-array range) :initform
    (alexandria:required-argument :ranges) :documentation
    "A list of ranges that can be edited together. The ranges must have
identical length and contain identical text content. The ranges cannot overlap.")
   (wordpattern :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional word pattern (regular expression) that describes valid contents for
the given ranges. If no pattern is provided, the client configuration's word
pattern will be used."))
  (:since "3.16.0")
  (:documentation "The result of a linked editing range request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class linked-editing-range-registration-options
    (text-document-registration-options linked-editing-range-options static-registration-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class create-files-params
    common-lisp:nil
  ((files :type (lem-language-server/protocol-generator::lsp-array file-create) :initform
    (alexandria:required-argument :files) :documentation
    "An array of all files/folders created in this operation."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated creation of
files.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-edit
    common-lisp:nil
  ((changes :type
    (lem-language-server/protocol-generator::lsp-map document-uri
     (lem-language-server/protocol-generator::lsp-array text-edit))
    :documentation "Holds changes to existing resources.")
   (documentchanges :type
    (lem-language-server/protocol-generator::lsp-array
     (common-lisp:or text-document-edit create-file rename-file delete-file))
    :documentation
    "Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
are either an array of `TextDocumentEdit`s to express changes to n different text documents
where each text document edit addresses a specific version of a text document. Or it can contain
above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.

Whether a client supports versioned document edits is expressed via
`workspace.workspaceEdit.documentChanges` client capability.

If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
only plain `TextEdit`s using the `changes` property are supported.")
   (changeannotations :type
    (lem-language-server/protocol-generator::lsp-map change-annotation-identifier
     change-annotation)
    :since "3.16.0" :documentation
    "A map of change annotations that can be referenced in `AnnotatedTextEdit`s or create, rename and
delete file / folder operations.

Whether clients honor this property depends on the client capability `workspace.changeAnnotationSupport`.

@since 3.16.0"))
  (:documentation
   "A workspace edit represents changes to many resources managed in the workspace. The edit
should either provide `changes` or `documentChanges`. If documentChanges are present
they are preferred over `changes` if the client can handle versioned document edits.

Since version 3.13.0 a workspace edit can contain resource operations as well. If resource
operations are present clients need to execute the operations in the order in which they
are provided. So a workspace edit for example can consist of the following two changes:
(1) a create file a.txt and (2) a text document edit which insert text into file a.txt.

An invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will
cause failure of the operation. How the client recovers from the failure is described by
the client capability: `workspace.workspaceEdit.failureHandling`")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-registration-options
    common-lisp:nil
  ((filters :type (lem-language-server/protocol-generator::lsp-array file-operation-filter)
    :initform (alexandria:required-argument :filters) :documentation "The actual filters."))
  (:since "3.16.0")
  (:documentation "The options to register for file operations.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-files-params
    common-lisp:nil
  ((files :type (lem-language-server/protocol-generator::lsp-array file-rename) :initform
    (alexandria:required-argument :files) :documentation
    "An array of all files/folders renamed in this operation. When a folder is renamed, only
the folder will be included, and not its children."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated renames of
files.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class delete-files-params
    common-lisp:nil
  ((files :type (lem-language-server/protocol-generator::lsp-array file-delete) :initform
    (alexandria:required-argument :files) :documentation
    "An array of all files/folders deleted in this operation."))
  (:since "3.16.0")
  (:documentation "The parameters sent in notifications/requests for user-initiated deletes of
files.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class moniker-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class moniker
    common-lisp:nil
  ((scheme :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :scheme) :documentation
    "The scheme of the moniker. For example tsc or .Net")
   (identifier :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :identifier) :documentation
    "The identifier of the moniker. The value is opaque in LSIF however
schema owners are allowed to define the structure if they want.")
   (unique :type uniqueness-level :initform (alexandria:required-argument :unique) :documentation
    "The scope in which the moniker is unique")
   (kind :type moniker-kind :documentation "The moniker kind if known."))
  (:since "3.16.0")
  (:documentation "Moniker definition to match LSIF 0.5 moniker definition.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class moniker-registration-options
    (text-document-registration-options moniker-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-prepare-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "The parameter of a `textDocument/prepareTypeHierarchy` request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-item
    common-lisp:nil
  ((name :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :name) :documentation "The name of this item.")
   (kind :type symbol-kind :initform (alexandria:required-argument :kind) :documentation
    "The kind of this item.")
   (tags :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :documentation
    "Tags for this item.")
   (detail :type lem-language-server/protocol-generator::lsp-string :documentation
    "More detail for this item, e.g. the signature of a function.")
   (uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The resource identifier of this item.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range enclosing this symbol not including leading/trailing whitespace
but everything else, e.g. comments and code.")
   (selectionrange :type range :initform (alexandria:required-argument :selectionrange)
    :documentation "The range that should be selected and revealed when this symbol is being
picked, e.g. the name of a function. Must be contained by the
{@link TypeHierarchyItem.range `range`}.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved between a type hierarchy prepare and
supertypes or subtypes requests. It could also be used to identify the
type hierarchy in the server, helping improve the performance on
resolving supertypes and subtypes."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-registration-options
    (text-document-registration-options type-hierarchy-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Type hierarchy options used during static or dynamic registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-supertypes-params
    (work-done-progress-params partial-result-params)
  ((item :type type-hierarchy-item :initform (alexandria:required-argument :item)))
  (:since "3.17.0")
  (:documentation "The parameter of a `typeHierarchy/supertypes` request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-subtypes-params
    (work-done-progress-params partial-result-params)
  ((item :type type-hierarchy-item :initform (alexandria:required-argument :item)))
  (:since "3.17.0")
  (:documentation "The parameter of a `typeHierarchy/subtypes` request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-params
    (work-done-progress-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The document range for which inline values should be computed.")
   (context :type inline-value-context :initform (alexandria:required-argument :context)
    :documentation "Additional information about the context in which inline values were
requested."))
  (:since "3.17.0")
  (:documentation "A parameter literal used in inline value requests.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-registration-options
    (inline-value-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inline value options used during static or dynamic registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-params
    (work-done-progress-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The document range for which inlay hints should be computed."))
  (:since "3.17.0")
  (:documentation "A parameter literal used in inlay hint requests.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint
    common-lisp:nil
  ((position :type position :initform (alexandria:required-argument :position) :documentation
    "The position of this hint.")
   (label :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    (lem-language-server/protocol-generator::lsp-array inlay-hint-label-part))
    :initform (alexandria:required-argument :label) :documentation
    "The label of this hint. A human readable string or an array of
InlayHintLabelPart label parts.

*Note* that neither the string nor the label part can be empty.")
   (kind :type inlay-hint-kind :documentation
    "The kind of this hint. Can be omitted in which case the client
should fall back to a reasonable default.")
   (textedits :type (lem-language-server/protocol-generator::lsp-array text-edit) :documentation
    "Optional text edits that are performed when accepting this inlay hint.

*Note* that edits are expected to change the document so that the inlay
hint (or its nearest variant) is now part of the document and the inlay
hint itself is now obsolete.")
   (tooltip :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string markup-content)
    :documentation "The tooltip text when you hover over this item.")
   (paddingleft :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Render padding before the hint.

Note: Padding should use the editor's background color, not the
background color of the hint itself. That means padding can be used
to visually align/separate an inlay hint.")
   (paddingright :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Render padding after the hint.

Note: Padding should use the editor's background color, not the
background color of the hint itself. That means padding can be used
to visually align/separate an inlay hint.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved on an inlay hint between
a `textDocument/inlayHint` and a `inlayHint/resolve` request."))
  (:since "3.17.0")
  (:documentation "Inlay hint information.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-registration-options
    (inlay-hint-options text-document-registration-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inlay hint options used during static or dynamic registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-diagnostic-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (identifier :type lem-language-server/protocol-generator::lsp-string :documentation
    "The additional identifier  provided during registration.")
   (previousresult-id :type lem-language-server/protocol-generator::lsp-string :documentation
    "The result id of a previous response if provided."))
  (:since "3.17.0")
  (:documentation "Parameters of the document diagnostic request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-diagnostic-report-partial-result
    common-lisp:nil
  ((relateddocuments :type
    (lem-language-server/protocol-generator::lsp-map document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :initform (alexandria:required-argument :relateddocuments)))
  (:since "3.17.0")
  (:documentation "A partial result for a document diagnostic report.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-server-cancellation-data
    common-lisp:nil
  ((retriggerrequest :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :retriggerrequest)))
  (:since "3.17.0")
  (:documentation "Cancellation data returned from a diagnostic request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-registration-options
    (text-document-registration-options diagnostic-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Diagnostic registration options.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-diagnostic-params
    (work-done-progress-params partial-result-params)
  ((identifier :type lem-language-server/protocol-generator::lsp-string :documentation
    "The additional identifier provided during registration.")
   (previousresult-ids :type (lem-language-server/protocol-generator::lsp-array previous-result-id)
    :initform (alexandria:required-argument :previousresult-ids) :documentation
    "The currently known diagnostic reports with their
previous result ids."))
  (:since "3.17.0")
  (:documentation "Parameters of the workspace diagnostic request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-diagnostic-report
    common-lisp:nil
  ((items :type
    (lem-language-server/protocol-generator::lsp-array workspace-document-diagnostic-report)
    :initform (alexandria:required-argument :items)))
  (:since "3.17.0")
  (:documentation "A workspace diagnostic report.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-diagnostic-report-partial-result
    common-lisp:nil
  ((items :type
    (lem-language-server/protocol-generator::lsp-array workspace-document-diagnostic-report)
    :initform (alexandria:required-argument :items)))
  (:since "3.17.0")
  (:documentation "A partial result for a workspace diagnostic report.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-open-notebook-document-params
    common-lisp:nil
  ((notebookdocument :type notebook-document :initform
    (alexandria:required-argument :notebookdocument) :documentation
    "The notebook document that got opened.")
   (celltext-documents :type (lem-language-server/protocol-generator::lsp-array text-document-item)
    :initform (alexandria:required-argument :celltext-documents) :documentation
    "The text documents that represent the content
of a notebook cell."))
  (:since "3.17.0")
  (:documentation "The params sent in an open notebook document notification.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-notebook-document-params
    common-lisp:nil
  ((notebookdocument :type versioned-notebook-document-identifier :initform
    (alexandria:required-argument :notebookdocument) :documentation
    "The notebook document that did change. The version number points
to the version after all provided changes have been applied. If
only the text document content of a cell changes the notebook version
doesn't necessarily have to change.")
   (change :type notebook-document-change-event :initform (alexandria:required-argument :change)
    :documentation "The actual changes to the notebook document.

The changes describe single state changes to the notebook document.
So if there are two changes c1 (at array index 0) and c2 (at array
index 1) for a notebook in state S then c1 moves the notebook from
S to S' and c2 from S' to S''. So c1 is computed on the state S and
c2 is computed on the state S'.

To mirror the content of a notebook using change events use the following approach:
- start with the same initial content
- apply the 'notebookDocument/didChange' notifications in the order you receive them.
- apply the `NotebookChangeEvent`s in a single notification in the order
  you receive them."))
  (:since "3.17.0")
  (:documentation "The params sent in a change notebook document notification.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-save-notebook-document-params
    common-lisp:nil
  ((notebookdocument :type notebook-document-identifier :initform
    (alexandria:required-argument :notebookdocument) :documentation
    "The notebook document that got saved."))
  (:since "3.17.0")
  (:documentation "The params sent in a save notebook document notification.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-close-notebook-document-params
    common-lisp:nil
  ((notebookdocument :type notebook-document-identifier :initform
    (alexandria:required-argument :notebookdocument) :documentation
    "The notebook document that got closed.")
   (celltext-documents :type
    (lem-language-server/protocol-generator::lsp-array text-document-identifier) :initform
    (alexandria:required-argument :celltext-documents) :documentation
    "The text documents that represent the content
of a notebook cell that got closed."))
  (:since "3.17.0")
  (:documentation "The params sent in a close notebook document notification.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class registration-params
    common-lisp:nil
  ((registrations :type (lem-language-server/protocol-generator::lsp-array registration) :initform
    (alexandria:required-argument :registrations)))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class unregistration-params
    common-lisp:nil
  ((unregisterations :type (lem-language-server/protocol-generator::lsp-array unregistration)
    :initform (alexandria:required-argument :unregisterations)))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class initialize-params
    (_initialize-params workspace-folders-initialize-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class initialize-result
    common-lisp:nil
  ((capabilities :type server-capabilities :initform (alexandria:required-argument :capabilities)
    :documentation "The capabilities the language server provides.")
   (serverinfo :type
    (lem-language-server/protocol-generator::lsp-interface
     ((name :type lem-language-server/protocol-generator::lsp-string :initform
       (alexandria:required-argument :name) :documentation
       "The name of the server as defined by the server.")
      (version :type lem-language-server/protocol-generator::lsp-string :documentation
       "The server's version as defined by the server.")))
    :since "3.15.0" :documentation "Information about the server.

@since 3.15.0"))
  (:documentation "The result returned from an initialize request.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class initialize-error
    common-lisp:nil
  ((retry :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :retry) :documentation
    "Indicates whether the client execute the following retry logic:
(1) show the message provided by the ResponseError to the user
(2) user selects retry or cancel
(3) if user selected retry the initialize method is sent again."))
  (:documentation "The data type of the ResponseError if the
initialize request fails.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class initialized-params
    common-lisp:nil
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-configuration-params
    common-lisp:nil
  ((settings :type l-s-p-any :initform (alexandria:required-argument :settings) :documentation
    "The actual changed settings"))
  (:documentation "The parameters of a change configuration notification.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-configuration-registration-options
    common-lisp:nil
  ((section :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    (lem-language-server/protocol-generator::lsp-array
                     lem-language-server/protocol-generator::lsp-string))))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-message-params
    common-lisp:nil
  ((type :type message-type :initform (alexandria:required-argument :type) :documentation
    "The message type. See {@link MessageType}")
   (message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message) :documentation "The actual message."))
  (:documentation "The parameters of a notification message.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-message-request-params
    common-lisp:nil
  ((type :type message-type :initform (alexandria:required-argument :type) :documentation
    "The message type. See {@link MessageType}")
   (message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message) :documentation "The actual message.")
   (actions :type (lem-language-server/protocol-generator::lsp-array message-action-item)
    :documentation "The message action items to present."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class message-action-item
    common-lisp:nil
  ((title :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :title) :documentation
    "A short title like 'Retry', 'Open Log' etc."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class log-message-params
    common-lisp:nil
  ((type :type message-type :initform (alexandria:required-argument :type) :documentation
    "The message type. See {@link MessageType}")
   (message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message) :documentation "The actual message."))
  (:documentation "The log message parameters.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-open-text-document-params
    common-lisp:nil
  ((textdocument :type text-document-item :initform (alexandria:required-argument :textdocument)
    :documentation "The document that was opened."))
  (:documentation "The parameters sent in an open text document notification")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-text-document-params
    common-lisp:nil
  ((textdocument :type versioned-text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation
    "The document that did change. The version number points
to the version after all provided content changes have
been applied.")
   (contentchanges :type
    (lem-language-server/protocol-generator::lsp-array text-document-content-change-event)
    :initform (alexandria:required-argument :contentchanges) :documentation
    "The actual content changes. The content changes describe single state changes
to the document. So if there are two content changes c1 (at array index 0) and
c2 (at array index 1) for a document in state S then c1 moves the document from
S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
on the state S'.

To mirror the content of a document using change events use the following approach:
- start with the same initial content
- apply the 'textDocument/didChange' notifications in the order you receive them.
- apply the `TextDocumentContentChangeEvent`s in a single notification in the order
  you receive them."))
  (:documentation "The change text document notification's parameters.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-change-registration-options
    (text-document-registration-options)
  ((synckind :type text-document-sync-kind :initform (alexandria:required-argument :synckind)
    :documentation "How documents are synced to the server."))
  (:documentation "Describe options to be used when registered for text document change events.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-close-text-document-params
    common-lisp:nil
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document that was closed."))
  (:documentation "The parameters sent in a close text document notification")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-save-text-document-params
    common-lisp:nil
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document that was saved.")
   (text :type lem-language-server/protocol-generator::lsp-string :documentation
    "Optional the content when saved. Depends on the includeText value
when the save notification was requested."))
  (:documentation "The parameters sent in a save text document notification")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-save-registration-options
    (text-document-registration-options save-options)
  common-lisp:nil
  (:documentation "Save registration options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class will-save-text-document-params
    common-lisp:nil
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document that will be saved.")
   (reason :type text-document-save-reason :initform (alexandria:required-argument :reason)
    :documentation "The 'TextDocumentSaveReason'."))
  (:documentation "The parameters sent in a will save text document notification.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-edit
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range of the text document to be manipulated. To insert
text into a document create a range where start === end.")
   (newtext :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :newtext) :documentation
    "The string to be inserted. For delete operations use an
empty string."))
  (:documentation "A text edit applicable to a text document.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-watched-files-params
    common-lisp:nil
  ((changes :type (lem-language-server/protocol-generator::lsp-array file-event) :initform
    (alexandria:required-argument :changes) :documentation "The actual file events."))
  (:documentation "The watched files change notification's parameters.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-watched-files-registration-options
    common-lisp:nil
  ((watchers :type (lem-language-server/protocol-generator::lsp-array file-system-watcher)
    :initform (alexandria:required-argument :watchers) :documentation "The watchers to register."))
  (:documentation "Describe options to be used when registered for text document change events.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class publish-diagnostics-params
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation
    "The URI for which diagnostic information is reported.")
   (version :type lem-language-server/protocol-generator::lsp-integer :since "3.15.0"
    :documentation "Optional the version number of the document the diagnostics are published for.

@since 3.15.0")
   (diagnostics :type (lem-language-server/protocol-generator::lsp-array diagnostic) :initform
    (alexandria:required-argument :diagnostics) :documentation
    "An array of diagnostic information items."))
  (:documentation "The publish diagnostic notification's parameters.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-params
    (text-document-position-params work-done-progress-params partial-result-params)
  ((context :type completion-context :documentation
    "The completion context. This is only available it the client specifies
to send this using the client capability `textDocument.completion.contextSupport === true`"))
  (:documentation "Completion parameters")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-item
    common-lisp:nil
  ((label :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :label) :documentation "The label of this completion item.

The label property is also by default the text that
is inserted when selecting this completion.

If label details are provided the label itself should
be an unqualified name of the completion item.")
   (labeldetails :type completion-item-label-details :since "3.17.0" :documentation
    "Additional details for the label

@since 3.17.0")
   (kind :type completion-item-kind :documentation
    "The kind of this completion item. Based of the kind
an icon is chosen by the editor.")
   (tags :type (lem-language-server/protocol-generator::lsp-array completion-item-tag) :since
    "3.15.0" :documentation "Tags for this completion item.

@since 3.15.0")
   (detail :type lem-language-server/protocol-generator::lsp-string :documentation
    "A human-readable string with additional information
about this item, like type or symbol information.")
   (documentation :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string markup-content)
    :documentation "A human-readable string that represents a doc-comment.")
   (deprecated :type lem-language-server/protocol-generator::lsp-boolean :deprecated
    "Use `tags` instead." :documentation "Indicates if this item is deprecated.
@deprecated Use `tags` instead.")
   (preselect :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Select this item when showing.

*Note* that only one completion item can be selected and that the
tool / client decides which item that is. The rule is that the *first*
item of those that match best is selected.")
   (sorttext :type lem-language-server/protocol-generator::lsp-string :documentation
    "A string that should be used when comparing this item
with other items. When `falsy` the {@link CompletionItem.label label}
is used.")
   (filtertext :type lem-language-server/protocol-generator::lsp-string :documentation
    "A string that should be used when filtering a set of
completion items. When `falsy` the {@link CompletionItem.label label}
is used.")
   (inserttext :type lem-language-server/protocol-generator::lsp-string :documentation
    "A string that should be inserted into a document when selecting
this completion. When `falsy` the {@link CompletionItem.label label}
is used.

The `insertText` is subject to interpretation by the client side.
Some tools might not take the string literally. For example
VS Code when code complete is requested in this example
`con<cursor position>` and a completion item with an `insertText` of
`console` is provided it will only insert `sole`. Therefore it is
recommended to use `textEdit` instead since it avoids additional client
side interpretation.")
   (inserttext-format :type insert-text-format :documentation
    "The format of the insert text. The format applies to both the
`insertText` property and the `newText` property of a provided
`textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.

Please note that the insertTextFormat doesn't apply to
`additionalTextEdits`.")
   (inserttext-mode :type insert-text-mode :since "3.16.0" :documentation
    "How whitespace and indentation is handled during completion
item insertion. If not provided the clients default value depends on
the `textDocument.completion.insertTextMode` client capability.

@since 3.16.0")
   (textedit :type (common-lisp:or text-edit insert-replace-edit) :since
    "3.16.0 additional type `InsertReplaceEdit`" :documentation
    "An {@link TextEdit edit} which is applied to a document when selecting
this completion. When an edit is provided the value of
{@link CompletionItem.insertText insertText} is ignored.

Most editors support two different operations when accepting a completion
item. One is to insert a completion text and the other is to replace an
existing text with a completion text. Since this can usually not be
predetermined by a server it can report both ranges. Clients need to
signal support for `InsertReplaceEdits` via the
`textDocument.completion.insertReplaceSupport` client capability
property.

*Note 1:* The text edit's range as well as both ranges from an insert
replace edit must be a [single line] and they must contain the position
at which completion has been requested.
*Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
must be a prefix of the edit's replace range, that means it must be
contained and starting at the same position.

@since 3.16.0 additional type `InsertReplaceEdit`")
   (textedit-text :type lem-language-server/protocol-generator::lsp-string :since "3.17.0"
    :documentation "The edit text used if the completion item is part of a CompletionList and
CompletionList defines an item default for the text edit range.

Clients will only honor this property if they opt into completion list
item defaults using the capability `completionList.itemDefaults`.

If not provided and a list's default range is provided the label
property is used as a text.

@since 3.17.0")
   (additionaltext-edits :type (lem-language-server/protocol-generator::lsp-array text-edit)
    :documentation
    "An optional array of additional {@link TextEdit text edits} that are applied when
selecting this completion. Edits must not overlap (including the same insert position)
with the main {@link CompletionItem.textEdit edit} nor with themselves.

Additional text edits should be used to change text unrelated to the current cursor position
(for example adding an import statement at the top of the file if the completion item will
insert an unqualified type).")
   (commitcharacters :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :documentation
    "An optional set of characters that when pressed while this completion is active will accept it first and
then type that character. *Note* that all commit characters should have `length=1` and that superfluous
characters will be ignored.")
   (command :type command :documentation
    "An optional {@link Command command} that is executed *after* inserting this completion. *Note* that
additional modifications to the current document should be described with the
{@link CompletionItem.additionalTextEdits additionalTextEdits}-property.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved on a completion item between a
{@link CompletionRequest} and a {@link CompletionResolveRequest}."))
  (:documentation "A completion item represents a text snippet that is
proposed to complete text that is being typed.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-list
    common-lisp:nil
  ((isincomplete :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :isincomplete) :documentation
    "This list it not complete. Further typing results in recomputing this list.

Recomputed lists have all their items replaced (not appended) in the
incomplete completion sessions.")
   (itemdefaults :type
    (lem-language-server/protocol-generator::lsp-interface
     ((commitcharacters :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :since "3.17.0" :documentation "A default commit character set.

@since 3.17.0")
      (editrange :type
       (common-lisp:or range
                       (lem-language-server/protocol-generator::lsp-interface
                        ((insert :type range :initform (alexandria:required-argument :insert))
                         (replace :type range :initform (alexandria:required-argument :replace)))))
       :since "3.17.0" :documentation "A default edit range.

@since 3.17.0")
      (inserttext-format :type insert-text-format :since "3.17.0" :documentation
       "A default insert text format.

@since 3.17.0")
      (inserttext-mode :type insert-text-mode :since "3.17.0" :documentation
       "A default insert text mode.

@since 3.17.0")
      (data :type l-s-p-any :since "3.17.0" :documentation "A default data value.

@since 3.17.0")))
    :since "3.17.0" :documentation
    "In many cases the items of an actual completion result share the same
value for properties like `commitCharacters` or the range of a text
edit. A completion list can therefore define item defaults which will
be used if a completion item itself doesn't specify the value.

If a completion list specifies a default value and a completion item
also specifies a corresponding value the one from the item is used.

Servers are only allowed to return default values if the client
signals support for this via the `completionList.itemDefaults`
capability.

@since 3.17.0")
   (items :type (lem-language-server/protocol-generator::lsp-array completion-item) :initform
    (alexandria:required-argument :items) :documentation "The completion items."))
  (:documentation
   "Represents a collection of {@link CompletionItem completion items} to be presented
in the editor.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-registration-options
    (text-document-registration-options completion-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CompletionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class hover-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link HoverRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class hover
    common-lisp:nil
  ((contents :type
    (common-lisp:or markup-content marked-string
                    (lem-language-server/protocol-generator::lsp-array marked-string))
    :initform (alexandria:required-argument :contents) :documentation "The hover's content")
   (range :type range :documentation "An optional range inside the text document that is used to
visualize the hover, e.g. by changing the background color."))
  (:documentation "The result of a hover request.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class hover-registration-options
    (text-document-registration-options hover-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link HoverRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help-params
    (text-document-position-params work-done-progress-params)
  ((context :type signature-help-context :since "3.15.0" :documentation
    "The signature help context. This is only available if the client specifies
to send this using the client capability `textDocument.signatureHelp.contextSupport === true`

@since 3.15.0"))
  (:documentation "Parameters for a {@link SignatureHelpRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help
    common-lisp:nil
  ((signatures :type (lem-language-server/protocol-generator::lsp-array signature-information)
    :initform (alexandria:required-argument :signatures) :documentation "One or more signatures.")
   (activesignature :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "The active signature. If omitted or the value lies outside the
range of `signatures` the value defaults to zero or is ignored if
the `SignatureHelp` has no signatures.

Whenever possible implementors should make an active decision about
the active signature and shouldn't rely on a default value.

In future version of the protocol this property might become
mandatory to better express this.")
   (activeparameter :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "The active parameter of the active signature. If omitted or the value
lies outside the range of `signatures[activeSignature].parameters`
defaults to 0 if the active signature has parameters. If
the active signature has no parameters it is ignored.
In future version of the protocol this property might become
mandatory to better express the active parameter if the
active signature does have any."))
  (:documentation "Signature help represents the signature of something
callable. There can be multiple signature but only one
active and only one active parameter.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help-registration-options
    (text-document-registration-options signature-help-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link SignatureHelpRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class definition-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link DefinitionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class definition-registration-options
    (text-document-registration-options definition-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DefinitionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class reference-params
    (text-document-position-params work-done-progress-params partial-result-params)
  ((context :type reference-context :initform (alexandria:required-argument :context)))
  (:documentation "Parameters for a {@link ReferencesRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class reference-registration-options
    (text-document-registration-options reference-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link ReferencesRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-highlight-params
    (text-document-position-params work-done-progress-params partial-result-params)
  common-lisp:nil
  (:documentation "Parameters for a {@link DocumentHighlightRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-highlight
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range this highlight applies to.")
   (kind :type document-highlight-kind :documentation
    "The highlight kind, default is {@link DocumentHighlightKind.Text text}."))
  (:documentation "A document highlight is a range inside a text document which deserves
special attention. Usually a document highlight is visualized by changing
the background color of its range.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-highlight-registration-options
    (text-document-registration-options document-highlight-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentHighlightRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-symbol-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document."))
  (:documentation "Parameters for a {@link DocumentSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class symbol-information
    (base-symbol-information)
  ((deprecated :type lem-language-server/protocol-generator::lsp-boolean :deprecated
    "Use tags instead" :documentation "Indicates if this symbol is deprecated.

@deprecated Use tags instead")
   (location :type location :initform (alexandria:required-argument :location) :documentation
    "The location of this symbol. The location's range is used by a tool
to reveal the location in the editor. If the symbol is selected in the
tool the range's start information is used to position the cursor. So
the range usually spans more than the actual symbol's name and does
normally include things like visibility modifiers.

The range doesn't have to denote a node range in the sense of an abstract
syntax tree. It can therefore not be used to re-construct a hierarchy of
the symbols."))
  (:documentation "Represents information about programming constructs like variables, classes,
interfaces etc.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-symbol
    common-lisp:nil
  ((name :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :name) :documentation
    "The name of this symbol. Will be displayed in the user interface and therefore must not be
an empty string or a string only consisting of white spaces.")
   (detail :type lem-language-server/protocol-generator::lsp-string :documentation
    "More detail for this symbol, e.g the signature of a function.")
   (kind :type symbol-kind :initform (alexandria:required-argument :kind) :documentation
    "The kind of this symbol.")
   (tags :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :since "3.16.0"
    :documentation "Tags for this document symbol.

@since 3.16.0")
   (deprecated :type lem-language-server/protocol-generator::lsp-boolean :deprecated
    "Use tags instead" :documentation "Indicates if this symbol is deprecated.

@deprecated Use tags instead")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range enclosing this symbol not including leading/trailing whitespace but everything else
like comments. This information is typically used to determine if the clients cursor is
inside the symbol to reveal in the symbol in the UI.")
   (selectionrange :type range :initform (alexandria:required-argument :selectionrange)
    :documentation
    "The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
Must be contained by the `range`.")
   (children :type (lem-language-server/protocol-generator::lsp-array document-symbol)
    :documentation "Children of this symbol, e.g. properties of a class."))
  (:documentation "Represents programming constructs like variables, classes, interfaces etc.
that appear in a document. Document symbols can be hierarchical and they
have two ranges: one that encloses its definition and one that points to
its most interesting range, e.g. the range of an identifier.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-symbol-registration-options
    (text-document-registration-options document-symbol-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation
    "The document in which the command was invoked.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range for which the command was invoked.")
   (context :type code-action-context :initform (alexandria:required-argument :context)
    :documentation "Context carrying additional information."))
  (:documentation "The parameters of a {@link CodeActionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class command
    common-lisp:nil
  ((title :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :title) :documentation "Title of the command, like `save`.")
   (command :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :command) :documentation
    "The identifier of the actual command handler.")
   (arguments :type (lem-language-server/protocol-generator::lsp-array l-s-p-any) :documentation
    "Arguments that the command handler should be
invoked with."))
  (:documentation "Represents a reference to a command. Provides a title which
will be used to represent a command in the UI and, optionally,
an array of arguments which will be passed to the command handler
function when invoked.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action
    common-lisp:nil
  ((title :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :title) :documentation
    "A short, human-readable, title for this code action.")
   (kind :type code-action-kind :documentation "The kind of the code action.

Used to filter code actions.")
   (diagnostics :type (lem-language-server/protocol-generator::lsp-array diagnostic) :documentation
    "The diagnostics that this code action resolves.")
   (ispreferred :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation
    "Marks this as a preferred action. Preferred actions are used by the `auto fix` command and can be targeted
by keybindings.

A quick fix should be marked preferred if it properly addresses the underlying error.
A refactoring should be marked preferred if it is the most reasonable choice of actions to take.

@since 3.15.0")
   (disabled :type
    (lem-language-server/protocol-generator::lsp-interface
     ((reason :type lem-language-server/protocol-generator::lsp-string :initform
       (alexandria:required-argument :reason) :documentation
       "Human readable description of why the code action is currently disabled.

This is displayed in the code actions UI.")))
    :since "3.16.0" :documentation "Marks that the code action cannot currently be applied.

Clients should follow the following guidelines regarding disabled code actions:

  - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)
    code action menus.

  - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type
    of code action, such as refactorings.

  - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)
    that auto applies a code action and only disabled code actions are returned, the client should show the user an
    error message with `reason` in the editor.

@since 3.16.0")
   (edit :type workspace-edit :documentation "The workspace edit this code action performs.")
   (command :type command :documentation "A command this code action executes. If a code action
provides an edit and a command, first the edit is
executed and then the command.")
   (data :type l-s-p-any :since "3.16.0" :documentation
    "A data entry field that is preserved on a code action between
a `textDocument/codeAction` and a `codeAction/resolve` request.

@since 3.16.0"))
  (:documentation
   "A code action represents a change that can be performed in code, e.g. to fix a problem or
to refactor code.

A CodeAction must set either `edit` and/or a `command`. If both are supplied, the `edit` is applied first, then the `command` is executed.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action-registration-options
    (text-document-registration-options code-action-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CodeActionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-symbol-params
    (work-done-progress-params partial-result-params)
  ((query :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :query) :documentation
    "A query string to filter symbols by. Clients may send an empty
string here to request all symbols."))
  (:documentation "The parameters of a {@link WorkspaceSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-symbol
    (base-symbol-information)
  ((location :type
    (common-lisp:or location
                    (lem-language-server/protocol-generator::lsp-interface
                     ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
                       (alexandria:required-argument :uri)))))
    :initform (alexandria:required-argument :location) :documentation
    "The location of the symbol. Whether a server is allowed to
return a location without a range depends on the client
capability `workspace.symbol.resolveSupport`.

See SymbolInformation#location for more details.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved on a workspace symbol between a
workspace symbol request and a workspace symbol resolve request."))
  (:since "3.17.0")
  (:documentation "A special workspace symbol that supports locations without a range.

See also SymbolInformation.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-symbol-registration-options
    (workspace-symbol-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link WorkspaceSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation
    "The document to request code lens for."))
  (:documentation "The parameters of a {@link CodeLensRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range in which this code lens is valid. Should only span a single line.")
   (command :type command :documentation "The command this code lens represents.")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved on a code lens item between
a {@link CodeLensRequest} and a [CodeLensResolveRequest]
(#CodeLensResolveRequest)"))
  (:documentation "A code lens represents a {@link Command command} that should be shown along with
source text, like the number of references, a way to run tests, etc.

A code lens is _unresolved_ when no command is associated to it. For performance
reasons the creation of a code lens and resolving should be done in two stages.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens-registration-options
    (text-document-registration-options code-lens-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link CodeLensRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-link-params
    (work-done-progress-params partial-result-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation
    "The document to provide document links for."))
  (:documentation "The parameters of a {@link DocumentLinkRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-link
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range this link applies to.")
   (target :type lem-language-server/protocol-generator::lsp-string :documentation
    "The uri this link points to. If missing a resolve request is sent later.")
   (tooltip :type lem-language-server/protocol-generator::lsp-string :since "3.15.0" :documentation
    "The tooltip text when you hover over this link.

If a tooltip is provided, is will be displayed in a string that includes instructions on how to
trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
user settings, and localization.

@since 3.15.0")
   (data :type l-s-p-any :documentation
    "A data entry field that is preserved on a document link between a
DocumentLinkRequest and a DocumentLinkResolveRequest."))
  (:documentation
   "A document link is a range in a text document that links to an internal or external resource, like another
text document or a web site.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-link-registration-options
    (text-document-registration-options document-link-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentLinkRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-formatting-params
    (work-done-progress-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document to format.")
   (options :type formatting-options :initform (alexandria:required-argument :options)
    :documentation "The format options."))
  (:documentation "The parameters of a {@link DocumentFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-formatting-registration-options
    (text-document-registration-options document-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-range-formatting-params
    (work-done-progress-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document to format.")
   (range :type range :initform (alexandria:required-argument :range) :documentation
    "The range to format")
   (options :type formatting-options :initform (alexandria:required-argument :options)
    :documentation "The format options"))
  (:documentation "The parameters of a {@link DocumentRangeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-range-formatting-registration-options
    (text-document-registration-options document-range-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentRangeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-on-type-formatting-params
    common-lisp:nil
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document to format.")
   (position :type position :initform (alexandria:required-argument :position) :documentation
    "The position around which the on type formatting should happen.
This is not necessarily the exact position where the character denoted
by the property `ch` got typed.")
   (ch :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :ch) :documentation
    "The character that has been typed that triggered the formatting
on type request. That is not necessarily the last character that
got inserted into the document since the client could auto insert
characters as well (e.g. like automatic brace completion).")
   (options :type formatting-options :initform (alexandria:required-argument :options)
    :documentation "The formatting options."))
  (:documentation "The parameters of a {@link DocumentOnTypeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-on-type-formatting-registration-options
    (text-document-registration-options document-on-type-formatting-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link DocumentOnTypeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-params
    (work-done-progress-params)
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The document to rename.")
   (position :type position :initform (alexandria:required-argument :position) :documentation
    "The position at which this request was sent.")
   (newname :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :newname) :documentation
    "The new name of the symbol. If the given name is not valid the
request must return a {@link ResponseError} with an
appropriate message set."))
  (:documentation "The parameters of a {@link RenameRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-registration-options
    (text-document-registration-options rename-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link RenameRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class prepare-rename-params
    (text-document-position-params work-done-progress-params)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class execute-command-params
    (work-done-progress-params)
  ((command :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :command) :documentation
    "The identifier of the actual command handler.")
   (arguments :type (lem-language-server/protocol-generator::lsp-array l-s-p-any) :documentation
    "Arguments that the command should be invoked with."))
  (:documentation "The parameters of a {@link ExecuteCommandRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class execute-command-registration-options
    (execute-command-options)
  common-lisp:nil
  (:documentation "Registration options for a {@link ExecuteCommandRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class apply-workspace-edit-params
    common-lisp:nil
  ((label :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional label of the workspace edit. This label is
presented in the user interface for example on an undo
stack to undo the workspace edit.")
   (edit :type workspace-edit :initform (alexandria:required-argument :edit) :documentation
    "The edits to apply."))
  (:documentation "The parameters passed via a apply workspace edit request.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class apply-workspace-edit-result
    common-lisp:nil
  ((applied :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :applied) :documentation
    "Indicates whether the edit was applied or not.")
   (failurereason :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional textual description for why the edit was not applied.
This may be used by the server for diagnostic logging or to provide
a suitable error for a request that triggered the edit.")
   (failedchange :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "Depending on the client's failure handling strategy `failedChange` might
contain the index of the change that failed. This property is only available
if the client signals a `failureHandlingStrategy` in its client capabilities."))
  (:since "3.17 renamed from ApplyWorkspaceEditResponse")
  (:documentation "The result returned from the apply workspace edit request.

@since 3.17 renamed from ApplyWorkspaceEditResponse")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-begin
    common-lisp:nil
  ((kind :type "begin" :initform (alexandria:required-argument :kind))
   (title :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :title) :documentation
    "Mandatory title of the progress operation. Used to briefly inform about
the kind of operation being performed.

Examples: \"Indexing\" or \"Linking dependencies\".")
   (cancellable :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Controls if a cancel button should show to allow the user to cancel the
long running operation. Clients that don't support cancellation are allowed
to ignore the setting.")
   (message :type lem-language-server/protocol-generator::lsp-string :documentation
    "Optional, more detailed associated progress message. Contains
complementary information to the `title`.

Examples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".
If unset, the previous progress message (if any) is still valid.")
   (percentage :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "Optional progress percentage to display (value 100 is considered 100%).
If not provided infinite progress is assumed and clients are allowed
to ignore the `percentage` value in subsequent in report notifications.

The value should be steadily rising. Clients are free to ignore values
that are not following this rule. The value range is [0, 100]."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-report
    common-lisp:nil
  ((kind :type "report" :initform (alexandria:required-argument :kind))
   (cancellable :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Controls enablement state of a cancel button.

Clients that don't support cancellation or don't support controlling the button's
enablement state are allowed to ignore the property.")
   (message :type lem-language-server/protocol-generator::lsp-string :documentation
    "Optional, more detailed associated progress message. Contains
complementary information to the `title`.

Examples: \"3/25 files\", \"project/src/module2\", \"node_modules/some_dep\".
If unset, the previous progress message (if any) is still valid.")
   (percentage :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "Optional progress percentage to display (value 100 is considered 100%).
If not provided infinite progress is assumed and clients are allowed
to ignore the `percentage` value in subsequent in report notifications.

The value should be steadily rising. Clients are free to ignore values
that are not following this rule. The value range is [0, 100]"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-end
    common-lisp:nil
  ((kind :type "end" :initform (alexandria:required-argument :kind))
   (message :type lem-language-server/protocol-generator::lsp-string :documentation
    "Optional, a final message indicating to for example indicate the outcome
of the operation."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class set-trace-params
    common-lisp:nil
  ((value :type trace-values :initform (alexandria:required-argument :value)))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class log-trace-params
    common-lisp:nil
  ((message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message))
   (verbose :type lem-language-server/protocol-generator::lsp-string))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class cancel-params
    common-lisp:nil
  ((id :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :id) :documentation "The request id to cancel."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class progress-params
    common-lisp:nil
  ((token :type progress-token :initform (alexandria:required-argument :token) :documentation
    "The progress token provided by the client or server.")
   (value :type l-s-p-any :initform (alexandria:required-argument :value) :documentation
    "The progress data."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-position-params
    common-lisp:nil
  ((textdocument :type text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document.")
   (position :type position :initform (alexandria:required-argument :position) :documentation
    "The position inside the text document."))
  (:documentation
   "A parameter literal used in requests to pass a text document and a position inside that
document.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class work-done-progress-params
    common-lisp:nil
  ((workdone-token :type progress-token :documentation
    "An optional token that a server can use to report work done progress."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class partial-result-params
    common-lisp:nil
  ((partialresult-token :type progress-token :documentation
    "An optional token that a server can use to report partial results (e.g. streaming) to
the client."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class location-link
    common-lisp:nil
  ((originselection-range :type range :documentation "Span of the origin of this link.

Used as the underlined span for mouse interaction. Defaults to the word range at
the definition position.")
   (targeturi :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :targeturi) :documentation
    "The target resource identifier of this link.")
   (targetrange :type range :initform (alexandria:required-argument :targetrange) :documentation
    "The full target range of this link. If the target for example is a symbol then target range is the
range enclosing this symbol not including leading/trailing whitespace but everything else
like comments. This information is typically used to highlight the range in the editor.")
   (targetselection-range :type range :initform
    (alexandria:required-argument :targetselection-range) :documentation
    "The range that should be selected and revealed when this link is being followed, e.g the name of a function.
Must be contained by the `targetRange`. See also `DocumentSymbol#range`"))
  (:documentation
   "Represents the connection of two locations. Provides additional metadata over normal {@link Location locations},
including an origin range.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class range
    common-lisp:nil
  ((start :type position :initform (alexandria:required-argument :start) :documentation
    "The range's start position.")
   (end :type position :initform (alexandria:required-argument :end) :documentation
    "The range's end position."))
  (:documentation "A range in a text document expressed as (zero-based) start and end positions.

If you want to specify a range that contains a line including the line ending
character(s) then use an end position denoting the start of the next line.
For example:
```ts
{
    start: { line: 5, character: 23 }
    end : { line 6, character : 0 }
}
```")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class implementation-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class static-registration-options
    common-lisp:nil
  ((id :type lem-language-server/protocol-generator::lsp-string :documentation
    "The id used to register the request. The id can be used to deregister
the request again. See also Registration#id."))
  (:documentation "Static registration options to be returned in the initialize
request.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-definition-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-folders-change-event
    common-lisp:nil
  ((added :type (lem-language-server/protocol-generator::lsp-array workspace-folder) :initform
    (alexandria:required-argument :added) :documentation "The array of added workspace folders")
   (removed :type (lem-language-server/protocol-generator::lsp-array workspace-folder) :initform
    (alexandria:required-argument :removed) :documentation
    "The array of the removed workspace folders"))
  (:documentation "The workspace folder change event.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class configuration-item
    common-lisp:nil
  ((scopeuri :type lem-language-server/protocol-generator::lsp-string :documentation
    "The scope to get the configuration section for.")
   (section :type lem-language-server/protocol-generator::lsp-string :documentation
    "The configuration section asked for."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-identifier
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The text document's uri."))
  (:documentation "A literal to identify a text document in the client.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class color
    common-lisp:nil
  ((red :type lem-language-server/protocol-generator::lsp-decimal :initform
    (alexandria:required-argument :red) :documentation
    "The red component of this color in the range [0-1].")
   (green :type lem-language-server/protocol-generator::lsp-decimal :initform
    (alexandria:required-argument :green) :documentation
    "The green component of this color in the range [0-1].")
   (blue :type lem-language-server/protocol-generator::lsp-decimal :initform
    (alexandria:required-argument :blue) :documentation
    "The blue component of this color in the range [0-1].")
   (alpha :type lem-language-server/protocol-generator::lsp-decimal :initform
    (alexandria:required-argument :alpha) :documentation
    "The alpha component of this color in the range [0-1]."))
  (:documentation "Represents a color in RGBA space.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-color-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class folding-range-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class declaration-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class position
    common-lisp:nil
  ((line :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :line) :documentation "Line position in a document (zero-based).

If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
If a line number is negative, it defaults to 0.")
   (character :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :character) :documentation
    "Character offset on a line in a document (zero-based).

The meaning of this offset is determined by the negotiated
`PositionEncodingKind`.

If the character value is greater than the line length it defaults back to the
line length."))
  (:since "3.17.0 - support for negotiated position encoding.")
  (:documentation "Position in a text document expressed as zero-based line and character
offset. Prior to 3.17 the offsets were always based on a UTF-16 string
representation. So a string of the form `a𐐀b` the character offset of the
character `a` is 0, the character offset of `𐐀` is 1 and the character
offset of b is 3 since `𐐀` is represented using two code units in UTF-16.
Since 3.17 clients and servers can agree on a different string encoding
representation (e.g. UTF-8). The client announces it's supported encoding
via the client capability [`general.positionEncodings`](#clientCapabilities).
The value is an array of position encodings the client supports, with
decreasing preference (e.g. the encoding at index `0` is the most preferred
one). To stay backwards compatible the only mandatory encoding is UTF-16
represented via the string `utf-16`. The server can pick one of the
encodings offered by the client and signals that encoding back to the
client via the initialize result's property
[`capabilities.positionEncoding`](#serverCapabilities). If the string value
`utf-16` is missing from the client's capability `general.positionEncodings`
servers can safely assume that the client supports UTF-16. If the server
omits the position encoding in its initialize result the encoding defaults
to the string value `utf-16`. Implementation considerations: since the
conversion from one encoding into another requires the content of the
file / line the conversion is best done where the file is read which is
usually on the server side.

Positions are line end character agnostic. So you can not specify a position
that denotes `\\r|\\n` or `\\n|` where `|` represents the character offset.

@since 3.17.0 - support for negotiated position encoding.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class selection-range-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.16.0")
  (:documentation "Call hierarchy options used during static registration.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-options
    (work-done-progress-options)
  ((legend :type semantic-tokens-legend :initform (alexandria:required-argument :legend)
    :documentation "The legend used by the server")
   (range :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                    (lem-language-server/protocol-generator::lsp-interface common-lisp:nil))
    :documentation "Server supports providing semantic tokens for a specific range
of a document.")
   (full :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                    (lem-language-server/protocol-generator::lsp-interface
                     ((delta :type lem-language-server/protocol-generator::lsp-boolean
                       :documentation "The server supports deltas for full documents."))))
    :documentation "Server supports providing semantic tokens for a full document."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-edit
    common-lisp:nil
  ((start :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :start) :documentation "The start offset of the edit.")
   (deletecount :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :deletecount) :documentation "The count of elements to remove.")
   (data :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-uinteger)
    :documentation "The elements to insert."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class linked-editing-range-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-create
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :uri) :documentation
    "A file:// URI for the location of the file/folder being created."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder create.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-edit
    common-lisp:nil
  ((textdocument :type optional-versioned-text-document-identifier :initform
    (alexandria:required-argument :textdocument) :documentation "The text document to change.")
   (edits :type
    (lem-language-server/protocol-generator::lsp-array
     (common-lisp:or text-edit annotated-text-edit))
    :initform (alexandria:required-argument :edits) :since
    "3.16.0 - support for AnnotatedTextEdit. This is guarded using a
client capability."
    :documentation "The edits to be applied.

@since 3.16.0 - support for AnnotatedTextEdit. This is guarded using a
client capability."))
  (:documentation
   "Describes textual changes on a text document. A TextDocumentEdit describes all changes
on a document version Si and after they are applied move the document to version Si+1.
So the creator of a TextDocumentEdit doesn't need to sort the array of edits or do any
kind of ordering. However the edits must be non overlapping.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class create-file
    (resource-operation)
  ((kind :type "create" :initform (alexandria:required-argument :kind) :documentation "A create")
   (uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The resource to create.")
   (options :type create-file-options :documentation "Additional options"))
  (:documentation "Create file operation.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-file
    (resource-operation)
  ((kind :type "rename" :initform (alexandria:required-argument :kind) :documentation "A rename")
   (olduri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :olduri) :documentation "The old (existing) location.")
   (newuri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :newuri) :documentation "The new location.")
   (options :type rename-file-options :documentation "Rename options."))
  (:documentation "Rename file operation")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class delete-file
    (resource-operation)
  ((kind :type "delete" :initform (alexandria:required-argument :kind) :documentation "A delete")
   (uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The file to delete.")
   (options :type delete-file-options :documentation "Delete options."))
  (:documentation "Delete file operation")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class change-annotation
    common-lisp:nil
  ((label :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :label) :documentation
    "A human-readable string describing the actual change. The string
is rendered prominent in the user interface.")
   (needsconfirmation :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "A flag which indicates that user confirmation is needed
before applying the change.")
   (description :type lem-language-server/protocol-generator::lsp-string :documentation
    "A human-readable string which is rendered less prominent in
the user interface."))
  (:since "3.16.0")
  (:documentation "Additional information that describes document changes.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-filter
    common-lisp:nil
  ((scheme :type lem-language-server/protocol-generator::lsp-string :documentation
    "A Uri scheme like `file` or `untitled`.")
   (pattern :type file-operation-pattern :initform (alexandria:required-argument :pattern)
    :documentation "The actual file operation pattern."))
  (:since "3.16.0")
  (:documentation "A filter to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-rename
    common-lisp:nil
  ((olduri :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :olduri) :documentation
    "A file:// URI for the original location of the file/folder being renamed.")
   (newuri :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :newuri) :documentation
    "A file:// URI for the new location of the file/folder being renamed."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder rename.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-delete
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :uri) :documentation
    "A file:// URI for the location of the file/folder being deleted."))
  (:since "3.16.0")
  (:documentation "Represents information on a file/folder delete.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class moniker-options
    (work-done-progress-options)
  common-lisp:nil
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Type hierarchy options used during static registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-context
    common-lisp:nil
  ((frameid :type lem-language-server/protocol-generator::lsp-integer :initform
    (alexandria:required-argument :frameid) :documentation
    "The stack frame (as a DAP Id) where the execution has stopped.")
   (stoppedlocation :type range :initform (alexandria:required-argument :stoppedlocation)
    :documentation "The document range where execution has stopped.
Typically the end position of the range denotes the line where the inline values are shown."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-text
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The document range for which the inline value applies.")
   (text :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :text) :documentation "The text of the inline value."))
  (:since "3.17.0")
  (:documentation "Provide inline value as text.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-variable-lookup
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The document range for which the inline value applies.
The range is used to extract the variable name from the underlying document.")
   (variablename :type lem-language-server/protocol-generator::lsp-string :documentation
    "If specified the name of the variable to look up.")
   (casesensitive-lookup :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :casesensitive-lookup) :documentation
    "How to perform the lookup."))
  (:since "3.17.0")
  (:documentation "Provide inline value through a variable lookup.
If only a range is specified, the variable name will be extracted from the underlying document.
An optional variable name can be used to override the extracted name.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-evaluatable-expression
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The document range for which the inline value applies.
The range is used to extract the evaluatable expression from the underlying document.")
   (expression :type lem-language-server/protocol-generator::lsp-string :documentation
    "If specified the expression overrides the extracted expression."))
  (:since "3.17.0")
  (:documentation "Provide an inline value through an expression evaluation.
If only a range is specified, the expression will be extracted from the underlying document.
An optional expression can be used to override the extracted expression.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-options
    (work-done-progress-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Inline value options used during static registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-label-part
    common-lisp:nil
  ((value :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :value) :documentation "The value of this label part.")
   (tooltip :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string markup-content)
    :documentation "The tooltip text when you hover over this label part. Depending on
the client capability `inlayHint.resolveSupport` clients might resolve
this property late using the resolve request.")
   (location :type location :documentation "An optional source code location that represents this
label part.

The editor will use this location for the hover and for code navigation
features: This part will become a clickable link that resolves to the
definition of the symbol at the given location (not necessarily the
location itself), it shows the hover that shows at the given location,
and it shows a context menu with further code navigation commands.

Depending on the client capability `inlayHint.resolveSupport` clients
might resolve this property late using the resolve request.")
   (command :type command :documentation "An optional command for this label part.

Depending on the client capability `inlayHint.resolveSupport` clients
might resolve this property late using the resolve request."))
  (:since "3.17.0")
  (:documentation "An inlay hint label part allows for interactive and composite labels
of inlay hints.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class markup-content
    common-lisp:nil
  ((kind :type markup-kind :initform (alexandria:required-argument :kind) :documentation
    "The type of the Markup")
   (value :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :value) :documentation "The content itself"))
  (:documentation
   "A `MarkupContent` literal represents a string value which content is interpreted base on its
kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.

If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

Here is an example how such a string can be constructed using JavaScript / TypeScript:
```ts
let markdown: MarkdownContent = {
 kind: MarkupKind.Markdown,
 value: [
   '# Header',
   'Some text',
   '```typescript',
   'someCode();',
   '```'
 ].join('\\n')
};
```

*Please Note* that clients might sanitize the return markdown. A client could decide to
remove HTML from the markdown to avoid script execution.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-options
    (work-done-progress-options)
  ((resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The server provides support to resolve additional
information for an inlay hint item."))
  (:since "3.17.0")
  (:documentation "Inlay hint options used during static registration.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class related-full-document-diagnostic-report
    (full-document-diagnostic-report)
  ((relateddocuments :type
    (lem-language-server/protocol-generator::lsp-map document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :since "3.17.0" :documentation "Diagnostics of related documents. This information is useful
in programming languages where code in a file A can generate
diagnostics in a file B which A depends on. An example of
such a language is C/C++ where marco definitions in a file
a.cpp and result in errors in a header file b.hpp.

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "A full diagnostic report with a set of related documents.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class related-unchanged-document-diagnostic-report
    (unchanged-document-diagnostic-report)
  ((relateddocuments :type
    (lem-language-server/protocol-generator::lsp-map document-uri
     (common-lisp:or full-document-diagnostic-report unchanged-document-diagnostic-report))
    :since "3.17.0" :documentation "Diagnostics of related documents. This information is useful
in programming languages where code in a file A can generate
diagnostics in a file B which A depends on. An example of
such a language is C/C++ where marco definitions in a file
a.cpp and result in errors in a header file b.hpp.

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "An unchanged diagnostic report with a set of related documents.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class full-document-diagnostic-report
    common-lisp:nil
  ((kind :type "full" :initform (alexandria:required-argument :kind) :documentation
    "A full document diagnostic report.")
   (resultid :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional result id. If provided it will
be sent on the next diagnostic request for the
same document.")
   (items :type (lem-language-server/protocol-generator::lsp-array diagnostic) :initform
    (alexandria:required-argument :items) :documentation "The actual items."))
  (:since "3.17.0")
  (:documentation "A diagnostic report with a full set of problems.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class unchanged-document-diagnostic-report
    common-lisp:nil
  ((kind :type "unchanged" :initform (alexandria:required-argument :kind) :documentation
    "A document diagnostic report indicating
no changes to the last result. A server can
only return `unchanged` if result ids are
provided.")
   (resultid :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :resultid) :documentation
    "A result id which will be sent on the next
diagnostic request for the same document."))
  (:since "3.17.0")
  (:documentation "A diagnostic report indicating that the last returned
report is still accurate.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-options
    (work-done-progress-options)
  ((identifier :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional identifier under which the diagnostics are
managed by the client.")
   (interfile-dependencies :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :interfile-dependencies) :documentation
    "Whether the language has inter file dependencies meaning that
editing code in one file can result in a different diagnostic
set in another file. Inter file dependencies are common for
most programming languages and typically uncommon for linters.")
   (workspacediagnostics :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :workspacediagnostics) :documentation
    "The server provides support for workspace diagnostics as well."))
  (:since "3.17.0")
  (:documentation "Diagnostic options.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class previous-result-id
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The URI for which the client knowns a
result id.")
   (value :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :value) :documentation "The value of the previous result id."))
  (:since "3.17.0")
  (:documentation "A previous result id in a workspace pull request.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :uri) :documentation "The notebook document's uri.")
   (notebooktype :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :notebooktype) :documentation "The type of the notebook.")
   (version :type lem-language-server/protocol-generator::lsp-integer :initform
    (alexandria:required-argument :version) :documentation
    "The version number of this document (it will increase after each
change, including undo/redo).")
   (metadata :type l-s-p-object :documentation "Additional metadata stored with the notebook
document.

Note: should always be an object literal (e.g. LSPObject)")
   (cells :type (lem-language-server/protocol-generator::lsp-array notebook-cell) :initform
    (alexandria:required-argument :cells) :documentation "The cells of a notebook."))
  (:since "3.17.0")
  (:documentation "A notebook document.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-item
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The text document's uri.")
   (languageid :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :languageid) :documentation
    "The text document's language identifier.")
   (version :type lem-language-server/protocol-generator::lsp-integer :initform
    (alexandria:required-argument :version) :documentation
    "The version number of this document (it will increase after each
change, including undo/redo).")
   (text :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :text) :documentation "The content of the opened text document."))
  (:documentation "An item to transfer a text document from the client to the
server.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class versioned-notebook-document-identifier
    common-lisp:nil
  ((version :type lem-language-server/protocol-generator::lsp-integer :initform
    (alexandria:required-argument :version) :documentation
    "The version number of this notebook document.")
   (uri :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :uri) :documentation "The notebook document's uri."))
  (:since "3.17.0")
  (:documentation "A versioned notebook document identifier.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-change-event
    common-lisp:nil
  ((metadata :type l-s-p-object :documentation "The changed meta data if any.

Note: should always be an object literal (e.g. LSPObject)")
   (cells :type
    (lem-language-server/protocol-generator::lsp-interface
     ((structure :type
       (lem-language-server/protocol-generator::lsp-interface
        ((array :type notebook-cell-array-change :initform (alexandria:required-argument :array)
          :documentation "The change to the cell array.")
         (didopen :type (lem-language-server/protocol-generator::lsp-array text-document-item)
          :documentation "Additional opened cell text documents.")
         (didclose :type
          (lem-language-server/protocol-generator::lsp-array text-document-identifier)
          :documentation "Additional closed cell text documents.")))
       :documentation "Changes to the cell structure to add or
remove cells.")
      (data :type (lem-language-server/protocol-generator::lsp-array notebook-cell) :documentation
       "Changes to notebook cells properties like its
kind, execution summary or metadata.")
      (textcontent :type
       (lem-language-server/protocol-generator::lsp-array
        (lem-language-server/protocol-generator::lsp-interface
         ((document :type versioned-text-document-identifier :initform
           (alexandria:required-argument :document))
          (changes :type
           (lem-language-server/protocol-generator::lsp-array text-document-content-change-event)
           :initform (alexandria:required-argument :changes)))))
       :documentation "Changes to the text content of notebook cells.")))
    :documentation "Changes to cells"))
  (:since "3.17.0")
  (:documentation "A change event for a notebook document.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-identifier
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :uri) :documentation "The notebook document's uri."))
  (:since "3.17.0")
  (:documentation "A literal to identify a notebook document in the client.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class registration
    common-lisp:nil
  ((id :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :id) :documentation
    "The id used to register the request. The id can be used to deregister
the request again.")
   (method :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :method) :documentation
    "The method / capability to register for.")
   (registeroptions :type l-s-p-any :documentation "Options necessary for the registration."))
  (:documentation
   "General parameters to to register for an notification or to register a provider.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class unregistration
    common-lisp:nil
  ((id :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :id) :documentation
    "The id used to unregister the request or notification. Usually an id
provided during the register request.")
   (method :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :method) :documentation "The method to unregister for."))
  (:documentation "General parameters to unregister a request or notification.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class _initialize-params
    (work-done-progress-params)
  ((processid :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-null)
    :initform (alexandria:required-argument :processid) :documentation
    "The process Id of the parent process that started
the server.

Is `null` if the process has not been started by another process.
If the parent process is not alive then the server should exit.")
   (clientinfo :type
    (lem-language-server/protocol-generator::lsp-interface
     ((name :type lem-language-server/protocol-generator::lsp-string :initform
       (alexandria:required-argument :name) :documentation
       "The name of the client as defined by the client.")
      (version :type lem-language-server/protocol-generator::lsp-string :documentation
       "The client's version as defined by the client.")))
    :since "3.15.0" :documentation "Information about the client

@since 3.15.0")
   (locale :type lem-language-server/protocol-generator::lsp-string :since "3.16.0" :documentation
    "The locale the client is currently showing the user interface
in. This must not necessarily be the locale of the operating
system.

Uses IETF language tags as the value's syntax
(See https://en.wikipedia.org/wiki/IETF_language_tag)

@since 3.16.0")
   (rootpath :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    lem-language-server/protocol-generator::lsp-null)
    :deprecated "in favour of rootUri." :documentation "The rootPath of the workspace. Is null
if no folder is open.

@deprecated in favour of rootUri.")
   (rooturi :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-document-uri
                    lem-language-server/protocol-generator::lsp-null)
    :initform (alexandria:required-argument :rooturi) :deprecated "in favour of workspaceFolders."
    :documentation "The rootUri of the workspace. Is null if no
folder is open. If both `rootPath` and `rootUri` are set
`rootUri` wins.

@deprecated in favour of workspaceFolders.")
   (capabilities :type client-capabilities :initform (alexandria:required-argument :capabilities)
    :documentation "The capabilities provided by the client (editor or tool)")
   (initializationoptions :type l-s-p-any :documentation "User provided initialization options.")
   (trace :type trace-values :documentation
    "The initial trace setting. If omitted trace is disabled ('off')."))
  (:documentation "The initialize parameters")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-folders-initialize-params
    common-lisp:nil
  ((workspacefolders :type
    (common-lisp:or (lem-language-server/protocol-generator::lsp-array workspace-folder)
                    lem-language-server/protocol-generator::lsp-null)
    :since "3.6.0" :documentation
    "The workspace folders configured in the client when the server starts.

This property is only available if the client supports workspace folders.
It can be `null` if the client supports workspace folders but none are
configured.

@since 3.6.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class server-capabilities
    common-lisp:nil
  ((positionencoding :type position-encoding-kind :since "3.17.0" :documentation
    "The position encoding the server picked from the encodings offered
by the client via the client capability `general.positionEncodings`.

If the client didn't provide any position encodings the only valid
value that a server can return is 'utf-16'.

If omitted it defaults to 'utf-16'.

@since 3.17.0")
   (textdocument-sync :type (common-lisp:or text-document-sync-options text-document-sync-kind)
    :documentation "Defines how text documents are synced. Is either a detailed structure
defining each notification or for backwards compatibility the
TextDocumentSyncKind number.")
   (notebookdocument-sync :type
    (common-lisp:or notebook-document-sync-options notebook-document-sync-registration-options)
    :since "3.17.0" :documentation "Defines how notebook documents are synced.

@since 3.17.0")
   (completionprovider :type completion-options :documentation
    "The server provides completion support.")
   (hoverprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean hover-options)
    :documentation "The server provides hover support.")
   (signaturehelp-provider :type signature-help-options :documentation
    "The server provides signature help support.")
   (declarationprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean declaration-options
                    declaration-registration-options)
    :documentation "The server provides Goto Declaration support.")
   (definitionprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean definition-options)
    :documentation "The server provides goto definition support.")
   (typedefinition-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean type-definition-options
                    type-definition-registration-options)
    :documentation "The server provides Goto Type Definition support.")
   (implementationprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean implementation-options
                    implementation-registration-options)
    :documentation "The server provides Goto Implementation support.")
   (referencesprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean reference-options)
    :documentation "The server provides find references support.")
   (documenthighlight-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean document-highlight-options)
    :documentation "The server provides document highlight support.")
   (documentsymbol-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean document-symbol-options)
    :documentation "The server provides document symbol support.")
   (codeaction-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean code-action-options)
    :documentation "The server provides code actions. CodeActionOptions may only be
specified if the client states that it supports
`codeActionLiteralSupport` in its initial `initialize` request.")
   (codelens-provider :type code-lens-options :documentation "The server provides code lens.")
   (documentlink-provider :type document-link-options :documentation
    "The server provides document link support.")
   (colorprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean document-color-options
                    document-color-registration-options)
    :documentation "The server provides color provider support.")
   (workspacesymbol-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean workspace-symbol-options)
    :documentation "The server provides workspace symbol support.")
   (documentformatting-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                    document-formatting-options)
    :documentation "The server provides document formatting.")
   (documentrange-formatting-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                    document-range-formatting-options)
    :documentation "The server provides document range formatting.")
   (documenton-type-formatting-provider :type document-on-type-formatting-options :documentation
    "The server provides document formatting on typing.")
   (renameprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean rename-options)
    :documentation "The server provides rename support. RenameOptions may only be
specified if the client states that it supports
`prepareSupport` in its initial `initialize` request.")
   (foldingrange-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean folding-range-options
                    folding-range-registration-options)
    :documentation "The server provides folding provider support.")
   (selectionrange-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean selection-range-options
                    selection-range-registration-options)
    :documentation "The server provides selection range support.")
   (executecommand-provider :type execute-command-options :documentation
    "The server provides execute command support.")
   (callhierarchy-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean call-hierarchy-options
                    call-hierarchy-registration-options)
    :since "3.16.0" :documentation "The server provides call hierarchy support.

@since 3.16.0")
   (linkedediting-range-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                    linked-editing-range-options linked-editing-range-registration-options)
    :since "3.16.0" :documentation "The server provides linked editing range support.

@since 3.16.0")
   (semantictokens-provider :type
    (common-lisp:or semantic-tokens-options semantic-tokens-registration-options) :since "3.16.0"
    :documentation "The server provides semantic tokens support.

@since 3.16.0")
   (monikerprovider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean moniker-options
                    moniker-registration-options)
    :since "3.16.0" :documentation "The server provides moniker support.

@since 3.16.0")
   (typehierarchy-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean type-hierarchy-options
                    type-hierarchy-registration-options)
    :since "3.17.0" :documentation "The server provides type hierarchy support.

@since 3.17.0")
   (inlinevalue-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean inline-value-options
                    inline-value-registration-options)
    :since "3.17.0" :documentation "The server provides inline values.

@since 3.17.0")
   (inlayhint-provider :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-boolean inlay-hint-options
                    inlay-hint-registration-options)
    :since "3.17.0" :documentation "The server provides inlay hints.

@since 3.17.0")
   (diagnosticprovider :type (common-lisp:or diagnostic-options diagnostic-registration-options)
    :since "3.17.0" :documentation "The server has support for pull model diagnostics.

@since 3.17.0")
   (workspace :type
    (lem-language-server/protocol-generator::lsp-interface
     ((workspacefolders :type workspace-folders-server-capabilities :since "3.6.0" :documentation
       "The server supports workspace folder.

@since 3.6.0")
      (fileoperations :type file-operation-options :since "3.16.0" :documentation
       "The server is interested in notifications/requests for operations on files.

@since 3.16.0")))
    :documentation "Workspace specific server capabilities.")
   (experimental :type l-s-p-any :documentation "Experimental server capabilities."))
  (:documentation "Defines the capabilities provided by a language
server.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class versioned-text-document-identifier
    (text-document-identifier)
  ((version :type lem-language-server/protocol-generator::lsp-integer :initform
    (alexandria:required-argument :version) :documentation "The version number of this document."))
  (:documentation "A text document identifier to denote a specific version of a text document.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class save-options
    common-lisp:nil
  ((includetext :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client is supposed to include the content on save."))
  (:documentation "Save options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-event
    common-lisp:nil
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation "The file's uri.")
   (type :type file-change-type :initform (alexandria:required-argument :type) :documentation
    "The change type."))
  (:documentation "An event describing a file change.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-system-watcher
    common-lisp:nil
  ((globpattern :type glob-pattern :initform (alexandria:required-argument :globpattern) :since
    "3.17.0 support for relative patterns." :documentation
    "The glob pattern to watch. See {@link GlobPattern glob pattern} for more detail.

@since 3.17.0 support for relative patterns.")
   (kind :type watch-kind :documentation "The kind of events of interest. If omitted it defaults
to WatchKind.Create | WatchKind.Change | WatchKind.Delete
which is 7."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic
    common-lisp:nil
  ((range :type range :initform (alexandria:required-argument :range) :documentation
    "The range at which the message applies")
   (severity :type diagnostic-severity :documentation
    "The diagnostic's severity. Can be omitted. If omitted it is up to the
client to interpret diagnostics as error, warning, info or hint.")
   (code :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-string)
    :documentation "The diagnostic's code, which usually appear in the user interface.")
   (codedescription :type code-description :since "3.16.0" :documentation
    "An optional property to describe the error code.
Requires the code field (above) to be present/not null.

@since 3.16.0")
   (source :type lem-language-server/protocol-generator::lsp-string :documentation
    "A human-readable string describing the source of this
diagnostic, e.g. 'typescript' or 'super lint'. It usually
appears in the user interface.")
   (message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message) :documentation
    "The diagnostic's message. It usually appears in the user interface")
   (tags :type (lem-language-server/protocol-generator::lsp-array diagnostic-tag) :since "3.15.0"
    :documentation "Additional metadata about the diagnostic.

@since 3.15.0")
   (relatedinformation :type
    (lem-language-server/protocol-generator::lsp-array diagnostic-related-information)
    :documentation "An array of related diagnostic information, e.g. when symbol-names within
a scope collide all definitions can be marked via this property.")
   (data :type l-s-p-any :since "3.16.0" :documentation
    "A data entry field that is preserved between a `textDocument/publishDiagnostics`
notification and `textDocument/codeAction` request.

@since 3.16.0"))
  (:documentation "Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
are only valid in the scope of a resource.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-context
    common-lisp:nil
  ((triggerkind :type completion-trigger-kind :initform (alexandria:required-argument :triggerkind)
    :documentation "How the completion was triggered.")
   (triggercharacter :type lem-language-server/protocol-generator::lsp-string :documentation
    "The trigger character (a single character) that has trigger code complete.
Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`"))
  (:documentation
   "Contains additional information about the context in which a completion request is triggered.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-item-label-details
    common-lisp:nil
  ((detail :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional string which is rendered less prominently directly after {@link CompletionItem.label label},
without any spacing. Should be used for function signatures and type annotations.")
   (description :type lem-language-server/protocol-generator::lsp-string :documentation
    "An optional string which is rendered less prominently after {@link CompletionItem.detail}. Should be used
for fully qualified names and file paths."))
  (:since "3.17.0")
  (:documentation "Additional details for a completion item label.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class insert-replace-edit
    common-lisp:nil
  ((newtext :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :newtext) :documentation "The string to be inserted.")
   (insert :type range :initform (alexandria:required-argument :insert) :documentation
    "The range if the insert is requested")
   (replace :type range :initform (alexandria:required-argument :replace) :documentation
    "The range if the replace is requested."))
  (:since "3.16.0")
  (:documentation "A special text edit to provide an insert and a replace operation.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-options
    (work-done-progress-options)
  ((triggercharacters :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :documentation
    "Most tools trigger completion request automatically without explicitly requesting
it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
starts to type an identifier. For example if the user types `c` in a JavaScript file
code complete will automatically pop up present `console` besides others as a
completion item. Characters that make up identifiers don't need to be listed here.

If code complete should automatically be trigger on characters not being valid inside
an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.")
   (allcommit-characters :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :since "3.2.0" :documentation
    "The list of all possible characters that commit a completion. This field can be used
if clients don't support individual commit characters per completion item. See
`ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`

If a server provides both `allCommitCharacters` and commit characters on an individual
completion item the ones on the completion item win.

@since 3.2.0")
   (resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The server provides support to resolve additional
information for a completion item.")
   (completionitem :type
    (lem-language-server/protocol-generator::lsp-interface
     ((labeldetails-support :type lem-language-server/protocol-generator::lsp-boolean :since
       "3.17.0" :documentation "The server has support for completion item label
details (see also `CompletionItemLabelDetails`) when
receiving a completion item in a resolve call.

@since 3.17.0")))
    :since "3.17.0" :documentation "The server supports the following `CompletionItem` specific
capabilities.

@since 3.17.0"))
  (:documentation "Completion options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class hover-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Hover options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help-context
    common-lisp:nil
  ((triggerkind :type signature-help-trigger-kind :initform
    (alexandria:required-argument :triggerkind) :documentation
    "Action that caused signature help to be triggered.")
   (triggercharacter :type lem-language-server/protocol-generator::lsp-string :documentation
    "Character that caused signature help to be triggered.

This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`")
   (isretrigger :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :isretrigger) :documentation
    "`true` if signature help was already showing when it was triggered.

Retriggers occurs when the signature help is already active and can be caused by actions such as
typing a trigger character, a cursor move, or document content changes.")
   (activesignature-help :type signature-help :documentation "The currently active `SignatureHelp`.

The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
the user navigating through available signatures."))
  (:since "3.15.0")
  (:documentation
   "Additional information about the context in which a signature help request was triggered.

@since 3.15.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-information
    common-lisp:nil
  ((label :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :label) :documentation
    "The label of this signature. Will be shown in
the UI.")
   (documentation :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string markup-content)
    :documentation "The human-readable doc-comment of this signature. Will be shown
in the UI but can be omitted.")
   (parameters :type (lem-language-server/protocol-generator::lsp-array parameter-information)
    :documentation "The parameters of this signature.")
   (activeparameter :type lem-language-server/protocol-generator::lsp-uinteger :since "3.16.0"
    :documentation "The index of the active parameter.

If provided, this is used in place of `SignatureHelp.activeParameter`.

@since 3.16.0"))
  (:documentation "Represents the signature of something callable. A signature
can have a label, like a function-name, a doc-comment, and
a set of parameters.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help-options
    (work-done-progress-options)
  ((triggercharacters :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :documentation "List of characters that trigger signature help automatically.")
   (retriggercharacters :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :since "3.15.0" :documentation "List of characters that re-trigger signature help.

These trigger characters are only active when signature help is already showing. All trigger characters
are also counted as re-trigger characters.

@since 3.15.0"))
  (:documentation "Server Capabilities for a {@link SignatureHelpRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class definition-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Server Capabilities for a {@link DefinitionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class reference-context
    common-lisp:nil
  ((includedeclaration :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :includedeclaration) :documentation
    "Include the declaration of the current symbol."))
  (:documentation "Value-object that contains additional information when
requesting references.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class reference-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Reference options.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-highlight-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Provider options for a {@link DocumentHighlightRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class base-symbol-information
    common-lisp:nil
  ((name :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :name) :documentation "The name of this symbol.")
   (kind :type symbol-kind :initform (alexandria:required-argument :kind) :documentation
    "The kind of this symbol.")
   (tags :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :since "3.16.0"
    :documentation "Tags for this symbol.

@since 3.16.0")
   (containername :type lem-language-server/protocol-generator::lsp-string :documentation
    "The name of the symbol containing this symbol. This information is for
user interface purposes (e.g. to render a qualifier in the user interface
if necessary). It can't be used to re-infer a hierarchy for the document
symbols."))
  (:documentation "A base for all symbol information.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-symbol-options
    (work-done-progress-options)
  ((label :type lem-language-server/protocol-generator::lsp-string :since "3.16.0" :documentation
    "A human-readable string that is shown when multiple outlines trees
are shown for the same document.

@since 3.16.0"))
  (:documentation "Provider options for a {@link DocumentSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action-context
    common-lisp:nil
  ((diagnostics :type (lem-language-server/protocol-generator::lsp-array diagnostic) :initform
    (alexandria:required-argument :diagnostics) :documentation
    "An array of diagnostics known on the client side overlapping the range provided to the
`textDocument/codeAction` request. They are provided so that the server knows which
errors are currently presented to the user for the given range. There is no guarantee
that these accurately reflect the error state of the resource. The primary parameter
to compute code actions is the provided range.")
   (only :type (lem-language-server/protocol-generator::lsp-array code-action-kind) :documentation
    "Requested kind of actions to return.

Actions not of this kind are filtered out by the client before being shown. So servers
can omit computing them.")
   (triggerkind :type code-action-trigger-kind :since "3.17.0" :documentation
    "The reason why code actions were requested.

@since 3.17.0"))
  (:documentation "Contains additional diagnostic information about the context in which
a {@link CodeActionProvider.provideCodeActions code action} is run.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action-options
    (work-done-progress-options)
  ((codeaction-kinds :type (lem-language-server/protocol-generator::lsp-array code-action-kind)
    :documentation "CodeActionKinds that this server may return.

The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
may list out every specific kind they provide.")
   (resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :since "3.16.0"
    :documentation "The server provides support to resolve additional
information for a code action.

@since 3.16.0"))
  (:documentation "Provider options for a {@link CodeActionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-symbol-options
    (work-done-progress-options)
  ((resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :since "3.17.0"
    :documentation "The server provides support to resolve additional
information for a workspace symbol.

@since 3.17.0"))
  (:documentation "Server capabilities for a {@link WorkspaceSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens-options
    (work-done-progress-options)
  ((resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Code lens has a resolve provider as well."))
  (:documentation "Code Lens provider options of a {@link CodeLensRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-link-options
    (work-done-progress-options)
  ((resolveprovider :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Document links have a resolve provider as well."))
  (:documentation "Provider options for a {@link DocumentLinkRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class formatting-options
    common-lisp:nil
  ((tabsize :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :tabsize) :documentation "Size of a tab in spaces.")
   (insertspaces :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :insertspaces) :documentation "Prefer spaces over tabs.")
   (trimtrailing-whitespace :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.15.0" :documentation "Trim trailing whitespace on a line.

@since 3.15.0")
   (insertfinal-newline :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "Insert a newline character at the end of the file if one does not exist.

@since 3.15.0")
   (trimfinal-newlines :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "Trim all newlines after the final newline at the end of the file.

@since 3.15.0"))
  (:documentation "Value-object describing what options formatting should use.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-formatting-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Provider options for a {@link DocumentFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-range-formatting-options
    (work-done-progress-options)
  common-lisp:nil
  (:documentation "Provider options for a {@link DocumentRangeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-on-type-formatting-options
    common-lisp:nil
  ((firsttrigger-character :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :firsttrigger-character) :documentation
    "A character on which formatting should be triggered, like `{`.")
   (moretrigger-character :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :documentation "More trigger characters."))
  (:documentation "Provider options for a {@link DocumentOnTypeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-options
    (work-done-progress-options)
  ((prepareprovider :type lem-language-server/protocol-generator::lsp-boolean :since
    "version 3.12.0" :documentation "Renames should be checked and tested before being executed.

@since version 3.12.0"))
  (:documentation "Provider options for a {@link RenameRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class execute-command-options
    (work-done-progress-options)
  ((commands :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :commands) :documentation
    "The commands to be executed on the server"))
  (:documentation "The server capabilities of a {@link ExecuteCommandRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-legend
    common-lisp:nil
  ((tokentypes :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :tokentypes) :documentation
    "The token types a server uses.")
   (tokenmodifiers :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :tokenmodifiers) :documentation
    "The token modifiers a server uses."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class optional-versioned-text-document-identifier
    (text-document-identifier)
  ((version :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-null)
    :initform (alexandria:required-argument :version) :documentation
    "The version number of this document. If a versioned text document identifier
is sent from the server to the client and the file is not open in the editor
(the server has not received an open notification before) the server can send
`null` to indicate that the version is unknown and the content on disk is the
truth (as specified with document content ownership)."))
  (:documentation
   "A text document identifier to optionally denote a specific version of a text document.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class annotated-text-edit
    (text-edit)
  ((annotationid :type change-annotation-identifier :initform
    (alexandria:required-argument :annotationid) :documentation
    "The actual identifier of the change annotation"))
  (:since "3.16.0.")
  (:documentation "A special text edit with an additional change annotation.

@since 3.16.0.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class resource-operation
    common-lisp:nil
  ((kind :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :kind) :documentation "The resource operation kind.")
   (annotationid :type change-annotation-identifier :since "3.16.0" :documentation
    "An optional annotation identifier describing the operation.

@since 3.16.0"))
  (:documentation "A generic resource operation.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class create-file-options
    common-lisp:nil
  ((overwrite :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Overwrite existing file. Overwrite wins over `ignoreIfExists`")
   (ignoreif-exists :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Ignore if exists."))
  (:documentation "Options to create a file.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-file-options
    common-lisp:nil
  ((overwrite :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Overwrite target if existing. Overwrite wins over `ignoreIfExists`")
   (ignoreif-exists :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Ignores if target exists."))
  (:documentation "Rename file options")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class delete-file-options
    common-lisp:nil
  ((recursive :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Delete the content recursively if a folder is denoted.")
   (ignoreif-not-exists :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Ignore the operation if the file doesn't exist."))
  (:documentation "Delete file options")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-pattern
    common-lisp:nil
  ((glob :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :glob) :documentation
    "The glob pattern to match. Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)")
   (matches :type file-operation-pattern-kind :documentation
    "Whether to match files or folders with this pattern.

Matches both if undefined.")
   (options :type file-operation-pattern-options :documentation
    "Additional options used during matching."))
  (:since "3.16.0")
  (:documentation "A pattern to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-full-document-diagnostic-report
    (full-document-diagnostic-report)
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation
    "The URI for which diagnostic information is reported.")
   (version :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-null)
    :initform (alexandria:required-argument :version) :documentation
    "The version number for which the diagnostics are reported.
If the document is not marked as open `null` can be provided."))
  (:since "3.17.0")
  (:documentation "A full document diagnostic report for a workspace diagnostic result.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-unchanged-document-diagnostic-report
    (unchanged-document-diagnostic-report)
  ((uri :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :uri) :documentation
    "The URI for which diagnostic information is reported.")
   (version :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-null)
    :initform (alexandria:required-argument :version) :documentation
    "The version number for which the diagnostics are reported.
If the document is not marked as open `null` can be provided."))
  (:since "3.17.0")
  (:documentation "An unchanged document diagnostic report for a workspace diagnostic result.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-cell
    common-lisp:nil
  ((kind :type notebook-cell-kind :initform (alexandria:required-argument :kind) :documentation
    "The cell's kind")
   (document :type lem-language-server/protocol-generator::lsp-document-uri :initform
    (alexandria:required-argument :document) :documentation "The URI of the cell's text document
content.")
   (metadata :type l-s-p-object :documentation "Additional metadata stored with the cell.

Note: should always be an object literal (e.g. LSPObject)")
   (executionsummary :type execution-summary :documentation
    "Additional execution summary information
if supported by the client."))
  (:since "3.17.0")
  (:documentation "A notebook cell.

A cell's document URI must be unique across ALL notebook
cells and can therefore be used to uniquely identify a
notebook cell or the cell's text document.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-cell-array-change
    common-lisp:nil
  ((start :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :start) :documentation
    "The start oftest of the cell that changed.")
   (deletecount :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :deletecount) :documentation "The deleted cells")
   (cells :type (lem-language-server/protocol-generator::lsp-array notebook-cell) :documentation
    "The new cells, if any"))
  (:since "3.17.0")
  (:documentation "A change describing how to move a `NotebookCell`
array from state S to S'.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class client-capabilities
    common-lisp:nil
  ((workspace :type workspace-client-capabilities :documentation
    "Workspace specific client capabilities.")
   (textdocument :type text-document-client-capabilities :documentation
    "Text document specific client capabilities.")
   (notebookdocument :type notebook-document-client-capabilities :since "3.17.0" :documentation
    "Capabilities specific to the notebook document support.

@since 3.17.0")
   (window :type window-client-capabilities :documentation "Window specific client capabilities.")
   (general :type general-client-capabilities :since "3.16.0" :documentation
    "General client capabilities.

@since 3.16.0")
   (experimental :type l-s-p-any :documentation "Experimental client capabilities."))
  (:documentation "Defines the capabilities provided by the client.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-sync-options
    common-lisp:nil
  ((openclose :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Open and close notifications are sent to the server. If omitted open close notification should not
be sent.")
   (change :type text-document-sync-kind :documentation
    "Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.")
   (willsave :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "If present will save notifications are sent to the server. If omitted the notification should not be
sent.")
   (willsave-wait-until :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "If present will save wait until requests are sent to the server. If omitted the request should not be
sent.")
   (save :type (common-lisp:or lem-language-server/protocol-generator::lsp-boolean save-options)
    :documentation
    "If present save notifications are sent to the server. If omitted the notification should not be
sent."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-sync-options
    common-lisp:nil
  ((notebookselector :type
    (lem-language-server/protocol-generator::lsp-array
     (common-lisp:or
      (lem-language-server/protocol-generator::lsp-interface
       ((notebook :type
         (common-lisp:or lem-language-server/protocol-generator::lsp-string
                         notebook-document-filter)
         :initform (alexandria:required-argument :notebook) :documentation
         "The notebook to be synced If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
        (cells :type
         (lem-language-server/protocol-generator::lsp-array
          (lem-language-server/protocol-generator::lsp-interface
           ((language :type lem-language-server/protocol-generator::lsp-string :initform
             (alexandria:required-argument :language)))))
         :documentation "The cells of the matching notebook to be synced.")))
      (lem-language-server/protocol-generator::lsp-interface
       ((notebook :type
         (common-lisp:or lem-language-server/protocol-generator::lsp-string
                         notebook-document-filter)
         :documentation "The notebook to be synced If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
        (cells :type
         (lem-language-server/protocol-generator::lsp-array
          (lem-language-server/protocol-generator::lsp-interface
           ((language :type lem-language-server/protocol-generator::lsp-string :initform
             (alexandria:required-argument :language)))))
         :initform (alexandria:required-argument :cells) :documentation
         "The cells of the matching notebook to be synced.")))))
    :initform (alexandria:required-argument :notebookselector) :documentation
    "The notebooks to be synced")
   (save :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether save notification should be forwarded to
the server. Will only be honored if mode === `notebook`."))
  (:since "3.17.0")
  (:documentation "Options specific to a notebook plus its cells
to be synced to the server.

If a selector provides a notebook document
filter but no cell selector all cells of a
matching notebook document will be synced.

If a selector provides no notebook document
filter but only a cell selector all notebook
document that contain at least one matching
cell will be synced.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-sync-registration-options
    (notebook-document-sync-options static-registration-options)
  common-lisp:nil
  (:since "3.17.0")
  (:documentation "Registration options specific to a notebook.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-folders-server-capabilities
    common-lisp:nil
  ((supported :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The server has support for workspace folders")
   (changenotifications :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    lem-language-server/protocol-generator::lsp-boolean)
    :documentation "Whether the server wants to receive workspace folder
change notifications.

If a string is provided the string is treated as an ID
under which the notification is registered on the client
side. The ID can be used to unregister for these events
using the `client/unregisterCapability` request."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-options
    common-lisp:nil
  ((didcreate :type file-operation-registration-options :documentation
    "The server is interested in receiving didCreateFiles notifications.")
   (willcreate :type file-operation-registration-options :documentation
    "The server is interested in receiving willCreateFiles requests.")
   (didrename :type file-operation-registration-options :documentation
    "The server is interested in receiving didRenameFiles notifications.")
   (willrename :type file-operation-registration-options :documentation
    "The server is interested in receiving willRenameFiles requests.")
   (diddelete :type file-operation-registration-options :documentation
    "The server is interested in receiving didDeleteFiles file notifications.")
   (willdelete :type file-operation-registration-options :documentation
    "The server is interested in receiving willDeleteFiles file requests."))
  (:since "3.16.0")
  (:documentation "Options for notifications/requests for user operations on files.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-description
    common-lisp:nil
  ((href :type lem-language-server/protocol-generator::lsp-uri :initform
    (alexandria:required-argument :href) :documentation
    "An URI to open with more information about the diagnostic error."))
  (:since "3.16.0")
  (:documentation "Structure to capture a description for an error code.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-related-information
    common-lisp:nil
  ((location :type location :initform (alexandria:required-argument :location) :documentation
    "The location of this related diagnostic information.")
   (message :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :message) :documentation
    "The message of this related diagnostic information."))
  (:documentation
   "Represents a related message and source code location for a diagnostic. This should be
used to point to code locations that cause or related to a diagnostics, e.g when duplicating
a symbol in a scope.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class parameter-information
    common-lisp:nil
  ((label :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    (lem-language-server/protocol-generator::lsp-tuple
                     lem-language-server/protocol-generator::lsp-uinteger
                     lem-language-server/protocol-generator::lsp-uinteger))
    :initform (alexandria:required-argument :label) :documentation
    "The label of this parameter information.

Either a string or an inclusive start and exclusive end offsets within its containing
signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
string representation as `Position` and `Range` does.

*Note*: a label of type string should be a substring of its containing signature label.
Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.")
   (documentation :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string markup-content)
    :documentation "The human-readable doc-comment of this parameter. Will be shown
in the UI but can be omitted."))
  (:documentation "Represents a parameter of a callable-signature. A parameter can
have a label and a doc-comment.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-cell-text-document-filter
    common-lisp:nil
  ((notebook :type
    (common-lisp:or lem-language-server/protocol-generator::lsp-string notebook-document-filter)
    :initform (alexandria:required-argument :notebook) :documentation
    "A filter that matches against the notebook
containing the notebook cell. If a string
value is provided it matches against the
notebook type. '*' matches every notebook.")
   (language :type lem-language-server/protocol-generator::lsp-string :documentation
    "A language id like `python`.

Will be matched against the language id of the
notebook cell document. '*' matches every language."))
  (:since "3.17.0")
  (:documentation "A notebook cell text document filter denotes a cell text
document by different properties.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-pattern-options
    common-lisp:nil
  ((ignorecase :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The pattern should be matched ignoring casing."))
  (:since "3.16.0")
  (:documentation "Matching options for the file operation pattern.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class execution-summary
    common-lisp:nil
  ((executionorder :type lem-language-server/protocol-generator::lsp-uinteger :initform
    (alexandria:required-argument :executionorder) :documentation
    "A strict monotonically increasing value
indicating the execution order of a cell
inside a notebook.")
   (success :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the execution was successful or
not if known by the client."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-client-capabilities
    common-lisp:nil
  ((applyedit :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports applying batch edits
to the workspace by supporting the request
'workspace/applyEdit'")
   (workspaceedit :type workspace-edit-client-capabilities :documentation
    "Capabilities specific to `WorkspaceEdit`s.")
   (didchange-configuration :type did-change-configuration-client-capabilities :documentation
    "Capabilities specific to the `workspace/didChangeConfiguration` notification.")
   (didchange-watched-files :type did-change-watched-files-client-capabilities :documentation
    "Capabilities specific to the `workspace/didChangeWatchedFiles` notification.")
   (symbol :type workspace-symbol-client-capabilities :documentation
    "Capabilities specific to the `workspace/symbol` request.")
   (executecommand :type execute-command-client-capabilities :documentation
    "Capabilities specific to the `workspace/executeCommand` request.")
   (workspacefolders :type lem-language-server/protocol-generator::lsp-boolean :since "3.6.0"
    :documentation "The client has support for workspace folders.

@since 3.6.0")
   (configuration :type lem-language-server/protocol-generator::lsp-boolean :since "3.6.0"
    :documentation "The client supports `workspace/configuration` requests.

@since 3.6.0")
   (semantictokens :type semantic-tokens-workspace-client-capabilities :since "3.16.0."
    :documentation "Capabilities specific to the semantic token requests scoped to the
workspace.

@since 3.16.0.")
   (codelens :type code-lens-workspace-client-capabilities :since "3.16.0." :documentation
    "Capabilities specific to the code lens requests scoped to the
workspace.

@since 3.16.0.")
   (fileoperations :type file-operation-client-capabilities :documentation
    "The client has support for file notifications/requests for user operations on files.

Since 3.16.0")
   (inlinevalue :type inline-value-workspace-client-capabilities :since "3.17.0." :documentation
    "Capabilities specific to the inline values requests scoped to the
workspace.

@since 3.17.0.")
   (inlayhint :type inlay-hint-workspace-client-capabilities :since "3.17.0." :documentation
    "Capabilities specific to the inlay hint requests scoped to the
workspace.

@since 3.17.0.")
   (diagnostics :type diagnostic-workspace-client-capabilities :since "3.17.0." :documentation
    "Capabilities specific to the diagnostic requests scoped to the
workspace.

@since 3.17.0."))
  (:documentation "Workspace specific client capabilities.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-client-capabilities
    common-lisp:nil
  ((synchronization :type text-document-sync-client-capabilities :documentation
    "Defines which synchronization capabilities the client supports.")
   (completion :type completion-client-capabilities :documentation
    "Capabilities specific to the `textDocument/completion` request.")
   (hover :type hover-client-capabilities :documentation
    "Capabilities specific to the `textDocument/hover` request.")
   (signaturehelp :type signature-help-client-capabilities :documentation
    "Capabilities specific to the `textDocument/signatureHelp` request.")
   (declaration :type declaration-client-capabilities :since "3.14.0" :documentation
    "Capabilities specific to the `textDocument/declaration` request.

@since 3.14.0")
   (definition :type definition-client-capabilities :documentation
    "Capabilities specific to the `textDocument/definition` request.")
   (typedefinition :type type-definition-client-capabilities :since "3.6.0" :documentation
    "Capabilities specific to the `textDocument/typeDefinition` request.

@since 3.6.0")
   (implementation :type implementation-client-capabilities :since "3.6.0" :documentation
    "Capabilities specific to the `textDocument/implementation` request.

@since 3.6.0")
   (references :type reference-client-capabilities :documentation
    "Capabilities specific to the `textDocument/references` request.")
   (documenthighlight :type document-highlight-client-capabilities :documentation
    "Capabilities specific to the `textDocument/documentHighlight` request.")
   (documentsymbol :type document-symbol-client-capabilities :documentation
    "Capabilities specific to the `textDocument/documentSymbol` request.")
   (codeaction :type code-action-client-capabilities :documentation
    "Capabilities specific to the `textDocument/codeAction` request.")
   (codelens :type code-lens-client-capabilities :documentation
    "Capabilities specific to the `textDocument/codeLens` request.")
   (documentlink :type document-link-client-capabilities :documentation
    "Capabilities specific to the `textDocument/documentLink` request.")
   (colorprovider :type document-color-client-capabilities :since "3.6.0" :documentation
    "Capabilities specific to the `textDocument/documentColor` and the
`textDocument/colorPresentation` request.

@since 3.6.0")
   (formatting :type document-formatting-client-capabilities :documentation
    "Capabilities specific to the `textDocument/formatting` request.")
   (rangeformatting :type document-range-formatting-client-capabilities :documentation
    "Capabilities specific to the `textDocument/rangeFormatting` request.")
   (ontype-formatting :type document-on-type-formatting-client-capabilities :documentation
    "Capabilities specific to the `textDocument/onTypeFormatting` request.")
   (rename :type rename-client-capabilities :documentation
    "Capabilities specific to the `textDocument/rename` request.")
   (foldingrange :type folding-range-client-capabilities :since "3.10.0" :documentation
    "Capabilities specific to the `textDocument/foldingRange` request.

@since 3.10.0")
   (selectionrange :type selection-range-client-capabilities :since "3.15.0" :documentation
    "Capabilities specific to the `textDocument/selectionRange` request.

@since 3.15.0")
   (publishdiagnostics :type publish-diagnostics-client-capabilities :documentation
    "Capabilities specific to the `textDocument/publishDiagnostics` notification.")
   (callhierarchy :type call-hierarchy-client-capabilities :since "3.16.0" :documentation
    "Capabilities specific to the various call hierarchy requests.

@since 3.16.0")
   (semantictokens :type semantic-tokens-client-capabilities :since "3.16.0" :documentation
    "Capabilities specific to the various semantic token request.

@since 3.16.0")
   (linkedediting-range :type linked-editing-range-client-capabilities :since "3.16.0"
    :documentation "Capabilities specific to the `textDocument/linkedEditingRange` request.

@since 3.16.0")
   (moniker :type moniker-client-capabilities :since "3.16.0" :documentation
    "Client capabilities specific to the `textDocument/moniker` request.

@since 3.16.0")
   (typehierarchy :type type-hierarchy-client-capabilities :since "3.17.0" :documentation
    "Capabilities specific to the various type hierarchy requests.

@since 3.17.0")
   (inlinevalue :type inline-value-client-capabilities :since "3.17.0" :documentation
    "Capabilities specific to the `textDocument/inlineValue` request.

@since 3.17.0")
   (inlayhint :type inlay-hint-client-capabilities :since "3.17.0" :documentation
    "Capabilities specific to the `textDocument/inlayHint` request.

@since 3.17.0")
   (diagnostic :type diagnostic-client-capabilities :since "3.17.0" :documentation
    "Capabilities specific to the diagnostic pull model.

@since 3.17.0"))
  (:documentation "Text document specific client capabilities.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-client-capabilities
    common-lisp:nil
  ((synchronization :type notebook-document-sync-client-capabilities :initform
    (alexandria:required-argument :synchronization) :since "3.17.0" :documentation
    "Capabilities specific to notebook document synchronization

@since 3.17.0"))
  (:since "3.17.0")
  (:documentation "Capabilities specific to the notebook document support.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class window-client-capabilities
    common-lisp:nil
  ((workdone-progress :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "It indicates whether the client supports server initiated
progress using the `window/workDoneProgress/create` request.

The capability also controls Whether client supports handling
of progress notifications. If set servers are allowed to report a
`workDoneProgress` property in the request specific server
capabilities.

@since 3.15.0")
   (showmessage :type show-message-request-client-capabilities :since "3.16.0" :documentation
    "Capabilities specific to the showMessage request.

@since 3.16.0")
   (showdocument :type show-document-client-capabilities :since "3.16.0" :documentation
    "Capabilities specific to the showDocument request.

@since 3.16.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class general-client-capabilities
    common-lisp:nil
  ((stalerequest-support :type
    (lem-language-server/protocol-generator::lsp-interface
     ((cancel :type lem-language-server/protocol-generator::lsp-boolean :initform
       (alexandria:required-argument :cancel) :documentation
       "The client will actively cancel the request.")
      (retryon-content-modified :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :initform (alexandria:required-argument :retryon-content-modified) :documentation
       "The list of requests for which the client
will retry the request if it receives a
response with error code `ContentModified`")))
    :since "3.17.0" :documentation "Client capability that signals how the client
handles stale requests (e.g. a request
for which the client will not process the response
anymore since the information is outdated).

@since 3.17.0")
   (regularexpressions :type regular-expressions-client-capabilities :since "3.16.0" :documentation
    "Client capabilities specific to regular expressions.

@since 3.16.0")
   (markdown :type markdown-client-capabilities :since "3.16.0" :documentation
    "Client capabilities specific to the client's markdown parser.

@since 3.16.0")
   (positionencodings :type
    (lem-language-server/protocol-generator::lsp-array position-encoding-kind) :since "3.17.0"
    :documentation "The position encodings supported by the client. Client and server
have to agree on the same position encoding to ensure that offsets
(e.g. character position in a line) are interpreted the same on both
sides.

To keep the protocol backwards compatible the following applies: if
the value 'utf-16' is missing from the array of position encodings
servers can assume that the client supports UTF-16. UTF-16 is
therefore a mandatory encoding.

If omitted it defaults to ['utf-16'].

Implementation considerations: since the conversion from one encoding
into another requires the content of the file / line the conversion
is best done where the file is read which is usually on the server
side.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "General client capabilities.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class relative-pattern
    common-lisp:nil
  ((baseuri :type (common-lisp:or workspace-folder lem-language-server/protocol-generator::lsp-uri)
    :initform (alexandria:required-argument :baseuri) :documentation
    "A workspace folder or a base URI to which this pattern will be matched
against relatively.")
   (pattern :type pattern :initform (alexandria:required-argument :pattern) :documentation
    "The actual glob pattern;"))
  (:since "3.17.0")
  (:documentation "A relative pattern is a helper to construct glob patterns that are matched
relatively to a base URI. The common value for a `baseUri` is a workspace
folder root, but it can be another absolute URI as well.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-edit-client-capabilities
    common-lisp:nil
  ((documentchanges :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports versioned document changes in `WorkspaceEdit`s")
   (resourceoperations :type
    (lem-language-server/protocol-generator::lsp-array resource-operation-kind) :since "3.13.0"
    :documentation "The resource operations the client supports. Clients should at least
support 'create', 'rename' and 'delete' files and folders.

@since 3.13.0")
   (failurehandling :type failure-handling-kind :since "3.13.0" :documentation
    "The failure handling strategy of a client if applying the workspace edit
fails.

@since 3.13.0")
   (normalizesline-endings :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.16.0" :documentation "Whether the client normalizes line endings to the client specific
setting.
If set to `true` the client will normalize line ending characters
in a workspace edit to the client-specified new line
character.

@since 3.16.0")
   (changeannotation-support :type
    (lem-language-server/protocol-generator::lsp-interface
     ((groupson-label :type lem-language-server/protocol-generator::lsp-boolean :documentation
       "Whether the client groups edits with equal labels into tree nodes,
for instance all edits labelled with \"Changes in Strings\" would
be a tree node.")))
    :since "3.16.0" :documentation
    "Whether the client in general supports change annotations on text edits,
create file, rename file and delete file changes.

@since 3.16.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-configuration-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Did change configuration notification supports dynamic registration."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class did-change-watched-files-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Did change watched files notification supports dynamic registration. Please note
that the current protocol doesn't support static configuration for file changes
from the server side.")
   (relativepattern-support :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.17.0" :documentation
    "Whether the client has support for {@link  RelativePattern relative pattern}
or not.

@since 3.17.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class workspace-symbol-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Symbol request supports dynamic registration.")
   (symbolkind :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array symbol-kind)
       :documentation "The symbol kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the symbol kinds from `File` to `Array` as defined in
the initial version of the protocol.")))
    :documentation "Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.")
   (tagsupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :initform
       (alexandria:required-argument :valueset) :documentation
       "The tags supported by the client.")))
    :since "3.16.0" :documentation "The client supports tags on `SymbolInformation`.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.16.0")
   (resolvesupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((properties :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :initform (alexandria:required-argument :properties) :documentation
       "The properties that a client can resolve lazily. Usually
`location.range`")))
    :since "3.17.0" :documentation
    "The client support partial workspace symbols. The client will send the
request `workspaceSymbol/resolve` to the server to resolve additional
properties.

@since 3.17.0"))
  (:documentation "Client capabilities for a {@link WorkspaceSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class execute-command-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Execute command supports dynamic registration."))
  (:documentation "The client capabilities of a {@link ExecuteCommandRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-workspace-client-capabilities
    common-lisp:nil
  ((refreshsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
semantic tokens currently shown. It should be used with absolute care
and is useful for situation where a server for example detects a project
wide change that requires such a calculation."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens-workspace-client-capabilities
    common-lisp:nil
  ((refreshsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client implementation supports a refresh request sent from the
server to the client.

Note that this event is global and will force the client to refresh all
code lenses currently shown. It should be used with absolute care and is
useful for situation where a server for example detect a project wide
change that requires such a calculation."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class file-operation-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client supports dynamic registration for file requests/notifications.")
   (didcreate :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending didCreateFiles notifications.")
   (willcreate :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending willCreateFiles requests.")
   (didrename :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending didRenameFiles notifications.")
   (willrename :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending willRenameFiles requests.")
   (diddelete :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending didDeleteFiles notifications.")
   (willdelete :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client has support for sending willDeleteFiles requests."))
  (:since "3.16.0")
  (:documentation "Capabilities relating to events from file operations by the user in the client.

These events do not come from the file system, they come from user operations
like renaming a file in the UI.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-workspace-client-capabilities
    common-lisp:nil
  ((refreshsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client implementation supports a refresh request sent from the
server to the client.

Note that this event is global and will force the client to refresh all
inline values currently shown. It should be used with absolute care and is
useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Client workspace capabilities specific to inline values.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-workspace-client-capabilities
    common-lisp:nil
  ((refreshsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
inlay hints currently shown. It should be used with absolute care and
is useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Client workspace capabilities specific to inlay hints.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-workspace-client-capabilities
    common-lisp:nil
  ((refreshsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client implementation supports a refresh request sent from
the server to the client.

Note that this event is global and will force the client to refresh all
pulled diagnostics currently shown. It should be used with absolute care and
is useful for situation where a server for example detects a project wide
change that requires such a calculation."))
  (:since "3.17.0")
  (:documentation "Workspace client capabilities specific to diagnostic pull requests.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class text-document-sync-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether text document synchronization supports dynamic registration.")
   (willsave :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports sending will save notifications.")
   (willsave-wait-until :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports sending a will save request and
waits for a response providing text edits which will
be applied to the document before it is saved.")
   (didsave :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports did save notifications."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class completion-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether completion supports dynamic registration.")
   (completionitem :type
    (lem-language-server/protocol-generator::lsp-interface
     ((snippetsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
       "Client supports snippets as insert text.

A snippet can define tab stops and placeholders with `$1`, `$2`
and `${3:foo}`. `$0` defines the final tab stop, it defaults to
the end of the snippet. Placeholders with equal identifiers are linked,
that is typing in one will update others too.")
      (commitcharacters-support :type lem-language-server/protocol-generator::lsp-boolean
       :documentation "Client supports commit characters on a completion item.")
      (documentationformat :type (lem-language-server/protocol-generator::lsp-array markup-kind)
       :documentation "Client supports the following content formats for the documentation
property. The order describes the preferred format of the client.")
      (deprecatedsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
       "Client supports the deprecated property on a completion item.")
      (preselectsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
       "Client supports the preselect property on a completion item.")
      (tagsupport :type
       (lem-language-server/protocol-generator::lsp-interface
        ((valueset :type (lem-language-server/protocol-generator::lsp-array completion-item-tag)
          :initform (alexandria:required-argument :valueset) :documentation
          "The tags supported by the client.")))
       :since "3.15.0" :documentation
       "Client supports the tag property on a completion item. Clients supporting
tags have to handle unknown tags gracefully. Clients especially need to
preserve unknown tags when sending a completion item back to the server in
a resolve call.

@since 3.15.0")
      (insertreplace-support :type lem-language-server/protocol-generator::lsp-boolean :since
       "3.16.0" :documentation
       "Client support insert replace edit to control different behavior if a
completion item is inserted in the text or should replace text.

@since 3.16.0")
      (resolvesupport :type
       (lem-language-server/protocol-generator::lsp-interface
        ((properties :type
          (lem-language-server/protocol-generator::lsp-array
           lem-language-server/protocol-generator::lsp-string)
          :initform (alexandria:required-argument :properties) :documentation
          "The properties that a client can resolve lazily.")))
       :since "3.16.0" :documentation
       "Indicates which properties a client can resolve lazily on a completion
item. Before version 3.16.0 only the predefined properties `documentation`
and `details` could be resolved lazily.

@since 3.16.0")
      (inserttext-mode-support :type
       (lem-language-server/protocol-generator::lsp-interface
        ((valueset :type (lem-language-server/protocol-generator::lsp-array insert-text-mode)
          :initform (alexandria:required-argument :valueset))))
       :since "3.16.0" :documentation "The client supports the `insertTextMode` property on
a completion item to override the whitespace handling mode
as defined by the client (see `insertTextMode`).

@since 3.16.0")
      (labeldetails-support :type lem-language-server/protocol-generator::lsp-boolean :since
       "3.17.0" :documentation "The client has support for completion item label
details (see also `CompletionItemLabelDetails`).

@since 3.17.0")))
    :documentation "The client supports the following `CompletionItem` specific
capabilities.")
   (completionitem-kind :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array completion-item-kind)
       :documentation "The completion item kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the completion items kinds from `Text` to `Reference` as defined in
the initial version of the protocol."))))
   (inserttext-mode :type insert-text-mode :since "3.17.0" :documentation
    "Defines how the client handles whitespace and indentation
when accepting a completion item that uses multi line
text in either `insertText` or `textEdit`.

@since 3.17.0")
   (contextsupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports to send additional context information for a
`textDocument/completion` request.")
   (completionlist :type
    (lem-language-server/protocol-generator::lsp-interface
     ((itemdefaults :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :since "3.17.0" :documentation "The client supports the following itemDefaults on
a completion list.

The value lists the supported property names of the
`CompletionList.itemDefaults` object. If omitted
no properties are supported.

@since 3.17.0")))
    :since "3.17.0" :documentation "The client supports the following `CompletionList` specific
capabilities.

@since 3.17.0"))
  (:documentation "Completion client capabilities")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class hover-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether hover supports dynamic registration.")
   (contentformat :type (lem-language-server/protocol-generator::lsp-array markup-kind)
    :documentation "Client supports the following content formats for the content
property. The order describes the preferred format of the client."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class signature-help-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether signature help supports dynamic registration.")
   (signatureinformation :type
    (lem-language-server/protocol-generator::lsp-interface
     ((documentationformat :type (lem-language-server/protocol-generator::lsp-array markup-kind)
       :documentation "Client supports the following content formats for the documentation
property. The order describes the preferred format of the client.")
      (parameterinformation :type
       (lem-language-server/protocol-generator::lsp-interface
        ((labeloffset-support :type lem-language-server/protocol-generator::lsp-boolean :since
          "3.14.0" :documentation "The client supports processing label offsets instead of a
simple label string.

@since 3.14.0")))
       :documentation "Client capabilities specific to parameter information.")
      (activeparameter-support :type lem-language-server/protocol-generator::lsp-boolean :since
       "3.16.0" :documentation
       "The client supports the `activeParameter` property on `SignatureInformation`
literal.

@since 3.16.0")))
    :documentation "The client supports the following `SignatureInformation`
specific properties.")
   (contextsupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "The client supports to send additional context information for a
`textDocument/signatureHelp` request. A client that opts into
contextSupport will also support the `retriggerCharacters` on
`SignatureHelpOptions`.

@since 3.15.0"))
  (:documentation "Client Capabilities for a {@link SignatureHelpRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class declaration-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether declaration supports dynamic registration. If this is set to `true`
the client supports the new `DeclarationRegistrationOptions` return value
for the corresponding server capability as well.")
   (linksupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports additional metadata in the form of declaration links."))
  (:since "3.14.0")
  (:documentation "@since 3.14.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class definition-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether definition supports dynamic registration.")
   (linksupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.14.0"
    :documentation "The client supports additional metadata in the form of definition links.

@since 3.14.0"))
  (:documentation "Client Capabilities for a {@link DefinitionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-definition-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `TypeDefinitionRegistrationOptions` return value
for the corresponding server capability as well.")
   (linksupport :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "The client supports additional metadata in the form of definition links.

Since 3.14.0"))
  (:documentation "Since 3.6.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class implementation-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `ImplementationRegistrationOptions` return value
for the corresponding server capability as well.")
   (linksupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.14.0"
    :documentation "The client supports additional metadata in the form of definition links.

@since 3.14.0"))
  (:since "3.6.0")
  (:documentation "@since 3.6.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class reference-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether references supports dynamic registration."))
  (:documentation "Client Capabilities for a {@link ReferencesRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-highlight-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether document highlight supports dynamic registration."))
  (:documentation "Client Capabilities for a {@link DocumentHighlightRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-symbol-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether document symbol supports dynamic registration.")
   (symbolkind :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array symbol-kind)
       :documentation "The symbol kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.

If this property is not present the client only supports
the symbol kinds from `File` to `Array` as defined in
the initial version of the protocol.")))
    :documentation "Specific capabilities for the `SymbolKind` in the
`textDocument/documentSymbol` request.")
   (hierarchicaldocument-symbol-support :type lem-language-server/protocol-generator::lsp-boolean
    :documentation "The client supports hierarchical document symbols.")
   (tagsupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array symbol-tag) :initform
       (alexandria:required-argument :valueset) :documentation
       "The tags supported by the client.")))
    :since "3.16.0" :documentation
    "The client supports tags on `SymbolInformation`. Tags are supported on
`DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.16.0")
   (labelsupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.16.0"
    :documentation "The client supports an additional label presented in the UI when
registering a document symbol provider.

@since 3.16.0"))
  (:documentation "Client Capabilities for a {@link DocumentSymbolRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-action-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether code action supports dynamic registration.")
   (codeaction-literal-support :type
    (lem-language-server/protocol-generator::lsp-interface
     ((codeaction-kind :type
       (lem-language-server/protocol-generator::lsp-interface
        ((valueset :type (lem-language-server/protocol-generator::lsp-array code-action-kind)
          :initform (alexandria:required-argument :valueset) :documentation
          "The code action kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.")))
       :initform (alexandria:required-argument :codeaction-kind) :documentation
       "The code action kind is support with the following value
set.")))
    :since "3.8.0" :documentation
    "The client support code action literals of type `CodeAction` as a valid
response of the `textDocument/codeAction` request. If the property is not
set the request can only return `Command` literals.

@since 3.8.0")
   (ispreferred-support :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "Whether code action supports the `isPreferred` property.

@since 3.15.0")
   (disabledsupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.16.0"
    :documentation "Whether code action supports the `disabled` property.

@since 3.16.0")
   (datasupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.16.0"
    :documentation "Whether code action supports the `data` property which is
preserved between a `textDocument/codeAction` and a
`codeAction/resolve` request.

@since 3.16.0")
   (resolvesupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((properties :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :initform (alexandria:required-argument :properties) :documentation
       "The properties that a client can resolve lazily.")))
    :since "3.16.0" :documentation "Whether the client supports resolving additional code action
properties via a separate `codeAction/resolve` request.

@since 3.16.0")
   (honorschange-annotations :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.16.0" :documentation "Whether the client honors the change annotations in
text edits and resource operations returned via the
`CodeAction#edit` property by for example presenting
the workspace edit in the user interface and asking
for confirmation.

@since 3.16.0"))
  (:documentation "The Client Capabilities of a {@link CodeActionRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class code-lens-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether code lens supports dynamic registration."))
  (:documentation "The client capabilities  of a {@link CodeLensRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-link-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether document link supports dynamic registration.")
   (tooltipsupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "Whether the client supports the `tooltip` property on `DocumentLink`.

@since 3.15.0"))
  (:documentation "The client capabilities of a {@link DocumentLinkRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-color-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `DocumentColorRegistrationOptions` return value
for the corresponding server capability as well."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-formatting-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether formatting supports dynamic registration."))
  (:documentation "Client capabilities of a {@link DocumentFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-range-formatting-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether range formatting supports dynamic registration."))
  (:documentation "Client capabilities of a {@link DocumentRangeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class document-on-type-formatting-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether on type formatting supports dynamic registration."))
  (:documentation "Client capabilities of a {@link DocumentOnTypeFormattingRequest}.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class rename-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether rename supports dynamic registration.")
   (preparesupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.12.0"
    :documentation "Client supports testing for validity of rename operations
before execution.

@since 3.12.0")
   (preparesupport-default-behavior :type prepare-support-default-behavior :since "3.16.0"
    :documentation "Client supports the default behavior result.

The value indicates the default behavior used by the
client.

@since 3.16.0")
   (honorschange-annotations :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.16.0" :documentation "Whether the client honors the change annotations in
text edits and resource operations returned via the
rename request's workspace edit by for example presenting
the workspace edit in the user interface and asking
for confirmation.

@since 3.16.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class folding-range-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration for folding range
providers. If this is set to `true` the client supports the new
`FoldingRangeRegistrationOptions` return value for the corresponding
server capability as well.")
   (rangelimit :type lem-language-server/protocol-generator::lsp-uinteger :documentation
    "The maximum number of folding ranges that the client prefers to receive
per document. The value serves as a hint, servers are free to follow the
limit.")
   (linefolding-only :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "If set, the client signals that it only supports folding complete lines.
If set, client will ignore specified `startCharacter` and `endCharacter`
properties in a FoldingRange.")
   (foldingrange-kind :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array folding-range-kind)
       :documentation "The folding range kind values the client supports. When this
property exists the client also guarantees that it will
handle values outside its set gracefully and falls back
to a default value when unknown.")))
    :since "3.17.0" :documentation "Specific options for the folding range kind.

@since 3.17.0")
   (foldingrange :type
    (lem-language-server/protocol-generator::lsp-interface
     ((collapsedtext :type lem-language-server/protocol-generator::lsp-boolean :since "3.17.0"
       :documentation "If set, the client signals that it supports setting collapsedText on
folding ranges to display custom labels instead of the default text.

@since 3.17.0")))
    :since "3.17.0" :documentation "Specific options for the folding range.

@since 3.17.0"))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class selection-range-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration for selection range providers. If this is set to `true`
the client supports the new `SelectionRangeRegistrationOptions` return value for the corresponding server
capability as well."))
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class publish-diagnostics-client-capabilities
    common-lisp:nil
  ((relatedinformation :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the clients accepts diagnostics with related information.")
   (tagsupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((valueset :type (lem-language-server/protocol-generator::lsp-array diagnostic-tag) :initform
       (alexandria:required-argument :valueset) :documentation
       "The tags supported by the client.")))
    :since "3.15.0" :documentation
    "Client supports the tag property to provide meta data about a diagnostic.
Clients supporting tags have to handle unknown tags gracefully.

@since 3.15.0")
   (versionsupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.15.0"
    :documentation "Whether the client interprets the version property of the
`textDocument/publishDiagnostics` notification's parameter.

@since 3.15.0")
   (codedescription-support :type lem-language-server/protocol-generator::lsp-boolean :since
    "3.16.0" :documentation "Client supports a codeDescription property

@since 3.16.0")
   (datasupport :type lem-language-server/protocol-generator::lsp-boolean :since "3.16.0"
    :documentation "Whether code action supports the `data` property which is
preserved between a `textDocument/publishDiagnostics` and
`textDocument/codeAction` request.

@since 3.16.0"))
  (:documentation "The publish diagnostic client capabilities.")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class call-hierarchy-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class semantic-tokens-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (requests :type
    (lem-language-server/protocol-generator::lsp-interface
     ((range :type
       (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                       (lem-language-server/protocol-generator::lsp-interface common-lisp:nil))
       :documentation "The client will send the `textDocument/semanticTokens/range` request if
the server provides a corresponding handler.")
      (full :type
       (common-lisp:or lem-language-server/protocol-generator::lsp-boolean
                       (lem-language-server/protocol-generator::lsp-interface
                        ((delta :type lem-language-server/protocol-generator::lsp-boolean
                          :documentation
                          "The client will send the `textDocument/semanticTokens/full/delta` request if
the server provides a corresponding handler."))))
       :documentation "The client will send the `textDocument/semanticTokens/full` request if
the server provides a corresponding handler.")))
    :initform (alexandria:required-argument :requests) :documentation
    "Which requests the client supports and might send to the server
depending on the server's capability. Please note that clients might not
show semantic tokens or degrade some of the user experience if a range
or full request is advertised by the client but not provided by the
server. If for example the client capability `requests.full` and
`request.range` are both set to true but the server only provides a
range provider the client might not render a minimap correctly or might
even decide to not show any semantic tokens at all.")
   (tokentypes :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :tokentypes) :documentation
    "The token types that the client supports.")
   (tokenmodifiers :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :initform (alexandria:required-argument :tokenmodifiers) :documentation
    "The token modifiers that the client supports.")
   (formats :type (lem-language-server/protocol-generator::lsp-array token-format) :initform
    (alexandria:required-argument :formats) :documentation
    "The token formats the clients supports.")
   (overlappingtoken-support :type lem-language-server/protocol-generator::lsp-boolean
    :documentation "Whether the client supports tokens that can overlap each other.")
   (multilinetoken-support :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether the client supports tokens that can span multiple lines.")
   (servercancel-support :type lem-language-server/protocol-generator::lsp-boolean :since "3.17.0"
    :documentation "Whether the client allows the server to actively cancel a
semantic token request, e.g. supports returning
LSPErrorCodes.ServerCancelled. If a server does the client
needs to retrigger the request.

@since 3.17.0")
   (augmentssyntax-tokens :type lem-language-server/protocol-generator::lsp-boolean :since "3.17.0"
    :documentation "Whether the client uses semantic tokens to augment existing
syntax tokens. If set to `true` client side created syntax
tokens and semantic tokens are both used for colorization. If
set to `false` the client only uses the returned semantic tokens
for colorization.

If the value is `undefined` then the client behavior is not
specified.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class linked-editing-range-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "Client capabilities for the linked editing range request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class moniker-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether moniker supports dynamic registration. If this is set to `true`
the client supports the new `MonikerRegistrationOptions` return value
for the corresponding server capability as well."))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to the moniker request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class type-hierarchy-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well."))
  (:since "3.17.0")
  (:documentation "@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inline-value-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration for inline value providers."))
  (:since "3.17.0")
  (:documentation "Client capabilities specific to inline values.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class inlay-hint-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether inlay hints support dynamic registration.")
   (resolvesupport :type
    (lem-language-server/protocol-generator::lsp-interface
     ((properties :type
       (lem-language-server/protocol-generator::lsp-array
        lem-language-server/protocol-generator::lsp-string)
       :initform (alexandria:required-argument :properties) :documentation
       "The properties that a client can resolve lazily.")))
    :documentation "Indicates which properties a client can resolve lazily on an inlay
hint."))
  (:since "3.17.0")
  (:documentation "Inlay hint client capabilities.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class diagnostic-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is set to `true`
the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (relateddocument-support :type lem-language-server/protocol-generator::lsp-boolean
    :documentation "Whether the clients supports related documents for document diagnostic pulls."))
  (:since "3.17.0")
  (:documentation "Client capabilities specific to diagnostic pull requests.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class notebook-document-sync-client-capabilities
    common-lisp:nil
  ((dynamicregistration :type lem-language-server/protocol-generator::lsp-boolean :documentation
    "Whether implementation supports dynamic registration. If this is
set to `true` the client supports the new
`(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
return value for the corresponding server capability as well.")
   (executionsummary-support :type lem-language-server/protocol-generator::lsp-boolean
    :documentation "The client supports sending execution summary data per cell."))
  (:since "3.17.0")
  (:documentation "Notebook specific client capabilities.

@since 3.17.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-message-request-client-capabilities
    common-lisp:nil
  ((messageaction-item :type
    (lem-language-server/protocol-generator::lsp-interface
     ((additionalproperties-support :type lem-language-server/protocol-generator::lsp-boolean
       :documentation "Whether the client supports additional attributes which
are preserved and send back to the server in the
request's response.")))
    :documentation "Capabilities specific to the `MessageActionItem` type."))
  (:documentation "Show message request client capabilities")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class show-document-client-capabilities
    common-lisp:nil
  ((support :type lem-language-server/protocol-generator::lsp-boolean :initform
    (alexandria:required-argument :support) :documentation
    "The client has support for the showDocument
request."))
  (:since "3.16.0")
  (:documentation "Client capabilities for the showDocument request.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class regular-expressions-client-capabilities
    common-lisp:nil
  ((engine :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :engine) :documentation "The engine's name.")
   (version :type lem-language-server/protocol-generator::lsp-string :documentation
    "The engine's version."))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to regular expressions.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-class markdown-client-capabilities
    common-lisp:nil
  ((parser :type lem-language-server/protocol-generator::lsp-string :initform
    (alexandria:required-argument :parser) :documentation "The name of the parser.")
   (version :type lem-language-server/protocol-generator::lsp-string :documentation
    "The version of the parser.")
   (allowedtags :type
    (lem-language-server/protocol-generator::lsp-array
     lem-language-server/protocol-generator::lsp-string)
    :since "3.17.0" :documentation "A list of HTML tags that the client allows / supports in
Markdown.

@since 3.17.0"))
  (:since "3.16.0")
  (:documentation "Client capabilities specific to the used markdown parser.

@since 3.16.0")
  (:metaclass lem-language-server/protocol-generator::protocol-class))

(lem-language-server/protocol-generator::define-type-alias definition
    (common-lisp:or location (lem-language-server/protocol-generator::lsp-array location))
  (:documentation "The definition of a symbol represented as one or many {@link Location locations}.
For most programming languages there is only one location at which a symbol is
defined.

Servers should prefer returning `DefinitionLink` over `Definition` if supported
by the client."))

(lem-language-server/protocol-generator::define-type-alias definition-link
    location-link
  (:documentation "Information about where a symbol is defined.

Provides additional metadata over normal {@link Location location} definitions, including the range of
the defining symbol"))

(lem-language-server/protocol-generator::define-type-alias l-s-p-array
    (lem-language-server/protocol-generator::lsp-array l-s-p-any)
  (:documentation "LSP arrays.
@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias l-s-p-any
    (common-lisp:or l-s-p-object l-s-p-array lem-language-server/protocol-generator::lsp-string
                    lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-uinteger
                    lem-language-server/protocol-generator::lsp-decimal
                    lem-language-server/protocol-generator::lsp-boolean
                    lem-language-server/protocol-generator::lsp-null)
  (:documentation "The LSP any type.
Please note that strictly speaking a property with the value `undefined`
can't be converted into JSON preserving the property name. However for
convenience it is allowed and assumed that all these properties are
optional as well.
@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias declaration
    (common-lisp:or location (lem-language-server/protocol-generator::lsp-array location))
  (:documentation
   "The declaration of a symbol representation as one or many {@link Location locations}."))

(lem-language-server/protocol-generator::define-type-alias declaration-link
    location-link
  (:documentation "Information about where a symbol is declared.

Provides additional metadata over normal {@link Location location} declarations, including the range of
the declaring symbol.

Servers should prefer returning `DeclarationLink` over `Declaration` if supported
by the client."))

(lem-language-server/protocol-generator::define-type-alias inline-value
    (common-lisp:or inline-value-text inline-value-variable-lookup
                    inline-value-evaluatable-expression)
  (:documentation "Inline value information can be provided by different means:
- directly as a text value (class InlineValueText).
- as a name to use for a variable lookup (class InlineValueVariableLookup)
- as an evaluatable expression (class InlineValueEvaluatableExpression)
The InlineValue types combines all inline value types into one type.

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias document-diagnostic-report
    (common-lisp:or related-full-document-diagnostic-report
                    related-unchanged-document-diagnostic-report)
  (:documentation "The result of a document diagnostic pull request. A report can
either be a full report containing all diagnostics for the
requested document or an unchanged report indicating that nothing
has changed in terms of diagnostics in comparison to the last
pull request.

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias prepare-rename-result
    (common-lisp:or range
                    (lem-language-server/protocol-generator::lsp-interface
                     ((range :type range :initform (alexandria:required-argument :range))
                      (placeholder :type lem-language-server/protocol-generator::lsp-string
                       :initform (alexandria:required-argument :placeholder))))
                    (lem-language-server/protocol-generator::lsp-interface
                     ((defaultbehavior :type lem-language-server/protocol-generator::lsp-boolean
                       :initform (alexandria:required-argument :defaultbehavior))))))

(lem-language-server/protocol-generator::define-type-alias document-selector
    (lem-language-server/protocol-generator::lsp-array document-filter)
  (:documentation "A document selector is the combination of one or many document filters.

@sample `let sel:DocumentSelector = [{ language: 'typescript' }, { language: 'json', pattern: '**∕tsconfig.json' }]`;

The use of a string as a document filter is deprecated @since 3.16.0.")
  (:since "3.16.0."))

(lem-language-server/protocol-generator::define-type-alias progress-token
    (common-lisp:or lem-language-server/protocol-generator::lsp-integer
                    lem-language-server/protocol-generator::lsp-string))

(lem-language-server/protocol-generator::define-type-alias change-annotation-identifier
    lem-language-server/protocol-generator::lsp-string
  (:documentation "An identifier to refer to a change annotation stored with a workspace edit."))

(lem-language-server/protocol-generator::define-type-alias workspace-document-diagnostic-report
    (common-lisp:or workspace-full-document-diagnostic-report
                    workspace-unchanged-document-diagnostic-report)
  (:documentation "A workspace diagnostic document report.

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias text-document-content-change-event
    (common-lisp:or
     (lem-language-server/protocol-generator::lsp-interface
      ((range :type range :initform (alexandria:required-argument :range) :documentation
        "The range of the document that changed.")
       (rangelength :type lem-language-server/protocol-generator::lsp-uinteger :documentation
        "The optional length of the range that got replaced.

@deprecated use range instead.")
       (text :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :text) :documentation
        "The new text for the provided range.")))
     (lem-language-server/protocol-generator::lsp-interface
      ((text :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :text) :documentation
        "The new text of the whole document."))))
  (:documentation "An event describing a change to a text document. If only a text is provided
it is considered to be the full content of the document."))

(lem-language-server/protocol-generator::define-type-alias marked-string
    (common-lisp:or lem-language-server/protocol-generator::lsp-string
                    (lem-language-server/protocol-generator::lsp-interface
                     ((language :type lem-language-server/protocol-generator::lsp-string :initform
                       (alexandria:required-argument :language))
                      (value :type lem-language-server/protocol-generator::lsp-string :initform
                       (alexandria:required-argument :value)))))
  (:deprecated "use MarkupContent instead.")
  (:documentation
   "MarkedString can be used to render human readable text. It is either a markdown string
or a code-block that provides a language and a code snippet. The language identifier
is semantically equal to the optional language identifier in fenced code blocks in GitHub
issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting

The pair of a language and a value is an equivalent to markdown:
```${language}
${value}
```

Note that markdown strings will be sanitized - that means html will be escaped.
@deprecated use MarkupContent instead."))

(lem-language-server/protocol-generator::define-type-alias document-filter
    (common-lisp:or text-document-filter notebook-cell-text-document-filter)
  (:documentation "A document filter describes a top level text document or
a notebook cell document.

@since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.")
  (:since "3.17.0 - proposed support for NotebookCellTextDocumentFilter."))

(lem-language-server/protocol-generator::define-type-alias l-s-p-object
    (lem-language-server/protocol-generator::lsp-map string l-s-p-any)
  (:documentation "LSP object definition.
@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias glob-pattern
    (common-lisp:or pattern relative-pattern)
  (:documentation "The glob pattern. Either a string pattern or a relative pattern.

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias text-document-filter
    (common-lisp:or
     (lem-language-server/protocol-generator::lsp-interface
      ((language :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :language) :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :documentation
        "A glob pattern, like `*.{ts,js}`.")))
     (lem-language-server/protocol-generator::lsp-interface
      ((language :type lem-language-server/protocol-generator::lsp-string :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :scheme) :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :documentation
        "A glob pattern, like `*.{ts,js}`.")))
     (lem-language-server/protocol-generator::lsp-interface
      ((language :type lem-language-server/protocol-generator::lsp-string :documentation
        "A language id, like `typescript`.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :pattern) :documentation
        "A glob pattern, like `*.{ts,js}`."))))
  (:documentation "A document filter denotes a document by different properties like
the {@link TextDocument.languageId language}, the {@link Uri.scheme scheme} of
its resource, or a glob-pattern that is applied to the {@link TextDocument.fileName path}.

Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

@sample A language filter that applies to typescript files on disk: `{ language: 'typescript', scheme: 'file' }`
@sample A language filter that applies to all package.json paths: `{ language: 'json', pattern: '**package.json' }`

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias notebook-document-filter
    (common-lisp:or
     (lem-language-server/protocol-generator::lsp-interface
      ((notebooktype :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :notebooktype) :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :documentation
        "A glob pattern.")))
     (lem-language-server/protocol-generator::lsp-interface
      ((notebooktype :type lem-language-server/protocol-generator::lsp-string :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :scheme) :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :documentation
        "A glob pattern.")))
     (lem-language-server/protocol-generator::lsp-interface
      ((notebooktype :type lem-language-server/protocol-generator::lsp-string :documentation
        "The type of the enclosing notebook.")
       (scheme :type lem-language-server/protocol-generator::lsp-string :documentation
        "A Uri {@link Uri.scheme scheme}, like `file` or `untitled`.")
       (pattern :type lem-language-server/protocol-generator::lsp-string :initform
        (alexandria:required-argument :pattern) :documentation "A glob pattern."))))
  (:documentation "A notebook document filter denotes a notebook document by
different properties. The properties will be match
against the notebook's URI (same as with documents)

@since 3.17.0")
  (:since "3.17.0"))

(lem-language-server/protocol-generator::define-type-alias pattern
    lem-language-server/protocol-generator::lsp-string
  (:documentation
   "The glob pattern to watch relative to the base path. Glob patterns can have the following syntax:
- `*` to match one or more characters in a path segment
- `?` to match on one character in a path segment
- `**` to match any number of path segments, including none
- `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
- `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
- `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)

@since 3.17.0")
  (:since "3.17.0"))