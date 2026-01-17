; TypeScript highlights query for Lem editor
; Based on nvim-treesitter and helix conventions
; Works with both typescript and tsx grammars

;; Comments
(comment) @comment

;; Keywords
[
  "break"
  "case"
  "catch"
  "class"
  "const"
  "continue"
  "debugger"
  "default"
  "delete"
  "do"
  "else"
  "export"
  "extends"
  "finally"
  "for"
  "function"
  "if"
  "import"
  "in"
  "instanceof"
  "let"
  "new"
  "of"
  "return"
  "static"
  "switch"
  "throw"
  "try"
  "typeof"
  "var"
  "void"
  "while"
  "with"
  "yield"
  "await"
  "async"
  "from"
  "as"
  "get"
  "set"
] @keyword

;; TypeScript-specific keywords
[
  "abstract"
  "declare"
  "enum"
  "implements"
  "interface"
  "keyof"
  "namespace"
  "override"
  "private"
  "protected"
  "public"
  "readonly"
  "type"
  "satisfies"
  "module"
  "infer"
  "is"
  "using"
  "asserts"
] @keyword

;; Control flow keywords with specific highlight
[
  "return"
  "throw"
] @keyword.return

;; Literals - Strings
(string) @string
(template_string) @string
(template_literal_type) @string
(escape_sequence) @string.escape

;; Template string interpolation
(template_substitution
  "${" @punctuation.special
  "}" @punctuation.special)

;; Literals - Numbers
(number) @number

;; Literals - Boolean and special values
[
  (true)
  (false)
] @boolean

(null) @constant.builtin
(undefined) @constant.builtin

;; Regular expressions
(regex) @string
(regex_pattern) @string
(regex_flags) @string

;; this, super
(this) @variable.builtin
(super) @variable.builtin

;; Variables
(identifier) @variable

;; Variable declarations
(variable_declarator
  name: (identifier) @variable)

;; Object destructuring
(shorthand_property_identifier_pattern) @variable
(object_pattern
  (shorthand_property_identifier_pattern) @variable)

;; Array destructuring
(array_pattern
  (identifier) @variable)

;; Function definitions
(function_declaration
  name: (identifier) @function)

(function_expression
  name: (identifier) @function)

(generator_function_declaration
  name: (identifier) @function)

;; Arrow functions
(arrow_function
  parameter: (identifier) @variable.parameter)

;; Method definitions
(method_definition
  name: (property_identifier) @function.method)

;; Function calls
(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (member_expression
    property: (property_identifier) @function.method))

;; New expressions
(new_expression
  constructor: (identifier) @type)

;; Class definitions
(class_declaration
  name: (type_identifier) @type)

(class_expression
  name: (type_identifier) @type)

;; Type annotations
(type_identifier) @type
(predefined_type) @type.builtin

;; Generic type parameters
(type_parameter
  name: (type_identifier) @type)

;; Interface and type alias definitions
(interface_declaration
  name: (type_identifier) @type)

(type_alias_declaration
  name: (type_identifier) @type)

;; Enum definitions
(enum_declaration
  name: (identifier) @type)

(enum_body
  name: (property_identifier) @constant)

;; Namespace declarations
(ambient_declaration
  "declare" @keyword)

;; Property access
(member_expression
  property: (property_identifier) @property)

;; Object properties
(property_signature
  name: (property_identifier) @property)

(public_field_definition
  name: (property_identifier) @property)

(pair
  key: (property_identifier) @property)

(shorthand_property_identifier) @property

;; Function parameters
(required_parameter
  pattern: (identifier) @variable.parameter)

(optional_parameter
  pattern: (identifier) @variable.parameter)

(rest_pattern
  (identifier) @variable.parameter)

;; Formal parameters in function signature
(formal_parameters
  (identifier) @variable.parameter)

;; Decorators
(decorator
  "@" @punctuation.special
  (identifier) @attribute)

(decorator
  "@" @punctuation.special
  (call_expression
    function: (identifier) @attribute))

;; Operators
[
  "+"
  "-"
  "*"
  "/"
  "%"
  "**"
  "++"
  "--"
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "**="
  "=="
  "==="
  "!="
  "!=="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "&"
  "|"
  "^"
  "~"
  "<<"
  ">>"
  ">>>"
  "&="
  "|="
  "^="
  "<<="
  ">>="
  ">>>="
  "&&="
  "||="
  "??"
  "??="
  "?."
  "=>"
  "..."
] @operator

;; Ternary operator
(ternary_expression
  "?" @operator
  ":" @operator)

;; Type annotation colon
(type_annotation
  ":" @punctuation.delimiter)

;; Optional chaining and optional properties
(optional_chain) @operator

;; Punctuation - Brackets
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

;; Type parameter brackets
[
  "<"
  ">"
] @punctuation.bracket

;; Punctuation - Delimiters
[
  ";"
  "."
  ","
] @punctuation.delimiter

;; Import/Export statements
(import_statement
  "import" @keyword.import)

(export_statement
  "export" @keyword.import)

(import_specifier
  name: (identifier) @variable)

(export_specifier
  name: (identifier) @variable)

;; Named imports/exports
(named_imports
  (import_specifier
    name: (identifier) @variable))

;; JSX (for TSX files - will be skipped if not present in grammar)
(jsx_element
  open_tag: (jsx_opening_element
    name: (identifier) @tag))

(jsx_element
  close_tag: (jsx_closing_element
    name: (identifier) @tag))

(jsx_self_closing_element
  name: (identifier) @tag)

(jsx_attribute
  (property_identifier) @attribute)

;; Error nodes
(ERROR) @error
