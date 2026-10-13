;; Nix highlights query for Lem editor
;; Based on tree-sitter-nix from nix-community/tree-sitter-nix

;; Comments
(comment) @comment

;; Keywords
[
  "if"
  "then"
  "else"
  "let"
  "in"
  "with"
  "rec"
  "inherit"
  "assert"
  "or"
] @keyword

;; Literals - Numbers
(integer_expression) @number
(float_expression) @number

;; Literals - Strings
(string_expression) @string
(indented_string_expression) @string

;; Escape sequences
(escape_sequence) @string.escape
(dollar_escape) @string.escape

;; Path expressions
(path_expression) @string
(hpath_expression) @string
(spath_expression) @string

;; URI expressions
(uri_expression) @string

;; Boolean and null constants
((identifier) @constant.builtin
 (#any-of? @constant.builtin "true" "false" "null"))

;; Builtins namespace
((identifier) @variable.builtin
 (#eq? @variable.builtin "builtins"))

;; String interpolation
(interpolation
  "${" @punctuation.special
  "}" @punctuation.special)

;; Operators
[
  "+"
  "-"
  "*"
  "/"
  "++"
  "//"
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  "&&"
  "||"
  "!"
  "->"
  "?"
  "@"
] @operator

;; Function definitions
(function_expression
  universal: (identifier) @variable.parameter)

;; Formal parameters in function arguments
(formal
  name: (identifier) @variable.parameter)

;; Formals with default values
(formal
  name: (identifier) @variable.parameter
  "?" @operator)

;; Inherit expressions
(inherit_from
  "inherit" @keyword
  "(" @punctuation.bracket
  ")" @punctuation.bracket)

(inherited_attrs
  (identifier) @property)

;; Attribute set bindings
(binding
  attrpath: (attrpath
              attr: (identifier) @property))

;; Attribute path navigation
(attrpath
  attr: (identifier) @property)

;; Select expressions (attribute access)
(select_expression
  attrpath: (attrpath
              attr: (identifier) @property))

;; Has attribute operator
(has_attr_expression
  "?" @operator)

;; Apply (function call)
(apply_expression
  function: (variable_expression
              name: (identifier) @function))

;; Variable references
(variable_expression
  name: (identifier) @variable)

;; Punctuation - Brackets
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

;; Punctuation - Delimiters
[
  "."
  ";"
  ":"
  ","
  "="
] @punctuation.delimiter

;; Error nodes (for diagnostics)
(ERROR) @warning
