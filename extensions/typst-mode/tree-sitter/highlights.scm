; Comments and shebang
(shebang) @keyword.directive
(line_comment) @comment
(block_comment) @comment
((line_comment) @comment.todo
  (#match? @comment.todo "TODO"))
((line_comment) @comment.todo
  (#match? @comment.todo "WIP"))
((block_comment) @comment.todo
  (#match? @comment.todo "TODO"))
((block_comment) @comment.todo
  (#match? @comment.todo "WIP"))
((line_comment) @comment.note
  (#match? @comment.note "NOTE"))
((line_comment) @comment.note
  (#match? @comment.note "INFO"))
((line_comment) @comment.note
  (#match? @comment.note "XXX"))
((block_comment) @comment.note
  (#match? @comment.note "NOTE"))
((block_comment) @comment.note
  (#match? @comment.note "INFO"))
((block_comment) @comment.note
  (#match? @comment.note "XXX"))
((line_comment) @comment.warning
  (#match? @comment.warning "WARNING"))
((line_comment) @comment.warning
  (#match? @comment.warning "FIX"))
((line_comment) @comment.warning
  (#match? @comment.warning "HACK"))
((block_comment) @comment.warning
  (#match? @comment.warning "WARNING"))
((block_comment) @comment.warning
  (#match? @comment.warning "FIX"))
((block_comment) @comment.warning
  (#match? @comment.warning "HACK"))
((line_comment) @comment.error
  (#match? @comment.error "ERROR"))
((line_comment) @comment.error
  (#match? @comment.error "FIXME"))
((line_comment) @comment.error
  (#match? @comment.error "DEPRECATED"))
((block_comment) @comment.error
  (#match? @comment.error "ERROR"))
((block_comment) @comment.error
  (#match? @comment.error "FIXME"))
((block_comment) @comment.error
  (#match? @comment.error "DEPRECATED"))

; Markup
[
  (incomplete_let_binding)
  (incomplete_module_import)
  (incomplete_module_include)
  (incomplete_return_expression)
  (incomplete_set_rule)
  (incomplete_show_rule)
  (malformed_automatic_link)
  (malformed_embedded_code)
  (malformed_escape)
  (malformed_number)
] @error

(heading) @markup.heading
((heading marker: (heading_marker) @_heading_marker) @markup.heading.1
  (#eq? @_heading_marker "="))
((heading marker: (heading_marker) @_heading_marker) @markup.heading.2
  (#eq? @_heading_marker "=="))
((heading marker: (heading_marker) @_heading_marker) @markup.heading.3
  (#eq? @_heading_marker "==="))
((heading marker: (heading_marker) @_heading_marker) @markup.heading.4
  (#eq? @_heading_marker "===="))
((heading marker: (heading_marker) @_heading_marker) @markup.heading.5
  (#eq? @_heading_marker "====="))
((heading marker: (heading_marker) @_heading_marker) @markup.heading.6
  (#eq? @_heading_marker "======"))
(bullet_list_item marker: (bullet_list_marker) @markup.list)
(numbered_list_item marker: (numbered_list_marker) @markup.list)
(term_list_item marker: (term_list_marker) @markup.list)
(strong) @markup.strong
(emphasis) @markup.italic
(smart_quote) @character.special
(shorthand) @character.special
(escape) @string.escape
(linebreak) @character.special
(bracketed_text ["[" "]"] @punctuation.bracket)
(automatic_link) @markup.link.url
(label) @label
(reference) @markup.link
(reference "@" @punctuation.special)
(reference target: (identifier) @markup.link.label)

; Raw text and language tags
(raw
  (raw_delimiter) @punctuation.special)
(raw language: (raw_language) @label)
(raw !language content: (raw_content) @markup.raw)
((raw
  (raw_delimiter) @_raw_delimiter
  !language
  content: (raw_content) @markup.raw.block)
  (#match? @_raw_delimiter "^```"))

; Cross-mode punctuation.
(embedded_code "#" @punctuation.special)
(equation "$" @punctuation.special)

[
  "(" ")" "[" "]" "{" "}"
] @punctuation.bracket

[
  "," ":" ";"
] @punctuation.delimiter

"." @punctuation.delimiter
(content_block ["[" "]"] @punctuation.special)

; Code keywords and builtins
["let" "set" "show" "context"] @keyword
(if_expression "if" @keyword.conditional)
(else_clause "else" @keyword.conditional)
(for_loop ["for" "in"] @keyword.repeat)
(while_loop "while" @keyword.repeat)
(return_expression "return" @keyword.return)
["import" "include"] @keyword.import
"as" @keyword.operator

(break_expression) @keyword.repeat
(continue_expression) @keyword.repeat

(none) @constant.builtin
(auto) @constant.builtin
(boolean) @boolean
(integer) @number
(float) @number.float
(numeric) @number
(unit) @type.builtin
(string) @string
(module_import source: (string) @string.special.path)
(module_include source: (string) @string.special.path)
(string_escape) @string.escape
(discard_pattern) @variable.builtin

; Definitions and calls
(let_binding
  name: (identifier) @function
  parameters: (parameters))
(let_binding
  name: (identifier) @variable
  !parameters)
(let_binding "=" @operator)
(closure parameters: (identifier) @variable.parameter)
(closure parameters: (discard_pattern) @variable.parameter.builtin)
(parameters (identifier) @variable.parameter)
(parameters (discard_pattern) @variable.parameter.builtin)
(named_parameter name: (identifier) @variable.parameter)
(sink_parameter name: (identifier) @variable.parameter)
(sink_parameter name: (discard_pattern) @variable.parameter.builtin)
(destructuring_pattern (identifier) @variable)
(named_destructuring_item pattern: (identifier) @variable)
(destructuring_sink pattern: (identifier) @variable)
(for_loop pattern: (identifier) @variable)
(named_destructuring_item key: (identifier) @property)
(dictionary_entry key: (identifier) @property)
(named_argument name: (identifier) @variable.parameter)

; Identifier references. Keep this targeted to expression-value positions:
; import items, dictionary keys, parameters, and field names have their own
; captures and should not fall through to generic variable highlighting.
(embedded_code body: (identifier) @variable)
(arguments (identifier) @variable)
(array (identifier) @variable)
(code_block body: (identifier) @variable)
(parenthesized_expression expression: (identifier) @variable)
(let_binding value: (identifier) @variable)
(named_parameter default: (identifier) @variable)
(named_argument value: (identifier) @variable)
(spread value: (identifier) @variable)
(spread_argument value: (identifier) @variable)
(dictionary_entry value: (identifier) @variable)
(binary_expression left: (identifier) @variable)
(binary_expression right: (identifier) @variable)
(unary_expression operand: (identifier) @variable)
(logical_not_expression operand: (identifier) @variable)
(assignment_expression left: (identifier) @variable)
(assignment_expression right: (identifier) @variable)
(destructuring_assignment value: (identifier) @variable)
(if_expression condition: (identifier) @variable)
(for_loop iterable: (identifier) @variable)
(while_loop condition: (identifier) @variable)
(return_expression value: (identifier) @variable)
(contextual_expression body: (identifier) @variable)
(field_access object: (identifier) @variable)

(field_access field: (identifier) @variable.member)
(function_call function: (identifier) @function.call)
(function_call function: (field_access field: (identifier) @function.method.call))
(set_rule target: (identifier) @function.builtin)
(set_rule target: (field_access field: (identifier) @function.builtin))
(show_rule selector: (identifier) @function.builtin)
(show_rule selector: (field_access field: (identifier) @function.builtin))
(module_import alias: (identifier) @module)
(import_item alias: (identifier) @module)
(import_path
  head: (identifier) @module)
(import_path
  tail: (identifier) @module)

; Operators
(unary_expression operator: ["+" "-" "−"] @operator)
(logical_not_expression operator: "not" @keyword.operator)
(assignment_expression operator: ["=" "+=" "-=" "−=" "*=" "/="] @operator)
(binary_expression
  operator: [
    "==" "!=" "<" "<=" ">" ">="
    "+" "-" "−" "*" "/"
  ] @operator)
(binary_expression
  operator: ["or" "and" "in"] @keyword.operator)
(not_in_operator) @keyword.operator
(destructuring_assignment operator: "=" @operator)
(closure "=>" @operator)
(spread ".." @operator)
(spread_argument ".." @operator)
(sink_parameter ".." @operator)
(destructuring_sink ".." @operator)
(wildcard_import "*" @operator)

; Math
(equation) @markup.math
(math_identifier) @constant
(math_letter) @variable
((math_text) @operator
  (#match? @operator "^[+=<>]$"))
((math_text) @character
  (#match? @character "^([^+=<>]|..+)$"))
(math_number) @number
(math_shorthand) @operator
(math_alignment_point) @operator
(math_primes) @operator
(math_field_access field: (math_identifier) @variable.member)
(math_call function: (math_identifier) @function.call)
(math_call function: (math_field_access field: (math_identifier) @function.method.call))
(math_application function: (math_letter) @function.call)
(math_application function: (math_identifier) @function.call)
(math_application function: (math_field_access field: (math_identifier) @function.method.call))
(math_named_argument name: (identifier) @variable.parameter)
(math_spread_argument (math_spread_operator) @operator)
(math_fraction "/" @operator)
(math_root operator: _ @operator)
(math_factorial "!" @operator)
(math_delimiter) @punctuation.bracket
(math_group
  open: (math_delimiter) @punctuation.special
  close: (math_delimiter) @punctuation.special)

; Attachments are captured at their structural node instead of trying to match
; arbitrary descendants. This handles subscript, superscript, and grouped prime
; forms consistently.
(math_attachment base: (math_text) @variable)
(math_attachment base: (math_letter) @variable)
(math_attachment base: (math_identifier) @variable)
(math_attachment subscript: (math_text) @variable)
(math_attachment subscript: (math_letter) @variable)
(math_attachment subscript: (math_identifier) @variable)
(math_attachment superscript: (math_text) @variable)
(math_attachment superscript: (math_letter) @variable)
(math_attachment superscript: (math_identifier) @variable)
(math_attachment ["_" "^"] @operator)
(math_attachment primes: (math_primes) @operator)