; Typst indent queries for Lem editor
; Based on @indent and @outdent captures processed by lem-tree-sitter/indent

[
  (code_block)
  (content_block)
  (parenthesized_expression)
  (array)
  (dictionary)
  (parameters)
  (destructuring_pattern)
  (reassignment_pattern)
  (parenthesized_import_list)
  (equation)
  (math_arguments)
  (math_delimited)
  (math_group)
  (list_body)
] @indent

[
  "}"
  "]"
  ")"
] @outdent

[
  "else"
] @outdent