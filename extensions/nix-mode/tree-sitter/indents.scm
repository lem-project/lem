; Nix indent queries for Lem editor
; Based on Helix format: @indent increases indentation, @outdent decreases
;
; This file defines indentation rules for the Nix language using tree-sitter queries.
; The captures are processed by lem-tree-sitter/indent module.

; Indent-contributing nodes
; These node types increase indentation by one level
[
  (attrset_expression)
  (rec_attrset_expression)
  (let_expression)
  (list_expression)
  (parenthesized_expression)
  (function_expression)
  (formals)
  (indented_string_expression)
  (with_expression)
  (if_expression)
] @indent

; Closers that trigger outdent
; These tokens decrease indentation when they appear at the start of a line
[
  "}"
  "]"
  ")"
  "''"
] @outdent

; Special handling for let-in expressions
; The "in" keyword should be at the same level as "let"
(let_expression "in" @outdent)

; if-then-else alignment
; "then" and "else" keywords are at the same level as "if"
(if_expression "then" @outdent)
(if_expression "else" @outdent)
