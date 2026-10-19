;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.
;;;;
;;;; Python highlight queries for le-treesitter-packs (tree-sitter-python grammar).
;;;; Later patterns override earlier ones for the same range.

[
  "def" "class" "lambda" "return" "yield" "if" "elif" "else" "for" "while"
  "break" "continue" "pass" "try" "except" "finally" "raise" "with" "as"
  "import" "from" "global" "nonlocal" "assert" "del" "async" "await"
  "match" "case" "in" "not" "and" "or" "is"
] @keyword

(comment) @comment
(string) @string
(escape_sequence) @string.escape
(integer) @number
(float) @number
[(true) (false) (none)] @constant.builtin

(type (identifier) @type)
(parameters (identifier) @variable.parameter)
(typed_parameter (identifier) @variable.parameter)
(default_parameter name: (identifier) @variable.parameter)
(typed_default_parameter name: (identifier) @variable.parameter)

(call function: (identifier) @function.call)
(call function: (attribute attribute: (identifier) @function.method))
(decorator) @function
(function_definition name: (identifier) @function)
(class_definition name: (identifier) @type)

;; docstrings are bare (string) statements at the start of a module or body
(module . (string) @string.documentation)
(function_definition body: (block . (string) @string.documentation))
(class_definition body: (block . (string) @string.documentation))
