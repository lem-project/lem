;;;; Copyright (c) 2026 Yuri Flores Albuquerque.
;;;; SPDX-License-Identifier: FSL-1.1-MIT
;;;; Licensed under the Functional Source License, Version 1.1 (see LICENSE-FSL).
;;;; Competing commercial use is strictly prohibited.
;;;;
;;;; Python indent queries for le-treesitter-packs (Helix-style @indent/@outdent:
;;;; a line is indented once per enclosing captured node that started on an
;;;; earlier line; @outdent applies to the line the node starts on).
;;;; Clauses that continue a statement (elif/else/except/finally) are not indent
;;;; nodes themselves: their bodies are indented by the enclosing statement, and
;;;; their header lines are outdented back to the statement level.

[
  (function_definition)
  (class_definition)
  (if_statement)
  (for_statement)
  (while_statement)
  (with_statement)
  (try_statement)
  (match_statement)
  (case_clause)
  (argument_list)
  (parameters)
  (list)
  (dictionary)
  (set)
  (tuple)
  (parenthesized_expression)
] @indent

[
  (elif_clause)
  (else_clause)
  (except_clause)
  (finally_clause)
] @outdent

[")" "]" "}"] @outdent
