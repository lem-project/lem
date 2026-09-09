; YAML highlights query for tree-sitter
; Based on nvim-treesitter conventions

; Scalars
(boolean_scalar) @boolean
(null_scalar) @constant.builtin
(double_quote_scalar) @string
(single_quote_scalar) @string
(block_scalar) @string
(string_scalar) @string
(escape_sequence) @string.escape

; Numbers
(integer_scalar) @number
(float_scalar) @number

; Comments
(comment) @comment

; Anchors and aliases
(anchor_name) @label
(alias_name) @label

; Tags
(tag) @type

; Directives
(yaml_directive) @keyword
(tag_directive) @keyword
(reserved_directive) @keyword

; Keys in block mappings
(block_mapping_pair
  key: (flow_node
    (double_quote_scalar) @property))

(block_mapping_pair
  key: (flow_node
    (single_quote_scalar) @property))

(block_mapping_pair
  key: (flow_node
    (plain_scalar
      (string_scalar) @property)))

; Keys in flow mappings
(flow_mapping
  (_
    key: (flow_node
      (double_quote_scalar) @property)))

(flow_mapping
  (_
    key: (flow_node
      (single_quote_scalar) @property)))

(flow_mapping
  (_
    key: (flow_node
      (plain_scalar
        (string_scalar) @property))))

; Punctuation
"," @punctuation.delimiter
"-" @punctuation.delimiter
":" @punctuation.delimiter
">" @punctuation.delimiter
"?" @punctuation.delimiter
"|" @punctuation.delimiter

"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

"*" @punctuation.special
"&" @punctuation.special
"---" @punctuation.special
"..." @punctuation.special
