; TOML highlights query for tree-sitter
; Based on nvim-treesitter and helix conventions
; Compatible with tree-sitter-toml grammar

; Keys
(bare_key) @property
(quoted_key) @string

; Values - Strings
(string) @string
(escape_sequence) @string.escape

; Values - Numbers
(integer) @number
(float) @number

; Values - Booleans
(boolean) @constant.builtin

; Comments
(comment) @comment

; Date and time values
(offset_date_time) @string.special
(local_date_time) @string.special
(local_date) @string.special
(local_time) @string.special

; Table headers - keys inside tables get @type highlighting
(table
  (bare_key) @type)

(table
  (quoted_key) @type)

(table
  (dotted_key
    (bare_key) @type))

(table
  (dotted_key
    (quoted_key) @type))

; Array of tables headers
(table_array_element
  (bare_key) @type)

(table_array_element
  (quoted_key) @type)

(table_array_element
  (dotted_key
    (bare_key) @type))

(table_array_element
  (dotted_key
    (quoted_key) @type))

; Punctuation - brackets
"[" @punctuation.bracket
"]" @punctuation.bracket
"[[" @punctuation.bracket
"]]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

; Punctuation - delimiters
"." @punctuation.delimiter
"," @punctuation.delimiter

; Operators
"=" @operator
