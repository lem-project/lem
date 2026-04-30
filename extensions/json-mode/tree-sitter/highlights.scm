; JSON highlights query for tree-sitter
; Based on nvim-treesitter and helix conventions

; Strings
(string) @string
(escape_sequence) @string.escape

; Numbers
(number) @number

; Booleans and null
(true) @boolean
(false) @boolean
(null) @constant.builtin

; Object keys (property names)
(pair
  key: (string) @property)

; Punctuation
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
":" @punctuation.delimiter
"," @punctuation.delimiter

; Comments (JSON5 extension, not standard JSON)
(comment) @comment
