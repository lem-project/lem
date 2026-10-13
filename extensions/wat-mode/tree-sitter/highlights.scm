;; WAT (WebAssembly Text Format) highlights query for Lem editor
;; Based on tree-sitter-wasm grammar (wasm-lsp/tree-sitter-wasm)
;; Following nvim-treesitter and helix conventions

;; Comments
(comment_line) @comment
(comment_block) @comment

;; Strings
(string) @string
(escape_sequence) @string.escape

;; Numbers
(nat) @number
(float) @number
(nan) @number
(align_offset_value) @number

;; Types
(value_type) @type
(ref_type) @type
(ref_kind) @type
(elem_kind) @type
(global_type_mut) @type

;; Module and function definitions
(module
  (identifier) @function)
(module_field_func
  (identifier) @function)

;; Identifiers (variable names like $foo)
(identifier) @variable

;; Keywords - Module structure
[
  "module"
  "func"
  "param"
  "result"
  "local"
  "global"
  "memory"
  "table"
  "type"
  "import"
  "export"
  "data"
  "elem"
  "start"
  "mut"
  "offset"
] @keyword

;; Control flow keywords
[
  "block"
  "loop"
  "if"
  "then"
  "else"
  "end"
  "br"
  "br_if"
  "br_table"
  "call"
  "call_indirect"
  "return"
  "unreachable"
  "nop"
] @keyword.control

;; Instructions (builtins)
(instr_plain) @function.builtin
(instr_list_call) @function.builtin
(op_table_init) @function.builtin
(op_simd_lane) @function.builtin
(op_simd_const) @function.builtin

;; Pattern matching instructions
(pat00) @function.builtin
(pat01) @function.builtin

;; Annotations
(annotation
  (identifier_pattern) @property)

;; Reserved words
(reserved) @constant

;; Punctuation
"(" @punctuation.bracket
")" @punctuation.bracket
"=" @operator

;; WAST-specific keywords (test format)
[
  "assert_return"
  "assert_trap"
  "assert_exhaustion"
  "assert_malformed"
  "assert_invalid"
  "assert_unlinkable"
  "invoke"
  "get"
  "register"
] @keyword

;; Error nodes (for diagnostics)
(ERROR) @warning
