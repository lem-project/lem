;;; Markdown highlight query for Lem editor
;;; Based on tree-sitter-markdown grammar (block grammar only)

;; Headers - ATX style with h1 marker
(atx_heading
  (atx_h1_marker)
  heading_content: (inline) @text.title.1)

;; Headers - ATX style with h2 marker
(atx_heading
  (atx_h2_marker)
  heading_content: (inline) @text.title.2)

;; Headers - ATX style with h3 marker
(atx_heading
  (atx_h3_marker)
  heading_content: (inline) @text.title.3)

;; Headers - ATX style with h4 marker
(atx_heading
  (atx_h4_marker)
  heading_content: (inline) @text.title.4)

;; Headers - ATX style with h5 marker
(atx_heading
  (atx_h5_marker)
  heading_content: (inline) @text.title.5)

;; Headers - ATX style with h6 marker
(atx_heading
  (atx_h6_marker)
  heading_content: (inline) @text.title.6)

;; Header markers
(atx_h1_marker) @punctuation.special
(atx_h2_marker) @punctuation.special
(atx_h3_marker) @punctuation.special
(atx_h4_marker) @punctuation.special
(atx_h5_marker) @punctuation.special
(atx_h6_marker) @punctuation.special

;; Code blocks
(fenced_code_block) @text.literal
(indented_code_block) @text.literal
(fenced_code_block_delimiter) @punctuation.delimiter
(info_string (language) @string)

;; List markers
(list_marker_plus) @punctuation.special
(list_marker_minus) @punctuation.special
(list_marker_star) @punctuation.special
(list_marker_dot) @punctuation.special
(list_marker_parenthesis) @punctuation.special

;; Block quotes
(block_quote) @markup.quote

;; Thematic breaks (---, ***, ___)
(thematic_break) @punctuation.special
