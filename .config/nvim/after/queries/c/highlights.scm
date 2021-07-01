(char_literal (escape_sequence) @character)

(preproc_def
  name: (_) @none)
(enumerator
  name: (identifier) @field)

[
  "#if"
  "#ifdef"
  "#ifndef"
  "#else"
  "#elif"
  "#endif"
  (preproc_directive)
] @constant.macro

[
 (preproc_arg)
 (preproc_defined)
 (preproc_function_def)
] @none

; In enum/struct definitions, don't highlight the name of the new object
(enum_specifier
  name: (_) @none
  body: (_))

(struct_specifier
  name: (_) @none
  body: (_))

; When using enum/struct types, highlight both the enum/struct keyword and the
; object name as a @type
(enum_specifier
  "enum" @type
  name: (_) @type .)

(struct_specifier
  "struct" @type
  name: (_) @type .)
