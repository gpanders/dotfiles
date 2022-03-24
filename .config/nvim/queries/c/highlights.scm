(call_expression function: (identifier) @function)
(sizeof_expression "sizeof" @operator)

(storage_class_specifier) @storageclass
(type_qualifier) @type
(null) @constant

(comment) @comment
[
 (string_literal)
 (system_lib_string)
] @string

(char_literal) @character
(number_literal) @number

[
 (primitive_type)
 (sized_type_specifier
   [
    "unsigned"
    "signed"
   ])
] @type

"typedef" @typedef

[
 "for"
 "while"
 "break"
 "continue"
] @repeat

[
 "if"
 "else"
 "switch"
] @conditional

[
 "case"
 "default"
] @label

[
 "goto"
 "return"
] @keyword

[
 "struct"
 "enum"
 "union"
] @structure

"#include" @include
"#define" @define

[
 "#ifdef"
 "#ifndef"
 "#if"
 "#endif"
 "#else"
] @preproc

(preproc_params (identifier) @identifier)

(preproc_if condition: (number_literal) @v (#eq? @v "0") "#endif" @comment) @comment

; #undef
(preproc_call directive: (preproc_directive) @define)
