(call_expression function: (identifier) @function)
(sizeof_expression "sizeof" @operator)

(storage_class_specifier) @storageclass
(type_qualifier) @type.qualifier
(null) @constant.builtin

(comment) @comment
[
 (string_literal)
 (system_lib_string)
] @string

(char_literal) @character
(number_literal) @number

[
 (primitive_type)
 (type_identifier)
 (sized_type_specifier
   [
    "unsigned"
    "signed"
   ])
] @type

[
 "for"
 "while"
 "break"
 "continue"
 "if"
 "else"
 "switch"
 "case"
 "goto"
 "return"
]
@keyword

"#include" @include

[
 "#ifdef"
 "#ifndef"
 "#if"
 "#endif"
 "#else"
]
@preproc

[
 "#define"
]
@define

(preproc_params (identifier) @preproc.parameter)

; #undef
(preproc_call directive: (preproc_directive) @define)
