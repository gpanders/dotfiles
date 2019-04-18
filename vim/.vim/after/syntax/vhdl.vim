syn keyword vhdlFunction  falling_edge rising_edge
syn keyword vhdlFunction  to_unsigned to_signed to_integer

syn keyword vhdlStatement function nextgroup=vhdlFunction skipwhite
syn match   vhdlFunction  "\h\w*" display contained

hi def link vhdlFunction Function
