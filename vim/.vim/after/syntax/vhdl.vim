syntax keyword vhdlFunction  falling_edge rising_edge
syntax keyword vhdlFunction  to_unsigned to_signed to_integer

syntax keyword vhdlStatement function procedure nextgroup=vhdlFunction skipwhite
syntax match   vhdlFunction  "\a\w*\ze(" display contained

syntax match   vhdlLabel     "^\s*\zs\I\i*\ze\s*:[A-Za-z ]" display

syntax region  vhdlPorts start="\s\?\<\%(port\|generic\)\_s*(" skip="([^\)]*)" end=")" contains=vhdlStatement,vhdlType,vhdlNumber,vhdlCharacter,vhdlConstant,vhdlBoolean,vhdlString,vhdlAttribute,vhdlOperator,vhdlComment keepend

syntax clear vhdlSpecial
syntax keyword vhdlSpecial and nand or nor xor xnor
syntax keyword vhdlSpecial rol ror sla sll sra srl
syntax keyword vhdlSpecial mod rem abs not

syntax region  vhdlIf matchgroup=vhdlConditional contains=TOP start="\s\?\<\%(els\)\?if\s" end="\sthen\>"
syntax region  vhdlIf matchgroup=vhdlConditional contains=TOP start="\s\?\<if\ze .\{-} generate\>" end="\s\?\<end generate\ze;"
syntax region  vhdlCase matchgroup=vhdlConditional contains=TOP start="\s\?\<case\s" end="\sis\>"
syntax keyword vhdlConditional when else
syntax keyword vhdlConditional contained containedin=vhdlIf generate
syntax match   vhdlConditional "\s\?\<end \%(if\|case\)\ze;"

syntax region  vhdlLoop matchgroup=vhdlRepeat contains=TOP start="\s\?\<\%(for\|while\)\s" end="\sloop\>"
syntax region  vhdlLoop matchgroup=vhdlRepeat contains=TOP start="\s\?\<for\ze .\{-} generate\>" end="\s\?\<end generate\ze;"
syntax keyword vhdlRepeat contained containedin=vhdlLoop in to downto
syntax keyword vhdlRepeat contained containedin=vhdlLoop generate
syntax match   vhdlRepeat "\s\?\<end loop\ze;"

" Xilinx specific attributes
syntax keyword vhdlAttribute MARK_DEBUG ASYNC_REG

highlight default link vhdlFunction    Function
highlight default link vhdlLabel       Label
highlight default link vhdlConditional Conditional
highlight default link vhdlRepeat      Repeat
