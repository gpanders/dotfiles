syntax keyword vhdlFunction  falling_edge rising_edge
syntax keyword vhdlFunction  to_unsigned to_signed to_integer

syntax keyword vhdlStatement function nextgroup=vhdlFunction skipwhite
syntax match   vhdlFunction  "\a\w*\ze(" display contained

syntax match   vhdlLabel     "^\s*\zs\I\i*\ze\s*:[A-Za-z ]" display

syntax region  vhdlPorts start="^\s*\(port\|generic\)\_s*(" skip="([^\)]*)" end=")" contains=vhdlStatement,vhdlType,vhdlNumber,vhdlCharacter,vhdlConstant,vhdlBoolean,vhdlString,vhdlAttribute,vhdlOperator,vhdlComment keepend

syntax clear vhdlSpecial
syntax keyword vhdlSpecial and nand or nor xor xnor
syntax keyword vhdlSpecial rol ror sla sll sra srl
syntax keyword vhdlSpecial mod rem abs not

syntax region  vhdlIf matchgroup=vhdlConditional contains=ALL start="^\s*\<\%(els\)\?if\>" end="\<then\>"
syntax region  vhdlCase matchgroup=vhdlConditional contains=ALL start="^\s*\<case\>" end="\<is\>"
syntax keyword vhdlConditional when else
syntax match   vhdlConditional "^\s*end \%(if\|case\)\ze;"

syntax region  vhdlLoop matchgroup=vhdlRepeat contains=ALL start="^\s*\%(for\|while\)\s" end="\<loop\>"
syntax keyword vhdlRepeat contained containedin=vhdlLoop in to downto
syntax match   vhdlRepeat "^\s*end loop\ze;"

" Xilinx specific attributes
syntax keyword vhdlAttribute MARK_DEBUG ASYNC_REG

highlight default link vhdlFunction    Function
highlight default link vhdlLabel       Label
highlight default link vhdlConditional Conditional
highlight default link vhdlRepeat      Repeat
