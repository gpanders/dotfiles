syn keyword fennelKeyword fn lambda if when match collect icollect each let var set local global _G
syn keyword fennelFunction pairs ipairs print
syn match fennelString /"[^"]\+"/
syn match fennelConstant /:\S\+\>/

hi def link fennelKeyword Keyword
hi def link fennelString String
hi def link fennelConstant Constant
