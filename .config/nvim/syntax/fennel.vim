syn keyword fennelKeyword fn lambda if when match collect icollect each let var set local global macro macros accumulate
syn keyword fennelFunction pairs ipairs print require setmetatable getmetatable assert error pcall xpcall unpack select loadstring dofile getfenv setfenv _G

syn match fennelString /"[^"]\+"/
syn match fennelConstant ":\S\+\>"
syn match fennelComment ";.*$"

hi def link fennelKeyword Keyword
hi def link fennelFunction Function
hi def link fennelString String
hi def link fennelConstant Constant
hi def link fennelComment Comment
