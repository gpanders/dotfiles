[
 "var"
 "const"
 "export"
 "fn"
 "pub"
 "return"
 "defer"
] @keyword

[
 "try"
 "catch"
 "errdefer"
 "unreachable"
] @exception

[
 "while"
 "for"
 "break"
 "continue"
] @repeat

[
 "if"
 "switch"
 "else"
] @conditional

[
 "struct"
 "union"
 "enum"
] @structure

(BUILTINIDENTIFIER) @special
(STRINGLITERALSINGLE) @string
(INTEGER) @number
(CHAR_LITERAL) @character
(STRINGLITERALSINGLE (EscapeSequence) @string.escape)

(line_comment) @comment

(BlockLabel (IDENTIFIER) @label)
(BreakLabel ":" @label (IDENTIFIER) @label)

[
 "extern"
 "inline"
 "comptime"
 "packed"
 "volatile"
] @storageclass

(BuildinTypeExpr) @type

function_call: (IDENTIFIER) @function
((IDENTIFIER) @comment (#eq? @comment "_"))
