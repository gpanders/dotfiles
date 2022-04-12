[
 "var"
 "export"
 "error"
 "orelse"
 "fn"
 "pub"
 "return"
 "defer"
] @keyword

(TestDecl "test" @keyword)
(VarDecl "const" @keyword)

(PrefixTypeOp
  [
   "const"
   "volatile"
  ] @type.qualifier)

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

(BUILTINIDENTIFIER) @function.macro
(STRINGLITERALSINGLE) @string
(LINESTRING) @string
(INTEGER) @number
(CHAR_LITERAL) @character
(STRINGLITERALSINGLE (EscapeSequence) @string.escape)

[
 (line_comment)
 (doc_comment)
 (container_doc_comment)
] @comment

(BlockLabel (IDENTIFIER) @label)
(BreakLabel ":" @label (IDENTIFIER) @label)

[
 "extern"
 "inline"
 "comptime"
 "packed"
 "volatile"
] @storageclass

"anytype" @type
(BuildinTypeExpr) @type

function_call: (IDENTIFIER) @function
((IDENTIFIER) @comment (#eq? @comment "_"))
