; Don't highlight types in import statements
(use_list (identifier) @none (#match? @none "^[A-Z]"))
(use_as_clause alias: (identifier) @none (#match? @none "^[A-Z]"))

(self) @none

(struct_item name: (type_identifier) @none)
(impl_item type: (type_identifier) @none)

; Don't highlight types in scoped identifiers
((scoped_identifier
  path: (identifier) @none)
 (#match? @none "^[A-Z]"))
((scoped_identifier
    name: (identifier) @none)
 (#match? @none "^[A-Z]"))

; Highlight macros as @macro instead of @function.macro
(macro_invocation
  macro: (identifier) @macro)
(macro_invocation
  macro: (scoped_identifier
           (identifier) @macro .))
