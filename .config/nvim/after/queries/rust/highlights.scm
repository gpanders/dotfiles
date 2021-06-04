; Don't highlight types in import statements
(use_list (identifier) @none (#match? @none "^[A-Z]"))
(use_as_clause alias: (identifier) @none (#match? @none "^[A-Z]"))

; Don't highlight types in scoped identifiers
((scoped_identifier
  path: (identifier) @none)
 (#match? @none "^[A-Z]"))
((scoped_identifier
    name: (identifier) @none)
 (#match? @none "^[A-Z]"))
