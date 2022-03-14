(function_definition
  (
    (type_qualifier)?
    (storage_class_specifier)?
    type: (_)
    (type_qualifier)? @context.text
  ) @context.text
  declarator: [
    (function_declarator)
    (pointer_declarator declarator: (function_declarator))
  ] @context.text) @context
