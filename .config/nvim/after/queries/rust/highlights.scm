[
    (attribute_item)
    (inner_attribute_item)
] @attribute

(meta_item
  (identifier) @attribute
  arguments: (meta_arguments (meta_item (identifier) @attribute) @attribute))

(attribute_item (["[" "]"]) @attribute)
(meta_arguments (["(" ")"]) @attribute)
