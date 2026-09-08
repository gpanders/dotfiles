(local annotation-text-schema
  {:type "string"
   :minLength 1
   :maxLength 4000
   :pattern "^(?:[^\\n]*\\n){0,49}[^\\n]*$"
   :description "User-visible explanation. Maximum 50 lines."})

(local annotation-range-schema
  {:anyOf
   [{:type "object"
     :required ["kind" "startLine" "endLine" "text"]
     :properties
     {:kind {:type "string" :const "block"}
      :startLine {:type "integer"
                  :minimum 1
                  :description "First line of the block annotation"}
      :endLine {:type "integer"
                :minimum 1
                :description "Last line of the block annotation. Must not precede startLine."}
      :text annotation-text-schema}
     :additionalProperties false}
    {:type "object"
     :required ["kind" "line" "startCharacter" "endCharacter" "text"]
     :properties
     {:kind {:type "string" :const "inline"}
      :line {:type "integer"
             :minimum 1
             :description "Line containing the inline annotation"}
      :startCharacter {:type "integer"
                       :minimum 0
                       :description "Zero-based Unicode code-point offset where the inline annotation starts"}
      :endCharacter {:type "integer"
                     :minimum 0
                     :description "Exclusive zero-based Unicode code-point offset where the inline annotation ends. Must be greater than startCharacter."}
      :text annotation-text-schema}
     :additionalProperties false}]})

{:label "Annotate"
 :description "Add user-visible explanations to block or inline ranges in files. A block marks complete lines. An inline range marks characters within one line. Ranges must be disjoint. Set adds annotations to a namespace without changing prior annotations. Clear removes all annotations in the namespace. Explanations preserve explicit lines and wrap to the available width."
 :retrySafe false
 :parameters
 {:type "object"
  :required ["action" "namespace" "annotations"]
  :properties
  {:action {:type "string"
            :enum ["set" "clear"]
            :description "Set adds annotations. Clear removes all annotations in the namespace."}
   :namespace {:type "string"
               :minLength 1
               :maxLength 100
               :description "Annotation group"}
   :present {:type "boolean"
             :description "Present the first new range to the user. Defaults to true."}
   :annotations
   {:type "array"
    :maxItems 100
    :description "Files and ranges to add. Set requires at least one and supports at most 100 total ranges. Clear requires an empty array."
    :items
    {:type "object"
     :required ["filename" "ranges"]
     :properties
     {:filename {:type "string"
                 :description "File containing the ranges, relative to the session directory or absolute"}
      :ranges {:type "array"
               :minItems 1
               :maxItems 100
               :description "Disjoint block or inline ranges. Set kind to select the required coordinate fields."
               :items annotation-range-schema}}
     :additionalProperties false}}}
  :additionalProperties false}
 :execute #(let [annotate (require :tool.annotate)]
             (annotate.execute $...))
 :cleanup #(let [annotate (. package.loaded :tool.annotate)]
             (when annotate
               (annotate.clear)))}
