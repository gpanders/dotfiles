{:label "Present"
 :description "Present a location in a file to the user without moving keyboard focus."
 :retrySafe true
 :execute (fn [arguments cwd]
            (let [present (require :present)
                  result (present.file arguments.path
                                       {:cwd cwd
                                        :line arguments.line
                                        :character arguments.character})]
              {:text (: "Presented %s:%d." :format arguments.path result.line)
               :details {:path result.path :line result.line}}))
 :parameters {:type "object"
              :required ["path"]
              :properties {:path {:type "string"
                                  :description "File to present, relative to the session directory or absolute"}
                           :line {:type "integer"
                                  :minimum 1
                                  :description "One-based line to center"}
                           :character {:type "integer"
                                       :minimum 0
                                       :description "Zero-based Unicode character offset"}}
              :additionalProperties false}}
