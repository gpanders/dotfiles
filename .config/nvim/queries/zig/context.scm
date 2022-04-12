(TopLevelDecl
  (FnProto function: (IDENTIFIER)) . (Block "{" @context.end)) @context

(TopLevelDecl
  (VarDecl variable_type_function: (IDENTIFIER)
           (ErrorUnionExpr
             (SuffixExpr
               (ContainerDecl "{" @context.end))))) @context

(TestDecl (Block "{" @context.end)) @context
