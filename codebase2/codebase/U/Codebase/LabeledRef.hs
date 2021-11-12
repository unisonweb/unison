module U.Codebase.LabeledRef where
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)

data LabeledRef =
      TypeReference Reference
    | TermReference Reference
    | ConstructorReference Referent
    deriving (Show, Eq, Ord)
