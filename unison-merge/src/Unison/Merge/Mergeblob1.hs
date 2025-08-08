module Unison.Merge.Mergeblob1
  ( hydratedDefnsLabeledDependencies,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration.Dependencies qualified as Decl
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (TermReferenceId, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Util.Defns (Defns (..), DefnsF)

-- | Get a names object for all the hydrated definitions AND their direct dependencies
hydratedDefnsLabeledDependencies ::
  DefnsF
    (Map Name)
    (TermReferenceId, (Term Symbol Ann, Type Symbol Ann))
    (TypeReferenceId, Decl Symbol Ann) ->
  Set LD.LabeledDependency
hydratedDefnsLabeledDependencies defns =
  let termDeps :: Set LD.LabeledDependency
      termDeps =
        foldOf
          ( folded
              . beside
                (to Reference.DerivedId . to LD.TermReference . to Set.singleton)
                (beside (to Term.labeledDependencies) (to Type.labeledDependencies))
          )
          defns.terms

      typeDeps :: Set LD.LabeledDependency
      typeDeps =
        defns.types
          & foldMap \(typeRefId, typeDecl) ->
            Decl.labeledDeclDependenciesIncludingSelfAndFieldAccessors (Reference.DerivedId typeRefId) typeDecl
   in Set.union termDeps typeDeps
