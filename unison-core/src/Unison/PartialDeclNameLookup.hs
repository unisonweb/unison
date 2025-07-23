module Unison.PartialDeclNameLookup
  ( PartialDeclNameLookup (..),
    expectDeclName,
    expectConstructorNames,
  )
where

import Data.Map.Strict qualified as Map
import Unison.Name (Name)
import Unison.Prelude

-- | Like a @DeclNameLookup@, but "partial" / more lenient - because we don't require the LCA of a merge to have a full
-- @DeclNameLookup@.
data PartialDeclNameLookup = PartialDeclNameLookup
  { constructorToDecl :: !(Map Name Name),
    declToConstructors :: !(Map Name [Maybe Name])
  }
  deriving stock (Generic)

expectDeclName :: (HasCallStack) => PartialDeclNameLookup -> Name -> Name
expectDeclName PartialDeclNameLookup {constructorToDecl} x =
  case Map.lookup x constructorToDecl of
    Nothing -> error (reportBug "E874908" ("Expected constructor name key " <> show x <> " in partial decl name lookup"))
    Just y -> y

expectConstructorNames :: (HasCallStack) => PartialDeclNameLookup -> Name -> [Maybe Name]
expectConstructorNames PartialDeclNameLookup {declToConstructors} x =
  case Map.lookup x declToConstructors of
    Nothing -> error (reportBug "E800097" ("Expected decl name key " <> show x <> " in partial decl name lookup"))
    Just y -> y
