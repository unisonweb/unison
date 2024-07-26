module Unison.Merge.DeclNameLookup
  ( DeclNameLookup (..),
    expectDeclName,
    expectConstructorNames,
  )
where

import Data.Map.Strict qualified as Map
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Unison.Name (Name)
import Unison.Prelude

-- | A lookup from decl-to-constructor name and vice-versa.
--
-- For example, a type decl like
--
-- @
-- unique type Foo
--   = Bar Int
--   | Baz.Qux Nat Nat
-- @
--
-- is represented as
--
-- @
-- DeclNameLookup
--   { constructorToDecl = Map.fromList [("Foo.Bar", "Foo"), ("Foo.Baz.Qux", "Foo")]
--   , declToConstructors = Map.fromList [("Foo", ["Foo.Bar", "Foo.Baz.Qux"])]
--   }
-- @
--
-- Note that:
--
-- * Constructor names are given "in full", though they will all necessarily begin with the decl's name.
-- * In @declToConstructors@, the constructor names are given in their canonical ordering.
data DeclNameLookup = DeclNameLookup
  { constructorToDecl :: !(Map Name Name),
    declToConstructors :: !(Map Name [Name])
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid DeclNameLookup)

expectDeclName :: (HasCallStack) => DeclNameLookup -> Name -> Name
expectDeclName DeclNameLookup {constructorToDecl} x =
  case Map.lookup x constructorToDecl of
    Nothing -> error (reportBug "E246726" ("Expected constructor name key " <> show x <> " in decl name lookup"))
    Just y -> y

expectConstructorNames :: (HasCallStack) => DeclNameLookup -> Name -> [Name]
expectConstructorNames DeclNameLookup {declToConstructors} x =
  case Map.lookup x declToConstructors of
    Nothing -> error (reportBug "E077058" ("Expected decl name key " <> show x <> " in decl name lookup"))
    Just y -> y
