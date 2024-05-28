module Unison.Merge.DeclNameLookup
  ( DeclNameLookup (..),
    expectDeclName,
    expectConstructorNames,
    setConstructorNames,
  )
where

import Control.Lens (over)
import Data.Map.Strict qualified as Map
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DataDeclaration
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Syntax.Name qualified as Name
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Nametree (Nametree (..))
import Unison.Var (Var)

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

expectDeclName :: HasCallStack => DeclNameLookup -> Name -> Name
expectDeclName DeclNameLookup {constructorToDecl} x =
  case Map.lookup x constructorToDecl of
    Nothing -> error (reportBug "E246726" ("Expected constructor name key " <> show x <> " in decl name lookup"))
    Just y -> y

expectConstructorNames :: HasCallStack => DeclNameLookup -> Name -> [Name]
expectConstructorNames DeclNameLookup {declToConstructors} x =
  case Map.lookup x declToConstructors of
    Nothing -> error (reportBug "E077058" ("Expected decl name key " <> show x <> " in decl name lookup"))
    Just y -> y

-- | Set the constructor names of a data declaration.
--
-- Presumably this is used because the decl was loaded from the database outside of the context of a namespace, because
-- it's not stored with names there, so we plugged in dummy names like "Constructor1", "Constructor2", ...
--
-- Then, at some point, a `DeclNameLookup` was constructed for the corresponding namespace, and now we'd like to
-- combine the two together to get a Decl structure in memory with good/correct names for constructors.
setConstructorNames :: forall a v. Var v => DeclNameLookup -> Name -> Decl v a -> Decl v a
setConstructorNames declNameLookup name =
  case Map.lookup name declNameLookup.declToConstructors of
    Nothing -> id
    Just constructorNames ->
      over
        (DataDeclaration.declAsDataDecl_ . DataDeclaration.constructors_)
        ( zipWith
            (\realConName (ann, _junkConName, typ) -> (ann, Name.toVar realConName, typ))
            constructorNames
        )
