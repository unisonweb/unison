module U.Codebase.Decl
  ( Decl,
    Type,
    Modifier (..),
    DeclR (..),

    -- * Hashing stuff
    V (..),
    F (..),
    dependencies,
  )
where

import U.Codebase.Reference (TypeRReference)
import U.Codebase.Type (TypeD, TypeR)
import U.Codebase.Type qualified as Type
import Unison.ConstructorType (ConstructorType)
import Unison.Prelude

type Decl v = DeclR TypeRReference v

type Type v = TypeD v

data Modifier = Structural | Unique !Text
  deriving (Eq, Ord, Show)

data DeclR r v = DataDeclaration
  { declType :: ConstructorType,
    modifier :: Modifier,
    bound :: [v],
    constructorTypes :: [TypeR r v]
  }
  deriving (Show)

-- * Hashing stuff

dependencies :: (Ord r, Ord v) => DeclR r v -> Set r
dependencies (DataDeclaration _ _ _ cts) = foldMap Type.dependencies cts

data V v = Bound v | Ctor Int

data F a
  = Type (Type.FD a)
  | LetRec [a] a
  | Constructors [a]
  | Modified ConstructorType Modifier a
  deriving (Functor, Foldable, Show)
