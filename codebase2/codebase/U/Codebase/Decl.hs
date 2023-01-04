module U.Codebase.Decl where

import U.Codebase.Reference (Reference')
import U.Codebase.Type (TypeR)
import qualified U.Codebase.Type as Type
import Unison.Hash (Hash)
import Unison.Prelude

type ConstructorId = Word64

data DeclType = Data | Effect
  deriving (Eq, Ord, Show, Enum)

type Decl v = DeclR TypeRef v

type TypeRef = Reference' Text (Maybe Hash)

type Type v = TypeR TypeRef v

data Modifier = Structural | Unique Text
  deriving (Eq, Ord, Show)

data DeclR r v = DataDeclaration
  { declType :: DeclType,
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
  | Modified DeclType Modifier a
  deriving (Functor, Foldable, Show)
