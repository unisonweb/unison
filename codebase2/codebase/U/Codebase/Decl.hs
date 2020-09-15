module U.Codebase.Decl where

import Data.Word (Word64)
import U.Codebase.Reference (Reference')
import Data.Text (Text)
import U.Util.Hash (Hash)
import U.Codebase.Type (TypeR)

type ConstructorId = Word64

data DeclType = Data | Effect 
  deriving (Eq, Ord, Show, Enum)

type Decl v = DeclR (Reference' Text Hash) v

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DeclR r v = DataDeclaration {
  declType :: DeclType,
  modifier :: Modifier,
  bound :: [v],
  constructors' :: [TypeR r v]
}

-- instance Hashable ConstructorType where
--   tokens b = [Tag . fromIntegral $ fromEnum b]
