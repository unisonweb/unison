module U.Codebase.Sqlite.Decl.Format where

import U.Codebase.Decl (DeclType, Modifier)
import U.Codebase.Reference (Reference')
-- import U.Codebase.Sqlite.DbId
import qualified U.Codebase.Type as Type
import qualified U.Core.ABT as ABT
import U.Codebase.Sqlite.LocalIds

-- | Add new formats here
data TermFormat v = Term (LocallyIndexedComponent v)

-- | V1: Decls included `Hash`es inline
--   V2: Instead of `Hash`, we use a smaller index.
data LocallyIndexedComponent v = LocallyIndexedComponent
  { lookup :: LocalIds,
    component :: [Decl v]
  }

data Decl v = DataDeclaration
  { declType :: DeclType,
    modifier :: Modifier,
    bound :: [v],
    constructors' :: [Type v]
  }

type Type v = ABT.Term (Type.F' TypeRef) v ()
type TypeRef = Reference' LocalTextId (Maybe LocalTypeId)

-- Int, because that's what Data.Vector.(!) takes
newtype LocalTextId = LocalTextId Int
newtype LocalTypeId = LocalTypeId Int
