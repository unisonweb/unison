module U.Codebase.Sqlite.HashHandle
  ( HashHandle (..),
  )
where

import qualified U.Codebase.Reference as C
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Term as C.Term
import qualified U.Codebase.Type as C.Type
import U.Util.Hash (Hash)
import Unison.Prelude

data HashHandle = HashHandle
  { -- | Hash type
    toReference :: C.Term.Type Symbol -> C.Reference,
    -- | Hash type's mentions
    toReferenceMentions :: C.Term.Type Symbol -> Set C.Reference,
    -- | Hash decl
    toReferenceDecl :: Hash -> C.Type.TypeD Symbol -> C.Reference,
    -- | Hash decl's mentions
    toReferenceDeclMentions :: Hash -> C.Type.TypeD Symbol -> Set C.Reference
  }
