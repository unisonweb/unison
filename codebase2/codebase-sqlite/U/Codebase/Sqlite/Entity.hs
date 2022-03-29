module U.Codebase.Sqlite.Entity where

import U.Codebase.Decl
import Unison.Prelude

data EntityKind
  = TermEntity
  | DeclEntity
  | PatchEntity

newtype ConstructorIndex = ConstructorIndex Word64

newtype ComponentIndex = ComponentIndex Word64

data Entity = Entity
  { entityKind :: EntityKind,
    entityRef :: EntityRef
  }

type ComponentIndex = Word64

data EntityRef
  = Builtin TextId
  | TermRef ObjectId ComponentIndex (Maybe ConstructorIndex)
  | DeclRef ObjectId ComponentIndex
  | PatchRef ObjectId

expandRef :: EntityRef -> (Maybe TextId, Maybe ObjectId, Maybe ComponentIndex, Maybe ConstructorId)
expandRef = \case
  Builtin tid -> (Just tid, Nothing, Nothing, Nothing)
  TermRef oid compInd conInd -> (Nothing, Just oid, Just compInd, conInd)
  DeclRef oid compInd -> (Nothing, Just oid, Just compInd, Nothing)
  PatchRef oid -> (Nothing, Just oid, Nothing, Nothing)

termEntity :: Referent'' -> Entity
termEntity referent =
  let (ref, mayConId) = case referent of
        C.Ref termRef -> (termRef, Nothing)
        C.Con typeRef conId -> (typeRef, Just conId)
   in case ref of
        ReferenceBuiltin builtinTextId -> Entity TermEntity (Builtin builtinTextId)
        ReferenceDerived (Id objId componentIndex) -> Entity TermEntity (TermRef objId componentIndex mayConId)

declEntity :: Reference -> Entity
declEntity = _

patchEntity :: ObjectId -> Entity
patchEntity oId = Entity PatchEntity (PatchRef oId)
