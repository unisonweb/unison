module U.Codebase.Sqlite.V2.Decl
  ( saveDeclComponent,
  )
where

import qualified Data.Set as Set
import qualified U.Codebase.Decl as V2
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Operations as U.Sqlite
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Util.Hash (Hash)
import U.Util.Type (removeAllEffectVars)
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, v2ToH2TypeD)
import qualified Unison.Hashing.V2.Type as H2
import Unison.Prelude
import Unison.Sqlite

saveDeclComponent ::
  -- | The serialized decl component if we already have it e.g. via sync
  Maybe ByteString ->
  -- | decl component hash
  Hash ->
  -- | decl component
  [V2.Decl Symbol] ->
  Transaction ObjectId
saveDeclComponent mbs h decls =
  U.Sqlite.saveDeclComponent
    (h2ToV2Reference . H2.toReference . v2ToH2TypeD h)
    (Set.map h2ToV2Reference . H2.toReferenceMentions . v2ToH2TypeD h . removeAllEffectVars)
    mbs
    h
    decls
