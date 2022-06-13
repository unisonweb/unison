module U.Codebase.Sqlite.V2.Term
  ( saveTermComponent,
  )
where

import qualified Data.Set as Set
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.Operations as U.Sqlite
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Term as V2
import U.Util.Hash (Hash)
import U.Util.Type (removeAllEffectVars)
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, v2ToH2Type)
import qualified Unison.Hashing.V2.Type as H2
import Unison.Prelude
import Unison.Sqlite

saveTermComponent ::
  -- | The serialized term component if we already have it e.g. via sync
  Maybe ByteString ->
  -- | term component hash
  Hash ->
  -- | term component
  [(V2.Term Symbol, V2.Type Symbol)] ->
  Transaction ObjectId
saveTermComponent =
  U.Sqlite.saveTermComponent
    (h2ToV2Reference . H2.toReference . v2ToH2Type)
    (Set.map h2ToV2Reference . H2.toReferenceMentions . v2ToH2Type . removeAllEffectVars)
