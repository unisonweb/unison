module U.Codebase.Sqlite.V2.HashHandle
  ( v2HashHandle,
  )
where

import qualified Data.Set as Set
import U.Codebase.Sqlite.HashHandle
import U.Util.Type (removeAllEffectVars)
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, v2ToH2Type, v2ToH2TypeD)
import qualified Unison.Hashing.V2.Type as H2

v2HashHandle :: HashHandle
v2HashHandle =
  HashHandle
    { toReference = h2ToV2Reference . H2.toReference . v2ToH2Type . removeAllEffectVars,
      toReferenceMentions = Set.map h2ToV2Reference . H2.toReferenceMentions . v2ToH2Type . removeAllEffectVars,
      toReferenceDecl = \h -> h2ToV2Reference . H2.toReference . v2ToH2TypeD h . removeAllEffectVars,
      toReferenceDeclMentions = \h -> Set.map h2ToV2Reference . H2.toReferenceMentions . v2ToH2TypeD h . removeAllEffectVars
    }
