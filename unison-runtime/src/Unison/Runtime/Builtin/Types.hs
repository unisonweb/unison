module Unison.Runtime.Builtin.Types
  ( typeReferences,
    builtinTypeNumbering,
    builtinTypeBackref,
    anyTypeNumber,
    booleanTypeNumber,
  )
where

import Data.Map qualified as Map
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Runtime.Builtin.Decls qualified as Ty
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC

-- | The numbers of the types the runtime tests values against on hot
-- paths (every comparison of two constructors, every boolean read:
-- 'Unison.Runtime.TypeTags.isAnyTag'). They are literals so that the test
-- compares against an immediate, and the numbering below is built from
-- them, so it agrees by construction.
anyTypeNumber, booleanTypeNumber :: Word64
anyTypeNumber = 1
{-# INLINE anyTypeNumber #-}
booleanTypeNumber = 2
{-# INLINE booleanTypeNumber #-}

builtinTypeNumbering :: Map Reference Word64
builtinTypeNumbering = Map.fromList typeReferences

-- | The fixed numbers first, then every other builtin type in order.
typeReferences :: [(Reference, Word64)]
typeReferences = fixed ++ zip rest [fromIntegral (length fixed) + 1 ..]
  where
    fixed = [(Ty.anyRef, anyTypeNumber), (Ty.booleanRef, booleanTypeNumber)]
    rest = filter (`notElem` map fst fixed) rs
    rs =
      Ty.builtinTypeRefs
        ++ Ty.builtinDataDeclRefs
        ++ Ty.builtinEffectDeclRefs

builtinTypeBackref :: EnumMap Word64 Reference
builtinTypeBackref = mapFromList $ swap <$> typeReferences
  where
    swap (x, y) = (y, x)
