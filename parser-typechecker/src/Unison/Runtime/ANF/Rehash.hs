
module Unison.Runtime.ANF.Rehash where

import Crypto.Hash
import Data.Bifunctor (bimap, second)
import Data.ByteArray (convert)
import Data.ByteString.Lazy (toChunks)
import Data.Graph as Gr
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Unison.Hash (fromByteString)
import Unison.Reference as Reference
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Serialize as ANF
import Unison.Var (Var)

rehashGroups ::
  Var v =>
  Map.Map Reference (SuperGroup v) ->
  (Map.Map Reference Reference, Map.Map Reference (SuperGroup v))
rehashGroups m = foldl step (Map.empty, Map.empty) sccs
  where
    f p@(r, sg) = (p, r, groupTermLinks sg)

    sccs = stronglyConnComp . fmap f $ Map.toList m

    step (remap, newSGs) scc0 =
      (Map.union remap rm, Map.union newSGs sgs)
      where
      rp b r | not b, Just r <- Map.lookup r remap = r
             | otherwise = r
      scc = second (overGroupLinks rp) <$> scc0
      (rm, sgs) = rehashSCC scc


rehashSCC
  :: Var v
  => SCC (Reference, SuperGroup v)
  -> (Map.Map Reference Reference, Map.Map Reference (SuperGroup v))
rehashSCC scc
  | checkSCC scc = (refreps, newSGs)
  where
    ps = flattenSCC scc
    sample = case fst $ head ps of
      Derived h _ -> h
      _ -> error "rehashSCC: impossible"
    bss = fmap (uncurry $ serializeGroupForRehash mempty) ps
    digest = hashFinalize
           $ foldl' (\cx -> hashUpdates cx . toChunks)
                    (hashInitWith Blake2b_256)
                    bss
    newHash = fromByteString $ convert digest
    replace (Derived h i)
      | h == sample = Derived newHash i
    replace r = r

    replace' = overGroupLinks (\b r -> if b then r else replace r)

    newSGs = Map.fromList $ fmap (bimap replace replace') ps

    refreps = Map.fromList $ fmap (\(r, _) -> (r, replace r)) ps

rehashSCC scc = error $ "unexpected SCC:\n" ++ show scc

checkSCC :: SCC (Reference, SuperGroup v) -> Bool
checkSCC AcyclicSCC{} = True
checkSCC (CyclicSCC []) = True
checkSCC (CyclicSCC (p:ps)) = all (same p) ps
  where
    same (Derived h _, _) (Derived h' _, _) = h == h'
    same _ _ = False
