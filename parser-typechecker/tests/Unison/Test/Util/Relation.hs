module Unison.Test.Util.Relation where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import EasyTest
import qualified Unison.Util.Relation as R

test :: Test ()
test =
  scope "util.relation" . tests $
    [ scope "searchDom" $ do
        replicateM_ 100 $ do
          -- check that `searchDom` gives equivalent results to linear search
          -- through all the pairs in the relation
          n <- int' 0 100
          q <- int' (-5) 20
          triples <- listOf n (liftM3 (,,) (int' 0 10) (int' 0 20) (int' 0 1000))
          let pairs = [((x, y), z) | (x, y, z) <- triples]
          let r = R.fromList pairs
          expect' $
            R.searchDom (\(x, _) -> compare x q) r
              == Set.fromList [z | ((x, _), z) <- pairs, x == q]
        ok,
      scope "(restrict/subtract)Dom" $ do
        replicateM_ 100 $ do
          n <- int' 0 100
          pairs <- listOf n (liftM2 (,) (int' 0 10) (int' 0 1000))
          let r = R.fromList pairs
          forM_ (R.dom r) $ \i -> do
            expect' $
              R.restrictDom (Set.singleton i) r
                == R.fromMultimap (Map.singleton i (R.lookupDom i r))
            expect' $ R.subtractDom (Set.singleton i) r == R.deleteDom i r
        ok
    ]
