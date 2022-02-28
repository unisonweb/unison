module Unison.Test.Util.Relation where

import EasyTest
import Control.Monad
import qualified Unison.Util.Relation as R
import qualified Data.Set as Set

test :: Test ()
test = scope "util.relation" . tests $ [
  scope "searchDom" $ do
    replicateM_ 100 $ do
      -- check that `searchDom` gives equivalent results to linear search
      -- through all the pairs in the relation
      n <- int' 0 100
      q <- int' (-5) 20
      triples <- listOf n (liftM3 (,,) (int' 0 10) (int' 0 20) (int' 0 1000))
      let pairs = [((x,y),z) | (x,y,z) <- triples ]
      let r = R.fromList pairs
      expect' $
        R.searchDom (\(x,_) -> compare x q) r
        ==
        Set.fromList [ z | ((x,_),z) <- pairs, x == q ]
    ok
  ]
