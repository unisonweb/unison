module Main where
import Criterion.Main
import qualified Unison.Util.Relation as R
import System.Random
import Control.Monad

genRelations :: (Random a, Random b, Ord a, Ord b)
             => Int -> Int -> IO (R.Relation a b, R.Relation a b)
genRelations numDomain rangeValuesPerDomain = do
  let genPairs = do
        k <- randomIO
        vs <- replicateM rangeValuesPerDomain ((k,) <$> randomIO)
        pure vs
  r1 <- R.fromList . concat <$> (replicateM numDomain genPairs)
  r2 <- R.fromList . concat <$> (replicateM numDomain genPairs)
  pure (r1, r2)

main :: IO ()
main = defaultMain
  [ env (genRelations @Char @Char 10000 20) $ \rs ->
    bgroup "Relation"
    [ bench "difference"   $ nf (uncurry R.difference) rs
    , bench "intersection" $ nf (uncurry R.intersection) rs
    , bench "union"        $ nf (uncurry R.union) rs
    ]
  ]
