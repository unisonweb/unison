module Main where
import Criterion.Main
import qualified Unison.Util.Relation as R
import System.Random
import Control.Monad
import qualified Unison.Util.Relation.Diff as D

genRelations :: (Random a, Random b, Ord a, Ord b) => IO (R.Relation a b, R.Relation a b)
genRelations = do
  let genPairs = do
        k <- randomIO
        vs <- replicateM 20 ((k,) <$> randomIO)
        pure vs
  r1 <- R.fromList . concat <$> (replicateM 10000 genPairs)
  r2 <- R.fromList . concat <$> (replicateM 10000 genPairs)
  pure (r1, r2)

main :: IO ()
main = defaultMain
  [ bgroup "diff"
      [ env (genRelations @Char @Char) $ \rs -> bgroup "withEnv"
      [
        bgroup "difference"
            [  bench "single diff"   $ nf (uncurry R.difference) rs
            ,  bench "combined diff" $ nf (D.leftOnly . uncurry R.relationDiff) rs
            ]
      , bgroup "symmetric diff"
            [  bench "single diff"   $ flip nf rs (\(r1, r2) ->
                 (R.difference r1 r2, R.difference r2 r1))
            ,  bench "combined diff" $ flip nf rs (\(r1,r2) ->
                 let d = R.relationDiff r1 r2
                  in (D.leftOnly d, D.rightOnly d)
                 )
            ]
      ]
      ]
  ]
