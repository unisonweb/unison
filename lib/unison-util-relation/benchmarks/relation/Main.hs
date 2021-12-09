module Main where

import Control.Monad
import qualified Data.Set as Set
import System.Random
import Test.Tasty.Bench
import Unison.Prelude
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

main :: IO ()
main =
  defaultMain
    [ env (genRelations @Char @Char 10000 20) \rs ->
        bgroup
          "Relation"
          [ bench "difference" $ nf (uncurry R.difference) rs,
            bench "intersection" $ nf (uncurry R.intersection) rs,
            bench "union" $ nf (uncurry R.union) rs
          ],
      env (pure (R.fromList ((,) <$> [(1 :: Int) .. 1000] <*> [(1 :: Int) .. 1000]))) \r ->
        bgroup
          "replaceDom"
          [ bench "old implementation" (nf (oldReplaceDom 1 2) r),
            bench "new implementation" (nf (R.replaceDom 2 2) r)
          ],
      env (genRelation @Char @Char 10000 2) \r ->
        env (genSet @Char 100) \s ->
          bgroup
            "Relation"
            [ bgroup
                "subtractDom"
                [ bench "old implementation" (nf (oldSubtractDom s) r),
                  bench "new implementation" (nf (R.subtractDom s) r)
                ]
            ]
    ]

oldReplaceDom :: (Ord a, Ord b) => a -> a -> Relation a b -> Relation a b
oldReplaceDom a a' r =
  foldl' (\r b -> R.insert a' b $ R.delete a b r) r (R.lookupDom a r)

oldSubtractDom :: (Ord a, Ord b) => Set a -> Relation a b -> Relation a b
oldSubtractDom s r =
  R.fromList [(a, b) | (a, b) <- R.toList r, not (a `Set.member` s)]

genRelation :: (Ord a, Ord b, Random a, Random b) => Int -> Int -> IO (Relation a b)
genRelation numDomain rangeValuesPerDomain = do
  let genPairs = do
        k <- randomIO
        vs <- replicateM rangeValuesPerDomain ((k,) <$> randomIO)
        pure vs
  R.fromList . concat <$> replicateM numDomain genPairs

genRelations ::
  (Random a, Random b, Ord a, Ord b) =>
  Int ->
  Int ->
  IO (Relation a b, Relation a b)
genRelations numDomain rangeValuesPerDomain =
  (,) <$> genRelation numDomain rangeValuesPerDomain <*> genRelation numDomain rangeValuesPerDomain

genSet :: (Ord a, Random a) => Int -> IO (Set a)
genSet n =
  Set.fromList <$> replicateM n randomIO
