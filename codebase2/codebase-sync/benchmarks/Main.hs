module Main where

import Test.Tasty.Bench
import U.Codebase.Sync2

main :: IO ()
main = do
  defaultMain
    [ bench "sync 100000, Map store" do
        whnfIO do
          store <- makeMapStore NotThreadSafe
          sync store fib 100000,
      bench "sync 1000000, Map store" do
        whnfIO do
          store <- makeMapStore NotThreadSafe
          sync store fib 1000000
    ]

fib :: (Int -> Sync IO Int) -> Int -> Sync IO Int
fib build = \case
  0 -> pure 1
  1 -> pure 1
  n -> do
    x <- build (n -1)
    y <- build (n -2)
    pure (x + y)
