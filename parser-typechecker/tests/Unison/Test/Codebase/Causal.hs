{-# LANGUAGE PartialTypeSignatures #-}
--{-# LANGUAGE #-}

module Unison.Test.Codebase.Causal where

import EasyTest
import Unison.Codebase.Causal (Causal(Cons, Merge), RawHash(..), one, currentHash, before)
import qualified Unison.Codebase.Causal as Causal
import Control.Monad.Trans.State (State, state)
import Data.Int (Int64)
import qualified Data.Map as Map
import Control.Monad.Extra (ifM)
import Control.Applicative (liftA2)
import Data.List (foldl1')
import Data.Functor ((<&>))
import Unison.Hashable (Hashable)

c :: State [[Int64]] (Causal (State [[Int64]]) h [Int64])
c = merge (foldr cons (one [1]) t1)
          (foldr cons (foldr cons (one [1]) t2) t3)
  where
  t1, t2, t3 :: [[Int64]]
  t1 = fmap pure [5,4..2]
  t2 = fmap pure [10,9..2]
  t3 = fmap pure [999,998]

test :: Test ()
test = scope "causal" . tests $ []
--  [ scope "foldHistoryUntil" . expect $ execState c mempty == Set.fromList [3,2,1]]

type M = State [[Int64]]
-- special cons and merge that mess with state monad for logging
cons :: [Int64]
     -> Causal M h [Int64]
     -> Causal M h [Int64]

merge :: Causal M h [Int64]
      -> Causal M h [Int64]
      -> M (Causal M h [Int64])

(cons, merge) = (cons'' pure, merge'' pure)
  where
  pure :: Causal m h [Int64] -> M (Causal m h [Int64])
  pure c = state (\s -> (c, Causal.head c : s))

cons'' :: Hashable e1
       => (Causal m1 h e2 -> m2 (Causal m2 h e1))
       -> e1 -> Causal m1 h e2 -> Causal m2 h e1
cons'' pure e tl =
  Cons (RawHash $ Causal.hash [Causal.hash e, unRawHash . currentHash $ tl]) e (currentHash tl, pure tl)

merge'' :: (Monad m, Semigroup e)
        => (Causal m h e -> m (Causal m h e))
        -> Causal m h e -> Causal m h e -> m (Causal m h e)
merge'' pure a b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
    (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
    (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (a, b) ->
      merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]
  where
  merge0 m =
    let e = if Map.null m
          then error "Causal.merge0 empty map"
          else foldl1' (liftA2 (<>)) (fmap Causal.head <$> Map.elems m)
        h = Causal.hash (Map.keys m) -- sorted order
    in  e <&> \e -> Merge (RawHash h) e m

