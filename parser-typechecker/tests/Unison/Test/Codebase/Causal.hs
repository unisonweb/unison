{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Test.Codebase.Causal where

import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.State (State, put, state)
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Int (Int64)
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import EasyTest
import Unison.Codebase.Causal
  ( Causal (Cons, Merge),
    RawHash (..),
    before,
    currentHash,
    one,
  )
import qualified Unison.Codebase.Causal as Causal
import Unison.CommandLine (beforeHash)
import Unison.Hash (Hash)
import Unison.Hashable (Hashable)

c :: M (Causal M Int64 [Int64])
c =
  merge
    (foldr cons (one [1]) t1)
    (foldr cons (foldr cons (one [1]) t2) t3)
  where
    t1, t2, t3 :: [[Int64]]
    t1 = fmap pure [5, 4 .. 2]
    t2 = fmap pure [100 .. 105]
    t3 = fmap pure [999, 998]

c2 :: M (Causal M Int64 [Int64])
c2 =
  merge
    (foldr cons (one [1]) t1)
    (foldr cons (foldr cons (one [1]) t2) t3)
  where
    t1, t2, t3 :: [[Int64]]
    t1 = fmap pure [5, 4 .. 2]
    t2 = fmap pure [10, 9 .. 2]
    t3 = fmap pure [999, 998]

{-
λ> show Unison.Test.Codebase.Causal.c
"Identity Merge 4gP [999,5] [\"3rG\",\"58U\"]"
λ> runIdentity Unison.Test.Codebase.Causal.result
step a=fromList [1,10] seen=[] rest=fromList [Merge 4gP [999,5] ["3rG","58U"]]
step a=fromList [1,10] seen=["4gP"] rest=fromList [Cons 3rG [999] 4LX,Cons 58U [5] 4vC]
step a=fromList [1,10] seen=["3rG","4gP"] rest=fromList [Cons 58U [5] 4vC,Cons 4LX [998] 26J]
step a=fromList [1,10] seen=["3rG","4gP","58U"] rest=fromList [Cons 4LX [998] 26J,Cons 4vC [4] yFt]
step a=fromList [1,10] seen=["3rG","4LX","4gP","58U"] rest=fromList [Cons 4vC [4] yFt,Cons 26J [100] 4FR]
step a=fromList [1,10] seen=["3rG","4LX","4gP","4vC","58U"] rest=fromList [Cons 26J [100] 4FR,Cons yFt [3] 3So]
step a=fromList [1,10] seen=["26J","3rG","4LX","4gP","4vC","58U"] rest=fromList [Cons yFt [3] 3So,Cons 4FR [101] 4az]
step a=fromList [1,10] seen=["yFt","26J","3rG","4LX","4gP","4vC","58U"] rest=fromList [Cons 4FR [101] 4az,Cons 3So [2] 5Lu]
step a=fromList [1,10] seen=["yFt","26J","3rG","4FR","4LX","4gP","4vC","58U"] rest=fromList [Cons 3So [2] 5Lu,Cons 4az [102] 2V3]
step a=fromList [1,10] seen=["yFt","26J","3So","3rG","4FR","4LX","4gP","4vC","58U"] rest=fromList [Cons 4az [102] 2V3,One 5Lu [1]]
step a=fromList [1,10] seen=["yFt","26J","3So","3rG","4FR","4LX","4az","4gP","4vC","58U"] rest=fromList [One 5Lu [1],Cons 2V3 [103] 5pS]
step a=fromList [10] seen=["yFt","26J","3So","3rG","4FR","4LX","4az","4gP","4vC","58U","5Lu"] rest=fromList [Cons 2V3 [103] 5pS]
step a=fromList [10] seen=["yFt","26J","2V3","3So","3rG","4FR","4LX","4az","4gP","4vC","58U","5Lu"] rest=fromList [Cons 5pS [104] 2tq]
step a=fromList [10] seen=["yFt","26J","2V3","3So","3rG","4FR","4LX","4az","4gP","4vC","58U","5Lu","5pS"] rest=fromList [Cons 2tq [105] 5Lu]
step a=fromList [10] seen=["yFt","26J","2V3","2tq","3So","3rG","4FR","4LX","4az","4gP","4vC","58U","5Lu","5pS"] rest=fromList [One 5Lu [1]]
step a=fromList [10] seen=["yFt","26J","2V3","2tq","3So","3rG","4FR","4LX","4az","4gP","4vC","58U","5Lu","5pS"] rest=fromList []
Unsatisfied (fromList [10])

λ> runIdentity Unison.Test.Codebase.Causal.result (with c2)
step a=fromList [1,10] seen=[] rest=fromList [Cons 2tg [999] 3AW]
step a=fromList [1,10] seen=["2tg"] rest=fromList [Cons 3AW [998] 33b]
step a=fromList [1,10] seen=["2tg","3AW"] rest=fromList [Cons 33b [10] 2NF]
step a=fromList [1] seen=["2tg","33b","3AW"] rest=fromList [Cons 2NF [9] 57i]
step a=fromList [1] seen=["2NF","2tg","33b","3AW"] rest=fromList [Cons 57i [8] ipV]
step a=fromList [1] seen=["2NF","2tg","33b","3AW","57i"] rest=fromList [Cons ipV [7] 3BZ]
step a=fromList [1] seen=["ipV","2NF","2tg","33b","3AW","57i"] rest=fromList [Cons 3BZ [6] 58U]
step a=fromList [1] seen=["ipV","2NF","2tg","33b","3AW","3BZ","57i"] rest=fromList [Cons 58U [5] 4vC]
step a=fromList [1] seen=["ipV","2NF","2tg","33b","3AW","3BZ","57i","58U"] rest=fromList [Cons 4vC [4] yFt]
step a=fromList [1] seen=["ipV","2NF","2tg","33b","3AW","3BZ","4vC","57i","58U"] rest=fromList [Cons yFt [3] 3So]
step a=fromList [1] seen=["ipV","yFt","2NF","2tg","33b","3AW","3BZ","4vC","57i","58U"] rest=fromList [Cons 3So [2] 5Lu]
step a=fromList [1] seen=["ipV","yFt","2NF","2tg","33b","3AW","3BZ","3So","4vC","57i","58U"] rest=fromList [One 5Lu [1]]
Satisfied (fromList [])
λ>

-}

test :: Test ()
test =
  scope "causal"
    . tests
    $ [ scope "threeWayMerge.ex1"
          . expect
          $ Causal.head testThreeWay
            == Set.fromList [3, 4],
        scope "threeWayMerge.idempotent"
          . expect
          $ testIdempotent oneCausal, -- == oneCausal
          --  $  prop_mergeIdempotent
        scope "threeWayMerge.identity"
          . expect
          $ testIdentity oneCausal emptyCausal,
        --  $  prop_mergeIdentity
        scope "threeWayMerge.commutative"
          . expect
          $ testCommutative (Set.fromList [3, 4]) oneRemoved,
        --  $  prop_mergeCommutative
        {- , scope "threeWayMerge.commonAncestor"
        .  expect
        $  testCommonAncestor
        -- $  prop_mergeCommonAncestor --}
        scope "lca.hasLca" lcaPairTest,
        scope "lca.noLca" noLcaPairTest,
        scope "beforeHash" $ beforeHashTests
      ]

beforeHashTests :: Test ()
beforeHashTests = do
  -- c1 and c2 have unrelated histories
  c1 <- pure $ Causal.one (0 :: Int64)
  c2 <- pure $ Causal.one (1 :: Int64)
  -- c1' and c2' are extension of c1 and c2, respectively
  c1' <- pure $ Causal.cons 2 c1
  c2' <- pure $ Causal.cons 3 c2
  c12 <- Causal.threeWayMerge sillyMerge c1' c2'

  -- verifying basic properties of `before` for these examples
  expect' =<< before c1 c1
  expect' =<< before c1 c12
  expect' =<< before c2 c2
  expect' =<< before c2 c12
  expect' =<< before c2 c2'
  expect' =<< before c1 c1'
  expect' . not =<< before c1 c2
  expect' . not =<< before c2 c1

  -- make sure the search cutoff works -
  -- even though both start with `Causal.one 0`, that's
  -- more than 10 steps back from `longCausal 1000`, so we
  -- want this to be false
  expect' . not =<< before c1 (longCausal (1000 :: Int64))
  ok
  where
    before h c = beforeHash 10 (Causal.currentHash h) c
    sillyMerge _lca l _r = pure l
    longCausal 0 = Causal.one 0
    longCausal n = Causal.cons n (longCausal (n - 1))

int64 :: Test Int64
int64 = random

extend ::
  Int ->
  Causal Identity Hash Int64 ->
  Test (Causal Identity Hash Int64)
extend 0 ca = pure ca
extend n ca = do
  i <- int64
  extend (n -1) (Causal.cons i ca)

lcaPair :: Test (Causal Identity Hash Int64, Causal Identity Hash Int64)
lcaPair = do
  base <- one <$> int64
  ll <- int' 0 20
  lr <- int' 0 20
  (,) <$> extend ll base <*> extend lr base

lcaPairTest :: Test ()
lcaPairTest = replicateM_ 50 test >> ok
  where
    test =
      runIdentity . uncurry Causal.lca <$> lcaPair >>= \case
        Just _ -> pure ()
        Nothing -> crash "expected lca"

noLcaPair ::
  Test (Causal Identity Hash Int64, Causal Identity Hash Int64)
noLcaPair = do
  basel <- one <$> int64
  baser <- one <$> int64
  ll <- int' 0 20
  lr <- int' 0 20
  (,) <$> extend ll basel <*> extend lr baser

noLcaPairTest :: Test ()
noLcaPairTest = replicateM_ 50 test >> ok
  where
    test =
      runIdentity . uncurry Causal.lca <$> noLcaPair >>= \case
        Nothing -> pure ()
        Just _ -> crash "expected no lca"

oneRemoved :: Causal Identity Hash (Set Int64)
oneRemoved =
  foldr
    Causal.cons
    (one (Set.singleton 1))
    (Set.fromList <$> [[2, 3, 4], [1, 2, 3, 4], [1, 2]])

twoRemoved :: Causal Identity Hash (Set Int64)
twoRemoved =
  foldr
    Causal.cons
    (one (Set.singleton 1))
    (Set.fromList <$> [[1, 3, 4], [1, 2, 3], [1, 2]])

testThreeWay :: Causal Identity Hash (Set Int64)
testThreeWay =
  runIdentity $
    threeWayMerge' oneRemoved twoRemoved

setCombine :: Applicative m => Ord a => Set a -> Set a -> m (Set a)
setCombine a b = pure $ a <> b

setDiff :: Applicative m => Ord a => Set a -> Set a -> m (Set a, Set a)
setDiff old new = pure (Set.difference new old, Set.difference old new)

setPatch :: Applicative m => Ord a => Set a -> (Set a, Set a) -> m (Set a)
setPatch s (added, removed) = pure (added <> Set.difference s removed)

-- merge x x == x, should not add a new head, and also the value at the head should be the same of course
testIdempotent :: Causal Identity Hash (Set Int64) -> Bool -- Causal Identity Hash (Set Int64)
testIdempotent causal =
  runIdentity (threeWayMerge' causal causal)
    == causal

-- prop_mergeIdempotent :: Bool
-- prop_mergeIdempotent = and (map testIdempotent (take 1000 generateRandomCausals))

oneCausal :: Causal Identity Hash (Set Int64)
oneCausal = Causal.one (Set.fromList [1])

-- generateRandomCausals :: Causal Identity Hash (Set Int64)
-- generateRandomCausals = undefined

easyCombine ::
  (Monad m, Semigroup d) =>
  (e -> e -> m e) ->
  (e -> e -> m d) ->
  (e -> d -> m e) ->
  (Maybe e -> e -> e -> m e)
easyCombine comb _ _ Nothing l r = comb l r
easyCombine _ diff appl (Just ca) l r = do
  dl <- diff ca l
  dr <- diff ca r
  appl ca (dl <> dr)

threeWayMerge' ::
  Causal Identity Hash (Set Int64) ->
  Causal Identity Hash (Set Int64) ->
  Identity (Causal Identity Hash (Set Int64))
threeWayMerge' = Causal.threeWayMerge (easyCombine setCombine setDiff setPatch)

-- merge x mempty == x, merge mempty x == x
testIdentity :: Causal Identity Hash (Set Int64) -> Causal Identity Hash (Set Int64) -> Bool
testIdentity causal mempty =
  (threeWayMerge' causal mempty)
    == (threeWayMerge' mempty causal)

emptyCausal :: Causal Identity Hash (Set Int64)
emptyCausal = one (Set.empty)

-- merge (cons hd tl) tl == cons hd tl, merge tl (cons hd tl) == cons hd tl
testCommutative :: Set Int64 -> Causal Identity Hash (Set Int64) -> Bool
testCommutative hd tl =
  (threeWayMerge' (Causal.cons hd tl) tl)
    == (threeWayMerge' tl (Causal.cons hd tl))

{-
testCommonAncestor ::
testCommonAncestor =
-}

--  [ scope "foldHistoryUntil" . expect $ execState c mempty == Set.fromList [3,2,1]]

--result :: M (Causal.FoldHistoryResult (Set Int64))
--result = Causal.foldHistoryUntil f (Set.fromList [10, 1]) =<< c2 where
--  f s e = let s' = Set.difference s (Set.fromList e) in (s', Set.null s')

result, result2 :: M (Causal.FoldHistoryResult (Set Int64))
(result, result2) =
  ( Causal.foldHistoryUntil f (Set.fromList [10, 1]) =<< (do c' <- c; put mempty; pure c'),
    Causal.foldHistoryUntil f (Set.fromList [10, 1]) =<< (do c' <- c2; put mempty; pure c')
  )
  where
    f s e = let s' = Set.difference s (Set.fromList e) in (s', Set.null s')

---- special cons and merge that mess with state monad for logging
type M = State [[Int64]]

cons ::
  [Int64] ->
  Causal M h [Int64] ->
  Causal M h [Int64]
merge ::
  Causal M h [Int64] ->
  Causal M h [Int64] ->
  M (Causal M h [Int64])
(cons, merge) = (cons'' pure, merge'' pure)
  where
    pure :: Causal m h [Int64] -> M (Causal m h [Int64])
    pure c = state (\s -> (c, Causal.head c : s))

cons'' ::
  Hashable e1 =>
  (Causal m1 h e2 -> m2 (Causal m2 h e1)) ->
  e1 ->
  Causal m1 h e2 ->
  Causal m2 h e1
cons'' pure e tl =
  Cons (RawHash $ Causal.hash [Causal.hash e, unRawHash . currentHash $ tl]) e (currentHash tl, pure tl)

merge'' ::
  (Monad m, Semigroup e) =>
  (Causal m h e -> m (Causal m h e)) ->
  Causal m h e ->
  Causal m h e ->
  m (Causal m h e)
merge'' pure a b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
    (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
    (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (a, b) ->
      merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]
  where
    merge0 m =
      let e =
            if Map.null m
              then error "Causal.merge0 empty map"
              else foldl1' (liftA2 (<>)) (fmap Causal.head <$> Map.elems m)
          h = Causal.hash (Map.keys m) -- sorted order
       in e <&> \e -> Merge (RawHash h) e m
