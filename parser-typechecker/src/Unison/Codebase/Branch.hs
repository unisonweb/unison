{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.Branch where

--import Control.Monad (join)
--import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
--import Data.Semigroup (sconcat)
--import Data.Foldable
import qualified Data.Map as Map
--import Unison.Hashable (Hashable)
import Unison.Codebase.Causal (Causal)
--import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Conflicted (Conflicted)
--import qualified Unison.Codebase.Conflicted as Conflicted
import Unison.Codebase.Name (Name)
import Unison.Codebase.NameEdit (NameEdit)
import Unison.Codebase.TermEdit (TermEdit)
import Unison.Codebase.TypeEdit (TypeEdit)
import Unison.Reference (Reference)

data Branch v a =
  Branch { namespace     :: Map Name (Causal NameEdit)
         , edited        :: Map Reference (Causal (Conflicted TermEdit))
         , editedDatas   :: Map Reference (Causal (Conflicted TypeEdit))
         , editedEffects :: Map Reference (Causal (Conflicted TypeEdit))
         }


{-
Denotation:

`Branch` denotes a `(Namespace', Namespace' -> Namespace')`, where `Namespace'`
is a mergeable version of `Namespace` that can be in a conflicted state with
respect to some `Name`.

merge : Branch -> Branch -> Branch
merge (ns1, up1) (ns2, up2) =
  (Map.unionWith Causal.merge ns1 ns2,
   \nsi -> Map.unionWith Causal.merge (up1 nsi) (up2 nsi))

sequence : Branch -> Branch -> Branch
sequence (ns1, up1) (ns2, up2) =
  (Map.unionWith Causal.sequence ns1 ns2, up2 . up1)

-}

merge :: Branch v a -> Branch v a -> Branch v a
merge (Branch n1 t1 d1 e1) (Branch n2 t2 d2 e2) =
  Branch (Map.unionWith (<>) n1 n2)
         (Map.unionWith (<>) t1 t2)
         (Map.unionWith (<>) d1 d2)
         (Map.unionWith (<>) e1 e2)

-- What does this actually do.
--sequence :: Branch v a -> Branch v a -> Branch v a
--sequence (Branch n1 t1 d1 e1) (Branch n2 t2 d2 e2) =
--  Branch (Map.unionWith Causal.sequence n1 n2)
--          (chain ) _

-- example:
-- in b1: foo is replaced with Conflicted (foo1, foo2)
-- in b2: foo1 is replaced with foo3
-- what do we want the output to be?
--    foo  -> Conflicted (foo3, foo2)
--    foo1 -> foo3

-- example:
-- in b1: foo is replaced with Conflicted (foo1, foo2)
-- in b2: foo1 is replaced with foo2
-- what do we want the output to be?
--    foo  -> foo2
--    foo1 -> foo2

-- example:
-- in b1: foo is replaced with Conflicted (foo1, foo2)
-- in b2: foo is replaced with foo2
-- what do we want the output to be?
--    foo -> foo2

-- v = Causal (Conflicted blah)
-- k = Reference

--bindMaybeCausal ::forall a. (Hashable a, Ord a) => Causal (Conflicted a) -> (a -> Maybe (Causal (Conflicted a))) -> Causal (Conflicted a)
--bindMaybeCausal cca f = case Causal.head cca of
--  Conflicted.One a -> case f a of
--    Just cca' -> Causal.sequence cca cca'
--    Nothing -> cca
--  Conflicted.Many as ->
--    Causal.sequence cca $ case nonEmpty . join $ (toList . f <$> toList as) of
--      -- Would be nice if there were a good NonEmpty.Set, but Data.NonEmpty.Set from `non-empty` doesn't seem to be it.
--      Nothing -> error "impossible, `as` was Many"
--      Just z -> sconcat z
--
--chain :: forall v k. Ord k => (v -> Maybe k) -> Map k (Causal (Conflicted v)) -> Map k (Causal (Conflicted v)) -> Map k (Causal (Conflicted v))
--chain toK m1 m2 =
--    let
--      chain' :: forall v k . (v -> Maybe k) -> (k -> Maybe (Causal (Conflicted v))) -> (k -> Maybe (Causal (Conflicted v))) -> (k -> Maybe (Causal (Conflicted v)))
--      chain' toK m1 m2 k = case m1 k of
--        Just ccv1 -> Just $ bindMaybeCausal ccv1 (\k -> m2 k >>= toK)
--        Nothing -> m2 k
--    in
--      Map.fromList
--        [ (k, v) | k <- Map.keys m1 ++ Map.keys m2
--                 , Just v <- [chain' toK (`Map.lookup` m1) (`Map.lookup` m2) k] ]

