module Unison.Codebase.Branch where

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Unison.Codebase.Causal (Causal)
--import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Conflicted (Conflicted)
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

chain2' :: (v -> v -> v) -> (v -> Maybe k) -> (k -> Maybe v) -> (k -> Maybe v) -> (k -> Maybe v)
chain2' seq toK m1 m2 k = case m1 k of
  -- if something upgraded by m1 is further upgraded by m2, then take m2's, else take m1's
  Just v1 -> (seq v1 <$> (m2 =<< toK v1)) <|> Just v1
  -- if not upgraded by m1, then take m2's
  Nothing -> m2 k

--chain :: Ord k => (v -> Maybe k) -> Map k v -> Map k v -> Map k v
--chain toK m1 m2 =
--  Map.fromList
--    [ (k,v) | k <- Map.keys m1 ++ Map.keys m2
--            , Just v <- [chain2' Causal.sequence toK (`Map.lookup` m1) (`Map.lookup` m2) k] ]