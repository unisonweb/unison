module Unison.Typechecker.MinimizeCycles where

import qualified Unison.Term as Term
import Unison.Term (Term)
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Data.Set as Set

minimize :: Var v => Term v -> Maybe (Term v)
minimize (Term.LetRecNamed' bs e) = done $ tsort bs Set.empty [] where
  tsort [] _ acc = acc
  tsort ((v,b):bst) seen acc = case Set.member v seen of
    True -> tsort bst seen acc
    False -> tsort bst2 (Set.insert v seen) ((v,b):acc) where
      bst2 = [ (vo,bo) | vo <- Set.toList (ABT.freeVars b), Just bo <- [lookup vo bs] ]
         ++ bs
  -- have things topologically sorted, now just need to group into components
  -- each component gets nested in a series of let/let rec, with the twist that
  -- single element components may be turned into a let if they don't refer to selves
  -- if just one component, return Nothing
  done bst2 = undefined
minimize _ = Nothing
