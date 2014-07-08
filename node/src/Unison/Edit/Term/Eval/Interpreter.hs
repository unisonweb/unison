-- | Very simple and inefficient interpreter of Unison terms
module Unison.Edit.Term.Eval.Interpreter where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Unison.Edit.Term.Eval
import Unison.Syntax.Term (Term)
import qualified Unison.Syntax.Term as E
import Unison.Syntax.Hash (Hash)
import qualified Unison.Syntax.Hash as H

-- | A Haskell function accepting 'arity' arguments
data Primop f =
  Primop { arity :: Int, call :: [Term] -> f Term }

-- | Produce an evaluator from a environment of 'Primop' values
eval :: (Applicative f, Monad f) => Map Hash (Primop f) -> Eval f
eval env = Eval step whnf
  where
    reduce e@(E.App (E.Lam _ _) _) args =
      return $ Just (foldl E.App (E.betaReduce e) args)
    reduce (E.App (E.Ref h) x) args = case M.lookup h env of
      Nothing -> return Nothing
      Just op | length (x:args) >= arity op ->
        call op (take (arity op) (x:args)) >>= \e ->
          return . Just $ foldl E.App e (drop (arity op) (x:args))
      Just _ | otherwise -> return Nothing
    reduce (E.App f x) args = reduce f (x:args)
    reduce _ _ = return $ Nothing

    step resolveRef e = case e of
      E.App f x -> E.link resolveRef f >>= \f ->
        liftM (fromMaybe (E.App f x)) (reduce f [x])
      _ -> return e

    whnf resolveRef e = case e of
      E.App f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (return e) (whnf resolveRef) e'
      _ -> return e
