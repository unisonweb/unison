-- | Very simple and inefficient interpreter of Unison terms
module Unison.Edit.Term.Eval.Interpreter where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Unison.Edit.Term.Eval
import Unison.Syntax.Term (Term)
import qualified Unison.Syntax.Term as E
import Unison.Syntax.Hash (Hash)
import qualified Unison.Syntax.Reference as R

-- | A Haskell function accepting 'arity' arguments
data Primop f =
  Primop { arity :: Int, call :: [Term] -> f Term }

watch :: Show a => String -> a -> a
watch msg a = trace (msg ++ ": " ++ show a) a

-- | Produce an evaluator from a environment of 'Primop' values
eval :: (Applicative f, Monad f) => Map R.Reference (Primop f) -> Eval f
eval env = Eval whnf step
  where
    reduce (E.Lam _) [] = return Nothing
    reduce e@(E.Lam _) (arg1:args) =
      return $ let r = watch "reduced" $ E.betaReduce (E.App e arg1)
               in Just (foldl E.App r args)
    reduce (E.App (E.Ref h) x) args = case M.lookup h env of
      Nothing -> return Nothing
      Just op | length (x:args) >= arity op ->
        call op (take (arity op) (x:args)) >>= \e ->
          return . Just $ foldl E.App e (drop (arity op) (x:args))
      Just _ | otherwise -> return Nothing
    reduce (E.App f x) args = reduce f (x:args)
    reduce _ _ = return Nothing

    step resolveRef e = case e of
      E.App f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (return e) return e'
      _ -> return e

    whnf resolveRef e = case e of
      E.App f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (return e) (whnf resolveRef) e'
      _ -> return e
