{-# LANGUAGE PatternSynonyms #-}
-- | Very simple and inefficient interpreter of Unison terms
module Unison.Eval.Interpreter where

import Control.Applicative
import Data.Map (Map)
import Debug.Trace
import Unison.Eval
import Unison.Term (Term)
import qualified Data.Map as M
import qualified Unison.Reference as R
import qualified Unison.Term as E

-- | A Haskell function accepting 'arity' arguments
data Primop f =
  Primop { arity :: Int, call :: [Term] -> f Term }

watch :: Show a => String -> a -> a
watch msg a = trace (msg ++ ": " ++ show a) a

-- | Produce an evaluator from a environment of 'Primop' values
eval :: (Applicative f, Monad f) => Map R.Reference (Primop f) -> Eval f
eval env = Eval whnf step
  where
    reduce x args | trace ("reduce:" ++ show (x:args)) False = undefined
    reduce (E.Lam' _ _) [] = return Nothing
    reduce e@(E.Lam' _ _) (arg1:args) =
      return $ let r = watch "reduced" $ E.betaReduce (E.app e arg1)
               in Just (foldl E.app r args)
    reduce (E.Ref' h) args = case M.lookup h env of
      Nothing -> return Nothing
      Just op | length args >= arity op ->
        call op (take (arity op) args) >>= \e ->
          return . Just $ foldl E.app e (drop (arity op) args)
      Just _ | otherwise -> return Nothing
    reduce (E.App' f x) args = reduce f (x:args)
    reduce _ _ = return Nothing

    step resolveRef e = case e of
      E.App' f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (return e) return e'
      E.Ref' h -> do
        f <- E.link resolveRef (E.ref h)
        e <- reduce f []
        maybe (return f) return e
      _ -> return e

    whnf resolveRef e = case e of
      E.App' f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (return e) (whnf resolveRef) e'
      _ -> return e
