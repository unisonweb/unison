{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Very simple and inefficient interpreter of Unison terms
module Unison.Eval.Interpreter where

import Data.Map (Map)
import Debug.Trace
import Unison.Eval
import Unison.Term (Term)
import Unison.Var (Var)
import qualified Data.Map as M
import qualified Unison.ABT as ABT
import qualified Unison.Reference as R
import qualified Unison.Term as E

-- | A Haskell function accepting 'arity' arguments
data Primop f v =
  Primop { arity :: Int, call :: [Term v] -> f (Term v) }

watch :: Show a => String -> a -> a
watch msg a = trace (msg ++ ": " ++ show a) a

-- | Produce an evaluator from a environment of 'Primop' values
eval :: forall f v . (Monad f, Var v) => Map R.Reference (Primop f v) -> Eval f v
eval env = Eval whnf step
  where
    -- reduce x args | trace ("reduce:" ++ show (x:args)) False = undefined
    reduce :: Term v -> [Term v] -> f (Maybe (Term v))
    reduce (E.Lam' _) [] = pure Nothing
    reduce (E.Lam' f) (arg1:args) =
      let r = ABT.bind f arg1
      in pure $ Just (foldl E.app r args)
    reduce (E.Ref' h) args = case M.lookup h env of
      Nothing -> pure Nothing
      Just op | length args >= arity op ->
        call op (take (arity op) args) >>= \e ->
          pure . Just $ foldl E.app e (drop (arity op) args)
      Just _ | otherwise -> pure Nothing
    reduce (E.App' f x) args = reduce f (x:args)
    reduce (E.Let1' binding body) xs = reduce (ABT.bind body binding) xs
    reduce _ _ = pure Nothing

    step resolveRef e = case e of
      E.Ref' h -> case M.lookup h env of
        Just op | arity op == 0 -> call op []
        _ -> pure e
      E.App' (E.LetRecNamed' bs body) x -> step resolveRef (E.letRec bs (body `E.app` x))
      E.App' f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (pure e) pure e'
      E.Ref' h -> do
        f <- E.link resolveRef (E.ref h)
        e <- reduce f []
        maybe (pure f) pure e
      E.Let1' binding body -> step resolveRef (ABT.bind body binding)
      E.LetRecNamed' bs body -> step resolveRef (ABT.substs substs body) where
        expandBinding v (E.LamNamed' name body) = E.lam name (expandBinding v body)
        expandBinding v body = ABT.substs substs' body
          where substs' = [ (v', ABT.subst v (E.letRec bs (E.var v)) b) | (v',b) <- bs ]
        substs = [ (v, expandBinding v b) | (v,b) <- bs ]
      _ -> pure e

    whnf resolveRef e = case e of
      E.Ref' h -> case M.lookup h env of
        Just op | arity op == 0 -> call op []
        _ -> pure e
      E.App' (E.Ann' f _) x -> whnf resolveRef (f `E.app` x)
      E.App' (E.LetRecNamed' bs body) x -> whnf resolveRef (E.letRec bs (body `E.app` x))
      E.App' (E.Let1Named' v b body) x -> whnf resolveRef (E.let1 [(v,b)] (body `E.app` x))
      E.App' f x -> do
        f' <- E.link resolveRef f
        e' <- reduce f' [x]
        maybe (pure e) (whnf resolveRef) e'
      E.Let1' binding body -> whnf resolveRef (ABT.bind body binding)
      E.LetRecNamed' bs body -> whnf resolveRef (ABT.substs substs body) where
        expandBinding v (E.LamNamed' name body) = E.lam name (expandBinding v body)
        expandBinding v body = ABT.substs substs' body
          where substs' = [ (v', ABT.subst v (E.letRec bs (E.var v)) b) | (v',b) <- bs ]
        substs = [ (v, expandBinding v b) | (v,b) <- bs ]
      _ -> pure e
