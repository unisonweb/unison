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
import qualified Data.Text as Text
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
    reduce resolveRef (E.App' f x) args = do
      x <- whnf resolveRef x
      reduce resolveRef f (x:args)
    reduce resolveRef (E.Let1' binding body) xs = do
      binding <- whnf resolveRef binding
      reduce resolveRef (ABT.bind body binding) xs
    reduce resolveRef (E.Ann' e _) args = reduce resolveRef e args
    reduce resolveRef f args = do
      f <- whnf resolveRef f
      case f of
        E.If' -> case take 3 args of
          [cond,t,f] -> do
            cond <- whnf resolveRef cond
            case cond of
              E.Builtin' c | Text.head c == 'F' -> pure . Just $ foldl E.app f (drop 3 args)
                           | otherwise -> pure . Just $ foldl E.app t (drop 3 args)
              _ -> pure Nothing
          _ -> pure Nothing
        E.Ref' h -> case M.lookup h env of
          Nothing -> pure Nothing
          Just op | length args >= arity op ->
            call op (take (arity op) args) >>= \e ->
              pure . Just $ foldl E.app e (drop (arity op) args)
          Just _ | otherwise -> pure Nothing
        E.Lam' f -> case args of
          [] -> pure Nothing
          (arg1:args) ->
            let r = ABT.bind f arg1
            in pure $ Just (foldl E.app r args)
        _ -> pure Nothing

    step resolveRef e = case e of
      E.Ref' h -> case M.lookup h env of
        Just op | arity op == 0 -> call op []
        _ -> pure e
      E.App' f x -> do
        f <- E.link resolveRef f
        e' <- reduce resolveRef f [x]
        maybe (pure e) pure e'
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
      E.Ann' e _ -> whnf resolveRef e
      E.Apps' E.If' (cond:t:f:tl) -> do
        cond <- whnf resolveRef cond
        case cond of
          E.Builtin' b | Text.head b == 'F' -> whnf resolveRef f >>= \f -> (`E.apps` tl) <$> whnf resolveRef f
                       | otherwise -> whnf resolveRef t >>= \t -> (`E.apps` tl) <$> whnf resolveRef t
          _ -> pure e
      E.Apps' f xs -> do
        f' <- E.link resolveRef f
        f <- whnf resolveRef f'
        xs <- traverse (whnf resolveRef) xs
        e' <- reduce resolveRef f xs
        maybe (pure $ f `E.apps` xs) (whnf resolveRef) e'
      E.Let1' binding body -> do
        binding <- whnf resolveRef binding
        whnf resolveRef (ABT.bind body binding)
      E.LetRecNamed' bs body -> whnf resolveRef (ABT.substs substs body) where
        expandBinding v (E.LamNamed' name body) = E.lam name (expandBinding v body)
        expandBinding v body = ABT.substs substs' body
          where substs' = [ (v', ABT.subst v (E.letRec bs (E.var v)) b) | (v',b) <- bs ]
        substs = [ (v, expandBinding v b) | (v,b) <- bs ]
      _ -> pure e
