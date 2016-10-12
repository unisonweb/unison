{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Very simple and inefficient interpreter of Unison terms
module Unison.Interpreter where

import Data.List
import Data.Map (Map)
import Debug.Trace
import Unison.Hash (Hash)
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
make :: forall f v . (Monad f, Var v) => Map R.Reference (Primop f v)
     -> (Hash -> f (Term v))
     -> Term v -> f (Term v)
make env = nf
  where
    -- reduce x args | trace ("reduce:" ++ show (x:args)) False = undefined
    reduce resolveRef (E.Ann' e _) args = reduce resolveRef e args
    reduce resolveRef (E.App' f x) args = do
      x <- nf resolveRef x
      reduce resolveRef f (x:args)
    reduce resolveRef (E.Let1' binding body) xs = do
      binding <- nf resolveRef binding
      reduce resolveRef (ABT.bind body binding) xs
    reduce resolveRef f args = do
      f <- nf resolveRef f
      case f of
        E.If' -> case take 3 args of
          [cond,t,f] -> do
            cond <- nf resolveRef cond
            case cond of
              E.Builtin' c | Text.head c == 'F' -> pure . Just $ foldl E.app f (drop 3 args)
                           | otherwise -> pure . Just $ foldl E.app t (drop 3 args)
              _ -> pure Nothing
          _ -> pure Nothing
        E.Ref' h -> case M.lookup h env of
          Nothing -> case h of
            R.Derived h -> do
              r <- resolveRef h
              r <- nf resolveRef r
              reduce resolveRef r args
            R.Builtin _ -> pure Nothing
          Just op | length args >= arity op ->
            call op (take (arity op) args) >>= \e ->
              pure . Just $ foldl E.app e (drop (arity op) args)
          Just _ | otherwise -> pure Nothing
        E.LamsNamed' vs body -> let n = length vs in case args of
          [] -> pure Nothing
          args | length args >= n -> pure $ Just (foldl' E.app (ABT.substs (vs `zip` args) body) (drop n args))
               | otherwise -> pure Nothing
        _ -> pure Nothing

    nf resolveRef e = case e of
      E.Ref' h -> case M.lookup h env of
        Just op | arity op == 0 -> call op []
                | otherwise -> pure e
        Nothing -> case h of
          R.Derived h -> do
            r <- resolveRef h
            nf resolveRef r
          R.Builtin _ -> pure e
      E.Ann' e _ -> nf resolveRef e
      E.App' (E.Ref' (R.Builtin "Vector.force")) (E.Vector' es) ->
        E.vector' <$> traverse (nf resolveRef) es
      E.Apps' E.If' (cond:t:f:tl) -> do
        cond <- nf resolveRef cond
        case cond of
          E.Builtin' b | Text.head b == 'F' -> nf resolveRef f >>= \f -> (`E.apps` tl) <$> nf resolveRef f
                       | otherwise -> nf resolveRef t >>= \t -> (`E.apps` tl) <$> nf resolveRef t
          _ -> pure e
      E.Apps' f xs -> case f of
        E.Ref' (R.Builtin "Pair") -> pure e
        f -> do
          xs <- traverse (nf resolveRef) xs
          f <- nf resolveRef f
          e' <- reduce resolveRef f xs
          maybe (pure $ f `E.apps` xs) (nf resolveRef) e'
      E.Let1' binding body -> do
        binding <- nf resolveRef binding
        nf resolveRef (ABT.bind body binding)
      E.LetRecNamed' bs body -> nf resolveRef (ABT.substs bs' body) where
        bs' = [ (v, expandBinding v b) | (v,b) <- bs ]
        expandBinding v (E.LamNamed' name body) = E.lam name (expandBinding v body)
        expandBinding v body = E.letRec bs body
      _ -> pure e
