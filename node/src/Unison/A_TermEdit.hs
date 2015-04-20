{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_TermEdit where

import Control.Applicative
import Control.Monad
import GHC.Generics
import Data.Aeson.TH
import Data.Bytes.Serial
import Unison.A_Eval (Eval)
import Unison.A_Hash (Hash)
import Unison.Note (Noted)
import qualified Data.Set as Set
import qualified Unison.A_Eval as Eval
import qualified Unison.A_Term as Term
import qualified Unison.A_Hash as Hash
import qualified Unison.ABT as ABT

-- f {42} x ==> f {(y -> y) 42} x
-- f {(y -> y) 42} x ==> {(y -> f y x) 42 }
-- f {42} x ==> f {let y = 42 in y} x
-- f {let y = 42 in y} x ==> {let y = 42 in f y x}
--
data Action
  = Abstract -- Turn target into function parameter
  | AbstractLet -- Turn target into let bound expression
  | AllowRec -- Turn a let into a let rec
  | EtaReduce -- Eta reduce the target
  | FloatOut -- Float the target binding out one level
  | Inline -- Delete a let binding by inlining its definition into usage sites
  | MergeLet -- Merge a let block into its parent let block
  | Noop -- Do nothing to the target
  | Rename ABT.V -- Rename the target var
  | Step -- Link + beta reduce the target
  | SwapDown -- Swap the target let binding with the subsequent binding
  | SwapUp -- Swap the target let binding with the previous binding
  | WHNF -- Simplify target to weak head normal form
  deriving Generic

-- | Interpret the given 'Action'
interpret :: (Applicative f, Monad f)
          => Eval (Noted f)
          -> (Hash -> Noted f Term.Term)
          -> Term.Path -> Action -> Term.Term -> Noted f (Maybe (Term.Path, Term.Term))
interpret eval link path action t = case action of
  Abstract -> pure $ abstract path t
  AbstractLet -> pure $ abstractLet path t
  AllowRec -> pure $ allowRec path t
  EtaReduce -> pure $ etaReduce path t
  MergeLet -> pure $ mergeLet path t

{- Example:
   f {42} x
   ==>
   f {(v -> v) 42}
-}
abstract :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
abstract path t = f <$> Term.focus path t where
  f (sub,replace) =
    let sub' = Term.lam (ABT.freshIn' sub "v") (ABT.var' "v")
               `Term.app`
               sub
    in (path,sub')

{- Example:
   f {42} x
   ==>
   f {let v = 42 in v} x
-}
abstractLet :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
abstractLet path t = f <$> Term.focus path t where
  f (sub,replace) =
    let sub' = Term.let' [(ABT.v' "v", sub)] (ABT.var' "v")
    in (path, sub')

{- Example:
   let x = 1 in x + x
   ==>
   {let rec x = 1 in x + x}
-}
allowRec :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
allowRec path t = do
  Term.LetNonrec' bs e <- Term.at path t
  t' <- Term.modify (const (Term.letRec bs e)) path t
  pure (path, t')

{- Example:
   { x -> f x }
   ==>
   { f }
-}
etaReduce :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
etaReduce path t = do
  Term.Lam' v (Term.App' f (ABT.Var' v2)) <- Term.at path t
  guard (v == v2 && not (Set.member v (ABT.freevars f))) -- make sure vars match and `f` doesn't mention `v`
  pure (path, f)

{- Example:
   let x = 1 in {let y = 2 in y*y}
   ==>
   {let
     x = 1
     y = 2
   in
     y*y}
-}
mergeLet :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
mergeLet path t = do
  parentPath <- Term.parent path
  (innerBindings,e,_,_) <- Term.at path t >>= Term.unLet
  (outerBindings,_,let',_) <- Term.at parentPath t >>= Term.unLet
  (,) parentPath <$> Term.modify
    (const $ let' (outerBindings ++ innerBindings) e)
    parentPath
    t

instance Serial Action
deriveJSON defaultOptions ''Action
