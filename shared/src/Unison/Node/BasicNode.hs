{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unison.Node.BasicNode where

import Data.Text (Text)
import Unison.Metadata (Metadata(..))
import Unison.Node (Node)
import Unison.Node.Store (Store)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as M
import qualified Data.Vector as Vector
import qualified Unison.Eval as Eval
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.Store as Store
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import qualified Unison.View as View

infixr 7 -->
(-->) :: Ord v => Type v -> Type v -> Type v
(-->) = Type.arrow

type Term' = Term (Symbol DFO)
type DFO = View.DFO
type V = Symbol DFO

make :: (Term V -> R.Reference)
     -> Store IO V
     -> IO (Node IO V R.Reference (Type V) (Term V))
make hash store =
  let
    builtins =
     [ let r = R.Builtin "()"
       in (r, Nothing, unitT, prefix "()")

     , let r = R.Builtin "Color.rgba"
       in (r, strict r 4, num --> num --> num --> num --> colorT, prefix "rgba")

     , let r = R.Builtin "Number.plus"
       in (r, Just (numeric2 (Term.ref r) (+)), numOpTyp, assoc 4 "+")
     , let r = R.Builtin "Number.minus"
       in (r, Just (numeric2 (Term.ref r) (-)), numOpTyp, opl 4 "-")
     , let r = R.Builtin "Number.times"
       in (r, Just (numeric2 (Term.ref r) (*)), numOpTyp, assoc 5 "*")
     , let r = R.Builtin "Number.divide"
       in (r, Just (numeric2 (Term.ref r) (/)), numOpTyp, opl 5 "/")

     , let r = R.Builtin "Symbol.Symbol"
       in (r, Nothing, str --> fixityT --> num --> symbolT, prefix "Symbol")

     , let r = R.Builtin "Text.concatenate"
       in (r, Just (string2 (Term.ref r) mappend), strOpTyp, prefixes ["concatenate", "Text"])
     , let r = R.Builtin "Text.left"
       in (r, Nothing, alignmentT, prefixes ["left", "Text"])
     , let r = R.Builtin "Text.right"
       in (r, Nothing, alignmentT, prefixes ["right", "Text"])
     , let r = R.Builtin "Text.center"
       in (r, Nothing, alignmentT, prefixes ["center", "Text"])
     , let r = R.Builtin "Text.justify"
       in (r, Nothing, alignmentT, prefixes ["justify", "Text"])

     , let r = R.Builtin "Vector.append"
           op [last,init] = do
             initr <- whnf init
             pure $ case initr of
               Term.Vector' init -> Term.vector' (Vector.snoc init last)
               init -> Term.ref r `Term.app` last `Term.app` init
           op _ = fail "Vector.append unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a") --> vec (v' "a"), prefix "append")
     , let r = R.Builtin "Vector.concatenate"
           op [a,b] = do
             ar <- whnf a
             br <- whnf b
             pure $ case (ar,br) of
               (Term.Vector' a, Term.Vector' b) -> Term.vector' (a `mappend` b)
               (a,b) -> Term.ref r `Term.app` a `Term.app` b
           op _ = fail "Vector.concatenate unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ vec (v' "a") --> vec (v' "a") --> vec (v' "a"), prefix "concatenate")
     , let r = R.Builtin "Vector.empty"
           op [] = pure $ Term.vector mempty
           op _ = fail "Vector.empty unpossible"
       in (r, Just (I.Primop 0 op), Type.forall' ["a"] (vec (v' "a")), prefix "empty")
     , let r = R.Builtin "Vector.map"
           op [f,vec] = do
             vecr <- whnf vec
             pure $ case vecr of
               Term.Vector' vs -> Term.vector' (fmap (Term.app f) vs)
               _ -> Term.ref r `Term.app` vecr
           op _ = fail "Vector.map unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a","b"] $ (v' "a" --> v' "b") --> vec (v' "a") --> vec (v' "b"), prefix "map")
     , let r = R.Builtin "Vector.prepend"
           op [hd,tl] = do
             tlr <- whnf tl
             pure $ case tlr of
               Term.Vector' tl -> Term.vector' (Vector.cons hd tl)
               tl -> Term.ref r `Term.app` hd `Term.app` tl
           op _ = fail "Vector.prepend unpossible"
       in (r, Just (I.Primop 2 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a") --> vec (v' "a"), prefix "prepend")
     , let r = R.Builtin "Vector.single"
           op [hd] = pure $ Term.vector (pure hd)
           op _ = fail "Vector.single unpossible"
       in (r, Just (I.Primop 1 op), Type.forall' ["a"] $ v' "a" --> vec (v' "a"), prefix "single")
     ]

    eval = I.eval (M.fromList [ (k,v) | (k,Just v,_,_) <- builtins ])
    readTerm h = Store.readTerm store h
    whnf = Eval.whnf eval readTerm
    node = Node.node eval hash store

    v' = Type.v'
    fixityT = Type.ref (R.Builtin "Fixity")
    symbolT = Type.ref (R.Builtin "Symbol")
    alignmentT = Type.ref (R.Builtin "Alignment")
    arr = Type.arrow
    colorT = Type.ref (R.Builtin "Color")
    num = Type.lit Type.Number
    numOpTyp = num --> num --> num
    str = Type.lit Type.Text
    strOpTyp = str `arr` (str `arr` str)
    unitT = Type.ref (R.Builtin "Unit")
    vec a = Type.app (Type.lit Type.Vector) a
    strict r n = Just (I.Primop n f)
      where f args = reapply <$> traverse whnf (take n args)
                     where reapply args' = Term.ref r `apps` args' `apps` drop n args
            apps f args = foldl Term.app f args

    numeric2 :: Term V -> (Double -> Double -> Double) -> I.Primop (N.Noted IO) V
    numeric2 sym f = I.Primop 2 $ \xs -> case xs of
      [x,y] -> g <$> whnf x <*> whnf y
        where g (Term.Number' x) (Term.Number' y) = Term.lit (Term.Number (f x y))
              g x y = sym `Term.app` x `Term.app` y
      _ -> error "unpossible"

    string2 :: Term V -> (Text -> Text -> Text) -> I.Primop (N.Noted IO) V
    string2 sym f = I.Primop 2 $ \xs -> case xs of
      [x,y] -> g <$> whnf x <*> whnf y
        where g (Term.Text' x) (Term.Text' y) = Term.lit (Term.Text (f x y))
              g x y = sym `Term.app` x `Term.app` y
      _ -> error "unpossible"

  in N.run $ do
    _ <- Node.createTerm node (Term.lam' ["a"] (Term.var' "a")) (prefix "identity")
    mapM_ (\(r,_,t,md) -> Node.updateMetadata node r md *> Store.annotateTerm store r t)
          builtins
    pure node

opl :: Int -> Text -> Metadata V h
opl p s =
  let
    sym :: Symbol DFO
    sym = Var.named s
    s' = Symbol.annotate (View.binary View.AssociateL (View.Precedence p)) sym
  in
    Metadata Metadata.Term (Metadata.Names [s']) Nothing

assoc :: Int -> Text -> Metadata V h
assoc p s =
  let
    sym :: Symbol DFO
    sym = Var.named s
    s' = Symbol.annotate (View.binary View.Associative (View.Precedence p)) sym
  in
    Metadata Metadata.Term (Metadata.Names [s']) Nothing

prefix :: Text -> Metadata V h
prefix s = prefixes [s]

prefixes :: [Text] -> Metadata V h
prefixes s = Metadata Metadata.Term
                      (Metadata.Names (map Var.named s))
                      Nothing
