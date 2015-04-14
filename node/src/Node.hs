{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Traversable
import Unison.Eval (Eval)
import Unison.Hash (Hash)
import Unison.Metadata (Metadata(..))
import Unison.Node (Node)
import Unison.Node.Store (Store)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.Map as M
import qualified Data.Vector as Vector
import qualified Unison.Eval as Eval
import qualified Unison.Eval.Interpreter as I
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.Common as C
import qualified Unison.Node.Server as S
import qualified Unison.Node.Store as Store
import qualified Unison.Node.Store.File as F
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Var as Var

numeric2 :: Term -> (Double -> Double -> Double) -> I.Primop (N.Noted IO)
numeric2 sym f = I.Primop 2 $ \xs -> case xs of
  [x,y] -> do
    xr <- whnf x
    yr <- whnf y
    pure $ case (xr, yr) of
      (Term.Lit (Term.Number x), Term.Lit (Term.Number y)) -> Term.Lit (Term.Number (f x y))
      (x,y) -> sym `Term.App` x `Term.App` y
  _ -> error "unpossible"

string2 :: Term -> (Text -> Text -> Text) -> I.Primop (N.Noted IO)
string2 sym f = I.Primop 2 $ \xs -> case xs of
  [x,y] -> do
    xr <- whnf x
    yr <- whnf y
    pure $ case (xr, yr) of
      (Term.Lit (Term.String x), Term.Lit (Term.String y)) -> Term.Lit (Term.String (f x y))
      (x,y) -> sym `Term.App` x `Term.App` y
  _ -> error "unpossible"

builtins :: [(R.Reference, Maybe (I.Primop (N.Noted IO)), Type, Metadata R.Reference)]
builtins =
  [ let r = R.Builtin "()"
    in (r, Nothing, unitT, prefix "()")

  , let r = R.Builtin "Color.rgba"
    in (r, strict r 4, num `arr` (num `arr` (num `arr` (num `arr` colorT))), prefix "rgba")

  , let r = R.Builtin "Fixity.Prefix"
    in (r, Nothing, fixityT, prefix "Prefix")
  , let r = R.Builtin "Fixity.InfixL"
    in (r, Nothing, fixityT, prefix "InfixL")
  , let r = R.Builtin "Fixity.InfixR"
    in (r, Nothing, fixityT, prefix "InfixR")

  , let r = R.Builtin "Metadata.metadata"
    in (r, strict r 2, vec symbolT `arr` (str `arr` metadataT), prefix "metadata")

  , let r = R.Builtin "Number.plus"
    in (r, Just (numeric2 (Term.Ref r) (+)), numOpTyp, opl 4 "+")
  , let r = R.Builtin "Number.minus"
    in (r, Just (numeric2 (Term.Ref r) (-)), numOpTyp, opl 4 "-")
  , let r = R.Builtin "Number.times"
    in (r, Just (numeric2 (Term.Ref r) (*)), numOpTyp, opl 5 "*")
  , let r = R.Builtin "Number.divide"
    in (r, Just (numeric2 (Term.Ref r) (/)), numOpTyp, opl 5 "/")

  , let r = R.Builtin "Symbol.Symbol"
    in (r, Nothing, str `arr` (fixityT `arr` (num `arr` symbolT)), prefix "Symbol")

  , let r = R.Builtin "Text.concatenate"
    in (r, Just (string2 (Term.Ref r) mappend), strOpTyp, prefixes ["concatenate", "Text"])
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
            Term.Vector init -> Term.Vector (Vector.snoc init last)
            init -> Term.Ref r `Term.App` last `Term.App` init
        op _ = fail "Vector.append unpossible"
    in (r, Just (I.Primop 2 op), Type.forall1 $ \a -> a `arr` (vec a `arr` vec a), prefix "append")
  , let r = R.Builtin "Vector.concatenate"
        op [a,b] = do
          ar <- whnf a
          br <- whnf b
          pure $ case (a,b) of
            (Term.Vector a, Term.Vector b) -> Term.Vector (a `mappend` b)
            (a,b) -> Term.Ref r `Term.App` a `Term.App` b
        op _ = fail "Vector.concatenate unpossible"
    in (r, Just (I.Primop 2 op), Type.forall1 $ \a -> vec a `arr` (vec a `arr` vec a), prefix "concatenate")
  , let r = R.Builtin "Vector.empty"
        op [] = pure $ Term.Vector mempty
        op _ = fail "Vector.empty unpossible"
    in (r, Just (I.Primop 0 op), Type.forall1 vec, prefix "empty")
  , let r = R.Builtin "Vector.map"
        op [f,vec] = do
          vecr <- whnf vec
          pure $ case vecr of
            Term.Vector vs -> Term.Vector (fmap (Term.App f) vs)
            _ -> Term.Ref r `Term.App` vecr
        op _ = fail "Vector.map unpossible"
    in (r, Just (I.Primop 2 op), Type.forall2 $ \a b -> (a `arr` b) `arr` (vec a `arr` vec b), prefix "map")
  , let r = R.Builtin "Vector.prepend"
        op [hd,tl] = do
          tlr <- whnf tl
          pure $ case tlr of
            Term.Vector tl -> Term.Vector (Vector.cons hd tl)
            tl -> Term.Ref r `Term.App` hd `Term.App` tl
        op _ = fail "Vector.prepend unpossible"
    in (r, Just (I.Primop 2 op), Type.forall1 $ \a -> a `arr` (vec a `arr` vec a), prefix "prepend")
  , let r = R.Builtin "Vector.single"
        op [hd] = pure $ Term.Vector (pure hd)
        op _ = fail "Vector.single unpossible"
    in (r, Just (I.Primop 1 op), Type.forall1 $ \a -> a `arr` vec a, prefix "single")

  , let r = R.Builtin "View.cell"
    in (r, strict r 2, Type.forall1 $ \a -> view a `arr` (a `arr` cellT), prefix "cell")
  , let r = R.Builtin "View.color"
    in (r, Nothing, colorT `arr` view cellT, prefix "color")
  , let r = R.Builtin "View.declare"
    in (r, strict r 1, Type.forall1 $ \a -> str `arr` (a `arr` cellT), prefix "declare")
  , let r = R.Builtin "View.embed"
    in (r, Nothing, view cellT, prefix "embed")
  , let r = R.Builtin "View.fit-width"
    in (r, strict r 1, Type.forall1 $ \a -> distanceT `arr` view a, prefix "fit-width")
  , let r = R.Builtin "View.function1"
    in ( r
       , Nothing
       , Type.forall2 $ \a b -> (cellT `arr` cellT) `arr` view (a `arr` b)
       , prefix "function1" )
  , let r = R.Builtin "View.hide"
    in (r, Nothing, Type.forall1 view, prefix "hide")
  , let r = R.Builtin "View.horizontal"
    in (r, Nothing, view (vec cellT), prefix "horizontal")
  , let r = R.Builtin "View.reactive"
    in (r, Nothing, Type.forall1 view, prefix "reactive")
  , let r = R.Builtin "View.source"
    in (r, Nothing, Type.forall1 view, prefix "source")
  , let r = R.Builtin "View.spacer"
    in (r, strict r 1, distanceT `arr` (num `arr` view unitT), prefix "spacer")
  , let r = R.Builtin "View.swatch"
    in (r, Nothing, view colorT, prefix "swatch")
  , let r = R.Builtin "View.text"
    in (r, strict r 1, styleT `arr` view str, prefix "text")
  , let r = R.Builtin "View.textbox"
    in (r, strict r 2, alignmentT `arr` (distanceT `arr` (styleT `arr` view str)), prefix "textbox")
  , let r = R.Builtin "View.vertical"
    in (r, Nothing, view (vec cellT), prefix "vertical")
  , let r = R.Builtin "View.view"
    in (r, strict r 1, Type.forall1 $ \a -> view a `arr` (a `arr` a), prefix "view")
  ]
  where
    fixityT = Type.Unit (Type.Ref (R.Builtin "Fixity"))
    symbolT = Type.Unit (Type.Ref (R.Builtin "Symbol"))
    alignmentT = Type.Unit (Type.Ref (R.Builtin "Alignment"))
    metadataT = Type.Unit (Type.Ref (R.Builtin "Metadata"))
    arr = Type.Arrow
    cellT = Type.Unit (Type.Ref (R.Builtin "Cell"))
    colorT = Type.Unit (Type.Ref (R.Builtin "Color"))
    distanceT = Type.Unit Type.Distance
    num = Type.Unit Type.Number
    numOpTyp = num `arr` (num `arr` num)
    styleT = Type.Unit (Type.Ref (R.Builtin "Text.Style"))
    st = strOpTyp
    str = Type.Unit Type.String
    strOpTyp = str `arr` (str `arr` str)
    unitT = Type.Unit (Type.Ref (R.Builtin "Unit"))
    vec a = Type.App (Type.Unit Type.Vector) a
    view a = Type.App (Type.Unit (Type.Ref (R.Builtin "View"))) a
    strict r n = Just (I.Primop n f)
      where f args = reapply <$> traverse whnf (take n args)
                     where reapply args' = Term.Ref r `apps` args' `apps` drop n args
            apps f args = foldl Term.App f args

opl n s = Metadata Metadata.Term
                   (Metadata.Names [Metadata.Symbol s Metadata.InfixL n ])
                   []
                   Nothing

prefix s = prefixes [s]

prefixes s = Metadata Metadata.Term
                    (Metadata.Names (map (\s -> Metadata.Symbol s Metadata.Prefix 9) s))
                    []
                    Nothing

builtinMetadatas :: Node IO R.Reference Type Term -> N.Noted IO ()
builtinMetadatas node = do
  _ <- Node.createTerm node (Term.Lam (Term.Var Var.bound1)) (prefix "identity")
  mapM_ (\(r,_,t,md) -> Node.updateMetadata node r md *> Store.annotateTerm store r t)
        builtins

store :: Store IO
store = F.store "store"

eval :: Eval (N.Noted IO)
eval = I.eval (M.fromList [ (k,v) | (k,Just v,_,_) <- builtins ])

readTerm :: Hash -> N.Noted IO Term
readTerm h = Store.readTerm store h

whnf :: Term -> N.Noted IO Term
whnf = Eval.whnf eval readTerm

node :: Node IO R.Reference Type Term
node = C.node eval store

main :: IO ()
main = do
  N.run (builtinMetadatas node)
  S.server 8080 node
