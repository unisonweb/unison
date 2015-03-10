{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Text (Text)
import Unison.Edit.Term.Eval (Eval)
import Unison.Node (Node)
import qualified Unison.Node as Node
import Unison.Node.Metadata (Metadata(..))
import Unison.Node.Store (Store)
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import qualified Data.Map as M
import qualified Unison.Edit.Term.Eval as Eval
import qualified Unison.Edit.Term.Eval.Interpreter as I
import qualified Unison.Node.Common as C
import qualified Unison.Node.Metadata as Metadata
import qualified Unison.Node.Server as S
import qualified Unison.Node.Store as Store
import qualified Unison.Node.Store.File as F
import qualified Unison.Note as N
import qualified Unison.Syntax.Reference as R
import qualified Unison.Syntax.Term as Term
import qualified Unison.Syntax.Type as Type
import qualified Unison.Syntax.Var as Var

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
    in (r, Nothing, num `arr` (num `arr` (num `arr` (num `arr` colorT))), prefix "rgba")

  , let r = R.Builtin "Number.plus"
    in (r, Just (numeric2 (Term.Ref r) (+)), numOpTyp, opl 4 "+")
  , let r = R.Builtin "Number.minus"
    in (r, Just (numeric2 (Term.Ref r) (-)), numOpTyp, opl 4 "-")
  , let r = R.Builtin "Number.times"
    in (r, Just (numeric2 (Term.Ref r) (*)), numOpTyp, opl 5 "*")
  , let r = R.Builtin "Number.divide"
    in (r, Just (numeric2 (Term.Ref r) (/)), numOpTyp, opl 5 "/")

  , let r = R.Builtin "Text.append"
    in (r, Just (string2 (Term.Ref r) mappend), strOpTyp, prefixes ["append", "Text"])
  , let r = R.Builtin "Text.left"
    in (r, Nothing, alignmentT, prefixes ["left", "Text"])
  , let r = R.Builtin "Text.right"
    in (r, Nothing, alignmentT, prefixes ["right", "Text"])
  , let r = R.Builtin "Text.center"
    in (r, Nothing, alignmentT, prefixes ["center", "Text"])
  , let r = R.Builtin "Text.justify"
    in (r, Nothing, alignmentT, prefixes ["center", "Text"])

  , let r = R.Builtin "View.cell"
    in (r, Nothing, Type.forall1 $ \a -> view a `arr` (a `arr` cellT), prefix "cell")
  , let r = R.Builtin "View.color"
    in (r, Nothing, colorT `arr` view cellT, prefix "color")
  , let r = R.Builtin "View.embed"
    in (r, Nothing, view cellT, prefix "embed")
  , let r = R.Builtin "View.fit-width"
    in (r, Nothing, Type.forall1 $ \a -> distanceT `arr` view a, prefix "fit-width")
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
    in (r, Nothing, Type.forall1 $ \a -> view a `arr` view a, prefix "reactive")
  , let r = R.Builtin "View.source"
    in (r, Nothing, Type.forall1 $ \a -> view a, prefix "source")
  , let r = R.Builtin "View.spacer"
    in (r, Nothing, distanceT `arr` (num `arr` view unitT), prefix "spacer")
  , let r = R.Builtin "View.swatch"
    in (r, Nothing, view colorT, prefix "swatch")
  , let r = R.Builtin "View.text"
    in (r, Nothing, styleT `arr` view str, prefix "text")
  , let r = R.Builtin "View.textbox"
    in (r, Nothing, alignmentT `arr` (distanceT `arr` (styleT `arr` view str)), prefix "textbox")
  , let r = R.Builtin "View.vertical"
    in (r, Nothing, view (vec cellT), prefix "vertical")
  , let r = R.Builtin "View.view"
    in (r, Nothing, Type.forall1 $ \a -> view a `arr` (a `arr` a), prefix "view")
  ]
  where
    alignmentT = Type.Unit (Type.Ref (R.Builtin "Alignment"))
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
