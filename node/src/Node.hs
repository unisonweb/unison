{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Applicative
import Data.Text (Text)
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
import qualified Unison.Node.Implementation as C
import qualified Unison.Node.Store as Store
import qualified Unison.NodeServer as S
import qualified Unison.Note as N
import qualified Unison.Reference as R
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.Type as Type

infixr 7 -->
(-->) :: Type -> Type -> Type
(-->) = Type.arrow

makeNode :: Store IO -> IO (Node IO R.Reference Type Term)
makeNode store =
  let
    builtins =
     [ let r = R.Builtin "()"
       in (r, Nothing, unitT, prefix "()")

     , let r = R.Builtin "Color.rgba"
       in (r, strict r 4, num --> num --> num --> num --> colorT, prefix "rgba")

     , let r = R.Builtin "Fixity.Prefix"
       in (r, Nothing, fixityT, prefix "Prefix")
     , let r = R.Builtin "Fixity.InfixL"
       in (r, Nothing, fixityT, prefix "InfixL")
     , let r = R.Builtin "Fixity.InfixR"
       in (r, Nothing, fixityT, prefix "InfixR")

     , let r = R.Builtin "Metadata.metadata"
       in (r, strict r 2, vec symbolT --> str --> metadataT, prefix "metadata")

     , let r = R.Builtin "Number.plus"
       in (r, Just (numeric2 (Term.ref r) (+)), numOpTyp, opl 4 "+")
     , let r = R.Builtin "Number.minus"
       in (r, Just (numeric2 (Term.ref r) (-)), numOpTyp, opl 4 "-")
     , let r = R.Builtin "Number.times"
       in (r, Just (numeric2 (Term.ref r) (*)), numOpTyp, opl 5 "*")
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

     , let r = R.Builtin "View.cell"
       in (r, strict r 2, Type.forall' ["a"] $ view (v' "a") --> v' "a" --> cellT, prefix "cell")
     , let r = R.Builtin "View.color"
       in (r, Nothing, colorT --> view cellT, prefix "color")
     , let r = R.Builtin "View.declare"
       in (r, strict r 1, Type.forall' ["a"] $ str --> v' "a" --> cellT, prefix "declare")
     , let r = R.Builtin "View.embed"
       in (r, Nothing, view cellT, prefix "embed")
     , let r = R.Builtin "View.fit-width"
       in (r, strict r 1, Type.forall' ["a"] $ distanceT --> view (v' "a"), prefix "fit-width")
     , let r = R.Builtin "View.function1"
       in ( r
          , Nothing
          , Type.forall' ["a","b"] $ (cellT --> cellT) --> view (v' "a" --> v' "b")
          , prefix "function1" )
     , let r = R.Builtin "View.hide"
       in (r, Nothing, Type.forall' ["a"] $ view (v' "a"), prefix "hide")
     , let r = R.Builtin "View.horizontal"
       in (r, Nothing, view (vec cellT), prefix "horizontal")
     , let r = R.Builtin "View.reactive"
       in (r, Nothing, Type.forall' ["a"] $ view (v' "a"), prefix "reactive")
     , let r = R.Builtin "View.source"
       in (r, Nothing, Type.forall' ["a"] $ view (v' "a"), prefix "source")
     , let r = R.Builtin "View.spacer"
       in (r, strict r 1, distanceT --> num --> view unitT, prefix "spacer")
     , let r = R.Builtin "View.swatch"
       in (r, Nothing, view colorT, prefix "swatch")
     , let r = R.Builtin "View.text"
       in (r, strict r 1, styleT --> view str, prefix "text")
     , let r = R.Builtin "View.textbox"
       in (r, strict r 2, alignmentT `arr` (distanceT `arr` (styleT `arr` view str)), prefix "textbox")
     , let r = R.Builtin "View.vertical"
       in (r, Nothing, view (vec cellT), prefix "vertical")
     , let r = R.Builtin "View.view"
       in (r, strict r 1, Type.forall' ["a"] $ view (v' "a") --> v' "a" --> v' "a", prefix "view")
     ]

    eval :: Eval (N.Noted IO)
    eval = I.eval (M.fromList [ (k,v) | (k,Just v,_,_) <- builtins ])

    readTerm :: Hash -> N.Noted IO Term
    readTerm h = Store.readTerm store h

    whnf :: Term -> N.Noted IO Term
    whnf = Eval.whnf eval readTerm

    node :: Node IO R.Reference Type Term
    node = C.node eval store

    v' = Type.v'
    fixityT = Type.ref (R.Builtin "Fixity")
    symbolT = Type.ref (R.Builtin "Symbol")
    alignmentT = Type.ref (R.Builtin "Alignment")
    metadataT = Type.ref (R.Builtin "Metadata")
    arr = Type.arrow
    cellT = Type.ref (R.Builtin "Cell")
    colorT = Type.ref (R.Builtin "Color")
    distanceT = Type.lit Type.Distance
    num = Type.lit Type.Number
    numOpTyp = num --> num --> num
    styleT = Type.ref (R.Builtin "Text.Style")
    str = Type.lit Type.Text
    strOpTyp = str `arr` (str `arr` str)
    unitT = Type.ref (R.Builtin "Unit")
    vec a = Type.app (Type.lit Type.Vector) a
    view a = Type.app (Type.ref (R.Builtin "View")) a
    strict r n = Just (I.Primop n f)
      where f args = reapply <$> traverse whnf (take n args)
                     where reapply args' = Term.ref r `apps` args' `apps` drop n args
            apps f args = foldl Term.app f args

    numeric2 :: Term -> (Double -> Double -> Double) -> I.Primop (N.Noted IO)
    numeric2 sym f = I.Primop 2 $ \xs -> case xs of
      [x,y] -> g <$> whnf x <*> whnf y
        where g (Term.Number' x) (Term.Number' y) = Term.lit (Term.Number (f x y))
              g x y = sym `Term.app` x `Term.app` y
      _ -> error "unpossible"

    string2 :: Term -> (Text -> Text -> Text) -> I.Primop (N.Noted IO)
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

opl :: Int -> Text -> Metadata k
opl n s = Metadata Metadata.Term
                   (Metadata.Names [Symbol.symbol s Symbol.InfixL n ])
                   []
                   Nothing

prefix :: Text -> Metadata k
prefix s = prefixes [s]

prefixes :: [Text] -> Metadata k
prefixes s = Metadata Metadata.Term
                    (Metadata.Names (map (\s -> Symbol.symbol s Symbol.Prefix 9) s))
                    []
                    Nothing

main :: IO ()
main = do
  store <- Store.store "store"
  node <- makeNode store
  S.server 8080 node
