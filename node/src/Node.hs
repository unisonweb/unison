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

builtins :: [(R.Reference, I.Primop (N.Noted IO), Type, Metadata R.Reference)]
builtins =
  [ let r = R.Builtin "Number.plus"
    in (r, numeric2 (Term.Ref r) (+), numOpTyp, opl 4 "+")
  , let r = R.Builtin "Number.minus"
    in (r, numeric2 (Term.Ref r) (-), numOpTyp, opl 4 "-")
  , let r = R.Builtin "Number.times"
    in (r, numeric2 (Term.Ref r) (*), numOpTyp, opl 5 "*")
  , let r = R.Builtin "Number.divide"
    in (r, numeric2 (Term.Ref r) (/), numOpTyp, opl 5 "/")
  , let r = R.Builtin "Text.append"
    in (r, string2 (Term.Ref r) mappend, strOpTyp, prefix "append")
  -- , let r = R.Builtin "View.cell"
  --       t = error "todo.view.cell"
  --   in (r, nf r 2, T.forall1 $ \a -> )
  ]
  where
    str = Type.Unit Type.String
    num = Type.Unit Type.Number
    arr = Type.Arrow
    numOpTyp = num `arr` (num `arr` num)
    strOpTyp = str `arr` (str `arr` str)
    st = strOpTyp
    nf r n = I.Primop n $ pure . foldl' Term.App (Term.Ref r)

opl n s = Metadata Metadata.Term
                   (Metadata.Names [Metadata.Symbol s Metadata.InfixL n ])
                   []
                   Nothing
prefix s = Metadata Metadata.Term
                    (Metadata.Names [Metadata.Symbol s Metadata.Prefix 9])
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
eval = I.eval (M.fromList $ map (\(k,v,_,_) -> (k,v)) builtins)

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
