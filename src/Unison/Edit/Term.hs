module Unison.Edit.Term where

import Control.Applicative
import qualified Unison.Edit.Term.Path as P
import Unison.Edit.Term.Action as A
import Unison.Edit.Term.Eval as Eval
import qualified Unison.Note as N
import Unison.Note (Noted)
import qualified Unison.Syntax.Term as E

apply :: (Applicative f, Monad f)
      => Eval f -> P.Path -> Action E.Term -> E.Term -> Noted f E.Term
apply eval loc f ctx = go f where
  go Abstract = N.liftMaybe invalid $ E.lam1 <$> P.set' loc ctx
  go Eta = case P.at loc ctx of
    Nothing -> N.failure invalid
    Just sub -> N.liftMaybe invalid $ P.set loc (E.etaReduce sub) ctx
  go Beta = case P.at loc ctx of
    Nothing -> N.failure invalid
    Just sub -> do
      sub <- N.lift $ step eval sub
      N.liftMaybe invalid $ P.set loc sub ctx
  go _ = undefined
  invalid = "invalid path " ++ show loc ++ " in:\n" ++ show ctx
