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
apply eval loc f e = go f where
  go Abstract = N.liftMaybe invalid $ E.lam1M (\x -> P.set loc x e) -- todo: use a lens
  -- so check validity of location once up front
  go Step = case P.at loc e of
    Nothing -> N.failure invalid
    Just sub -> do
      sub <- N.lift $ step eval sub
      N.liftMaybe invalid $ P.set loc sub e
  go _ = undefined
  invalid = "invalid path " ++ show loc ++ " in:\n" ++ show e
