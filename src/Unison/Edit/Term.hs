module Unison.Edit.Term where

import qualified Unison.Edit.Term.Path as P
import Unison.Edit.Term.Action as A
import Unison.Edit.Term.Eval as Eval
import qualified Unison.Syntax.Term as E

apply :: Monad f => Eval f -> P.Path -> Action E.Term -> E.Term -> f (Maybe E.Term)
apply eval loc f e = go f where
  go Abstract = return . E.lam1M $ \x -> P.set loc x e
  go Step = case P.at loc e of
    Nothing -> return Nothing
    Just sub -> do
      sub <- step eval sub
      return $ P.set loc sub e
  go _ = undefined
