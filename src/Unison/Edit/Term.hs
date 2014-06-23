module Unison.Edit.Term where

import Unison.Edit.Term.Action as A
import Unison.Edit.Term.Eval as Eval
import qualified Unison.Syntax.Term as E

apply :: Eval f -> Action E.Term -> P.Path -> E.Term -> Maybe E.Term
apply eval loc e = go f where
  go Abstract = abstract loc e
  go Step = step expandLit loc e
  go _ = undefined

abstract :: P.Path -> E.Term -> Maybe E.Term
abstract loc e =
  let v = V.decr V.bound0 -- unused
  in do
    arg <- P.at loc e
    f <- E.Lam . E.abstract v <$> P.set (E.Var v) loc e
    pure $ E.App f arg

{-
step :: (E.Literal -> Either Primop E.Term)
     -> P.Path
     -> E.Term
     -> Maybe E.Term
step expandLit loc e = do
  (e', wrap) <- E.stripAnn <$> P.at loc e
  pure . wrap $ case e' of
    E.Lit l -> case expandLit l of
      Right e'' -> e''
      Left _ -> E.Lit l
    E.App (E.Lit f) x -> case expandLit f of
      Right f' -> E.betaReduce (E.App f' x)
      Left (Primop n f') -> let args = E.arguments e' in
        if length args <= n then E.applyN (f' (take n args)) (drop n args)
        else e'
    _ -> E.betaReduce e'
-}
