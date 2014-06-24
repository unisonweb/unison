module Unison.Edit.Term.Path where

import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Var as V
import Control.Applicative

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  deriving (Eq,Ord,Show)

newtype Path = Path [E] deriving (Eq,Ord,Show)

at :: Path -> E.Term -> Maybe E.Term
at (Path [])    e = Just e
at (Path (h:t)) e = go h e where
  go _ (E.Var _) = Nothing
  go _ (E.Lit _) = Nothing
  go Fn (E.App f _) = at (Path t) f
  go Arg (E.App _ x) = at (Path t) x
  go _ (E.Ann e' _) = at (Path (h:t)) e'
  go Body (E.Lam _ body) = at (Path t) body
  go _ _ = Nothing

set :: Path -> E.Term -> E.Term -> Maybe E.Term
set path focus ctx = impl path ctx where
  maxVar = E.maxV focus
  impl (Path []) _ = Just focus
  impl (Path (h:t)) ctx = go h ctx where
    go _ (E.Var _) = Nothing
    go _ (E.Lit _) = Nothing
    go Fn (E.App f arg) = (\f' -> E.App f' arg) <$> impl (Path t) f
    go Arg (E.App f arg) = E.App f <$> impl (Path t) arg
    go _ (E.Ann x _) = impl (Path (h:t)) x
    go Body (E.Lam n body) = E.Lam <$> pure (V.nest n maxVar) <*> impl (Path t) body
    go _ _ = Nothing

modify :: Path -> (E.Term -> E.Term) -> E.Term -> Maybe E.Term
modify loc f e = do
  x <- at loc e
  set loc (f x) e
