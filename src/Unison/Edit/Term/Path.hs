module Unison.Edit.Term.Path where

import qualified Unison.Syntax.Term as E
import Control.Applicative

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda

newtype Path = Path [E]

at :: Path -> E.Term -> Maybe E.Term
at (Path [])    e = Just e
at (Path (h:t)) e = go h e where
  go _ (E.Var _) = Nothing
  go _ (E.Lit _) = Nothing
  go Fn (E.App f _) = at (Path t) f
  go Arg (E.App _ x) = at (Path t) x
  go _ (E.Ann e' _) = at (Path (h:t)) e'
  go Body (E.Lam body) = at (Path t) body
  go _ _ = Nothing

set :: E.Term -> Path -> E.Term -> Maybe E.Term
set e (Path []) _ = Just e
set e (Path (h:t)) ctx = go h ctx where
  go _ (E.Var _) = Nothing
  go _ (E.Lit _) = Nothing
  go Fn (E.App f arg) = (\f' -> E.App f' arg) <$> set e (Path t) f
  go Arg (E.App f arg) = E.App f <$> set e (Path t) arg
  go _ (E.Ann x _) = set e (Path (h:t)) x
  go Body (E.Lam body) = E.Lam <$> set e (Path t) body
  go _ _ = Nothing

modify :: (E.Term -> E.Term) -> Path -> E.Term -> Maybe E.Term
modify f loc e = do
  x <- at loc e
  set (f x) loc e
