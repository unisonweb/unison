{-# LANGUAGE TemplateHaskell #-}

module Unison.Edit.Term.Path where

import Control.Applicative
import Data.Aeson.TH
import Data.Maybe (fromJust)
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Var as V

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  deriving (Eq,Ord,Show)

newtype Path = Path { elements :: [E] } deriving (Eq,Ord,Show)

-- | Add an element onto the end of this 'Path'
extend :: E -> Path -> Path
extend e (Path p) = Path (p ++ [e])

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

-- | If the given @Path@ points to a valid subterm, we replace
-- that subterm @e@ with @v e@ and sequence the @Applicative@ effects
-- visit :: Path -> Traversal' E.Term E.Term
visit :: Applicative f => Path -> (E.Term -> f E.Term) -> E.Term -> f E.Term
visit (Path []) v e = v e
visit (Path (h:t)) v e = go h e where
  go Fn (E.App f arg) = E.App <$> visit (Path t) v f <*> pure arg
  go Arg (E.App f arg) = E.App <$> pure f <*> visit (Path t) v arg
  go _ (E.Ann e' typ) = E.Ann <$> visit (Path (h:t)) v e' <*> pure typ
  go Body (E.Lam n body) = fn <$> visit (Path t) v body
    where fn body = E.lam1 $ \x -> E.betaReduce (E.Lam n body `E.App` x)
  go _ e = pure e

-- | Like 'at', but returns a function which may be used to modify the focus
at' :: Path -> E.Term -> Maybe (E.Term, E.Term -> E.Term)
at' loc ctx = case at loc ctx of
  Nothing -> Nothing
  Just focus -> Just (focus, \focus -> fromJust (set loc focus ctx)) -- safe since `at` proves `loc` valid

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

-- | Like 'set', but accepts the new focus within the returned @Maybe@.
set' :: Path -> E.Term -> Maybe (E.Term -> E.Term)
set' loc ctx = snd <$> at' loc ctx

modify :: Path -> (E.Term -> E.Term) -> E.Term -> Maybe E.Term
modify loc f ctx = do
  x <- at loc ctx
  set loc (f x) ctx

-- | Drop from the right of this 'Path' until reaching the given element
trimToR :: E -> Path -> Path
trimToR e (Path p) = Path . reverse . dropWhile (/= e) . reverse $ p

-- | Drop the rightmost element of this 'Path', if it exists
drop1R :: Path -> Path
drop1R (Path p) = Path . reverse . drop 1 . reverse $ p

-- | Assuming Axelsson-Claessen naming, drop from the right of this
-- path until reaching the shortest path which still binds that name
trimToV :: Maybe (V.Var) -> Path -> Path
trimToV Nothing p = p
trimToV (Just minv) p | minv == V.bound1 = trimToR Body p
                      | otherwise        = trimToV (Just $ V.decr minv) (drop1R (trimToR Body p))


deriveJSON defaultOptions ''E
deriveJSON defaultOptions ''Path
