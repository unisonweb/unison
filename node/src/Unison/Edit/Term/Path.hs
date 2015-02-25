{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Edit.Term.Path where

import Control.Applicative
import Data.Text.Internal (Text)
import Data.Foldable
import Data.Aeson as A
import Data.Aeson.TH
import Data.Maybe (fromJust)
import Data.Vector ((!?), (//))
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Var as V

data E
  = Fn -- ^ Points at function in a function application
  | Arg -- ^ Points at the argument of a function application
  | Body -- ^ Points at the body of a lambda
  | Index !Int -- ^ Points at the index of a vector
  deriving (Eq,Ord,Show)

newtype Path = Path [E] deriving (Eq,Ord)

instance Show Path where
  show (Path es) = show es

elements :: Path -> [E]
elements (Path e) = e

prefixes :: Path -> [Path]
prefixes (Path p) = map Path (go p)
  where go :: [a] -> [[a]]
        go as = map reverse (scanl (flip (:)) [] as)

-- | Add an element onto the end of this 'Path'
extend :: E -> Path -> Path
extend e (Path p) = Path (p ++ [e])

at :: Path -> E.Term -> Maybe E.Term
at (Path [])    e = Just e
at (Path (h:t)) e = go h e where
  go _ (E.Var _) = Nothing
  go (Index i) (E.Vector xs) = xs !? i >>= at (Path t)
  go _ (E.Lit _) = Nothing
  go Fn (E.App f _) = at (Path t) f
  go Arg (E.App _ x) = at (Path t) x
  go _ (E.Ann e' _) = at (Path (h:t)) e'
  go Body (E.Lam body) = at (Path t) body
  go _ _ = Nothing

along :: Path -> E.Term -> [E.Term]
along (Path path) e = go path e
  where go [] e = [e]
        go (Fn:path) e@(E.App f _) = e : go path f
        go (Arg:path) e@(E.App _ arg) = e : go path arg
        go (Body:path) e@(E.Lam body) = e : go path body
        go (Index i:path) e@(E.Vector xs) = e : maybe [] (go path) (xs !? i)
        go _ _ = []

valid :: Path -> E.Term -> Bool
valid p e = maybe False (const True) (at p e)

-- | Like 'at', but returns a function which may be used to modify the focus
at' :: Path -> E.Term -> Maybe (E.Term, E.Term -> E.Term)
at' loc ctx = case at loc ctx of
  Nothing -> Nothing
  Just focus -> Just (focus, \focus -> fromJust (set loc focus ctx)) -- safe since `at` proves `loc` valid

-- | Returns the list of variables in scope at the given path
inScopeAt :: Path -> E.Term -> [V.Var]
inScopeAt p e =
  let vars = map f (along p e)
      f (E.Lam _) = Just ()
      f _ = Nothing
      n = length (drop 1 (reverse vars) >>= toList)
  in take n (iterate V.succ V.bound1)

set :: Path -> E.Term -> E.Term -> Maybe E.Term
set path focus ctx = impl path ctx where
  impl (Path []) _ = Just focus
  impl (Path (h:t)) ctx = go h ctx where
    go _ (E.Var _) = Nothing
    go (Index i) (E.Vector xs) = let replace xi = E.Vector (xs // [(i,xi)])
                                 in replace <$> xs !? i >>= impl (Path t)
    go _ (E.Lit _) = Nothing
    go Fn (E.App f arg) = E.App <$> impl (Path t) f <*> pure arg
    go Arg (E.App f arg) = E.App f <$> impl (Path t) arg
    go _ (E.Ann x _) = impl (Path (h:t)) x
    go Body (E.Lam body) = E.Lam <$> impl (Path t) body
    go _ _ = Nothing

-- | Like 'set', but accepts the new focus within the returned @Maybe@.
set' :: Path -> E.Term -> Maybe (E.Term -> E.Term)
set' loc ctx = snd <$> at' loc ctx

modify :: Path -> (E.Term -> E.Term) -> E.Term -> Maybe E.Term
modify loc f ctx = do
  x <- at loc ctx
  set loc (f x) ctx

freeAt :: Path -> V.Var
freeAt path =
  let used = length [ Body | Body <- elements path ]
      vars = iterate V.succ V.bound1
  in head (drop used vars)

-- | @introduceAt1 path (\var focus -> ...) ctx@ introduces a
-- new free variable at the given path and makes some edit to
-- the focus using this variable and the current focus.
introduceAt1 :: Path
             -> (forall e . E.Scoped e -> E.Scoped e -> E.Scoped e)
             -> E.Scoped a
             -> Maybe (E.Scoped (Maybe a))
introduceAt1 path f ctx = do
  focus <- E.Scoped <$> at path (E.unscope ctx)
  focus' <- pure (f (E.Scoped (E.Var (freeAt path))) focus)
  E.Scoped <$> set path (E.unscope focus') (E.unscope ctx)

-- | Like @introduceAt1@, but takes raw terms, and returns a lambda term
introduceAt1' :: Path
             -> (forall e . E.Scoped e -> E.Scoped e -> E.Scoped e)
             -> E.Term
             -> Maybe E.Term
introduceAt1' path f ctx = do
  ctx' <- E.scoped ctx
  body <- introduceAt1 path f ctx'
  pure (E.unscope (E.lam body))

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

instance A.FromJSON Path where
  parseJSON (Object o) = Path <$> o .: "contents"
  parseJSON j = fail $ "Path.parseJSON expected Object, got: " ++ show j

instance A.ToJSON Path where
  toJSON (Path es) = object [ "tag" .= ("Path" :: Text), "contents" .= es ]
