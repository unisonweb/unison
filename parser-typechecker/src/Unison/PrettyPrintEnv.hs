{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv where

import Data.List (foldl')
import Data.Map (Map)
import Unison.Reference (Reference)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Unison.Names (Name,Names)
import qualified Unison.Names as Names
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

type Histogram = Map Name Word

-- Maps terms, types, constructors and constructor patterns to a histogram of names.
data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms, constructors, and requests
  terms :: Referent -> Histogram,
  -- names for constructors that appear as patterns
  patterns :: Reference -> Int -> Histogram,
  -- names for types
  types :: Reference -> Histogram }

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

fromNames :: Names -> PrettyPrintEnv
fromNames ns =
  let
    terms = Map.fromList
      [ (r, n) | (n, r) <- Map.toList (Names.termNames ns) ]
    patterns = Map.fromList
      [ ((r, i), n) | (n, (r, i)) <- Map.toList (Names.patternNames ns) ]
    types = Map.fromList [ (r, n) | (n, r) <- Map.toList (Names.typeNames ns) ]
    hist :: Ord k => Map k Name -> k -> Histogram
    hist m k = maybe mempty (\n -> Map.fromList [(n, 1)]) $ Map.lookup k m
  in
    PrettyPrintEnv (hist terms)
                   (curry $ hist patterns)
                   (hist types)

-- The monoid sums corresponding histograms

instance Semigroup PrettyPrintEnv where (<>) = mappend

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const mempty) (\_ _ -> mempty) (const mempty)
  mappend e1 e2 =
    PrettyPrintEnv
      (\r -> Map.unionWith (+) (terms e1 r) (terms e2 r))
      (\r i -> Map.unionWith (+) (patterns e1 r i) (patterns e2 r i))
      (\r -> Map.unionWith (+) (types e1 r) (types e2 r))

adjust :: (Word -> Word) -> PrettyPrintEnv -> PrettyPrintEnv
adjust by e = PrettyPrintEnv
  (\r -> by <$> terms e r)
  (\r i -> by <$> patterns e r i)
  (\r -> by <$> types e r)

scale :: Word -> PrettyPrintEnv -> PrettyPrintEnv
scale by = adjust (by *)

incrementBy :: Word -> PrettyPrintEnv -> PrettyPrintEnv
incrementBy by = adjust (by +)

weightedSum :: [(Word,PrettyPrintEnv)] -> PrettyPrintEnv
weightedSum envs = mconcat (uncurry scale <$> envs)

fromTypeNames :: [(Reference,Name)] -> PrettyPrintEnv
fromTypeNames types = let
  m = Map.fromList types
  toH Nothing = mempty
  toH (Just t) = Map.fromList [(t, 1)]
  in mempty { types = \r -> toH $ Map.lookup r m }

fromTermNames :: [(Referent,Name)] -> PrettyPrintEnv
fromTermNames tms = let
  m = Map.fromList tms
  toH Nothing = mempty
  toH (Just t) = Map.fromList [(t, 1)]
  in mempty { terms = \r -> toH $ Map.lookup r m }

fromConstructorNames :: [((Reference,Int), Name)] -> [((Reference,Int), Name)] -> PrettyPrintEnv
fromConstructorNames ctors reqs = let
  cs = Map.fromList ctors
  rs = Map.fromList reqs
  toH Nothing = mempty
  toH (Just t) = Map.fromList [(t, 1)]
  in mempty { terms = \r -> case r of
                Referent.Con r i -> toH $ Map.lookup (r,i) cs
                Referent.Req r i -> toH $ Map.lookup (r,i) rs
                _ -> mempty
            , patterns = \r i -> toH $ Map.lookup (r,i) (cs `Map.union` rs) }

-- These functions pick out the most common name and fall back
-- to showing the `Reference` if no names are available

termName :: PrettyPrintEnv -> Referent -> Name
termName env r = pickNameReferent r (terms env r)

typeName :: PrettyPrintEnv -> Reference -> Name
typeName env r = pickName r (types env r)

constructorName :: PrettyPrintEnv -> Reference -> Int -> Name
constructorName env r cid = pickNameCid r cid (terms env (Referent.Con r cid))

requestName :: PrettyPrintEnv -> Reference -> Int -> Name
requestName env r cid = pickNameCid r cid (terms env (Referent.Req r cid))

patternName :: PrettyPrintEnv -> Reference -> Int -> Name
patternName env r cid = pickNameCid r cid (patterns env r cid)

pickName :: Reference -> Histogram -> Name
pickName r h = case argmax snd (Map.toList h) of
  Nothing -> Text.pack (show r)
  Just (name,_) -> name

pickNameReferent :: Referent -> Histogram -> Name
pickNameReferent r h = case argmax snd (Map.toList h) of
  Nothing -> case r of
    Referent.Ref r -> Text.pack (show r)
    Referent.Con r i -> Text.pack (show r <> "#" <> show i)
    Referent.Req r i -> Text.pack (show r <> "#" <> show i)
  Just (name,_) -> name

pickNameCid :: Reference -> Int -> Histogram -> Name
pickNameCid r cid h = case argmax snd (Map.toList h) of
  Nothing -> Text.pack (show r) <> "#" <> Text.pack (show cid)
  Just (name,_) -> name

-- this fn should really exist someplace else
argmax :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
argmax by as = fst <$> foldl' go Nothing as where
  go Nothing a = Just (a, by a)
  go cur@(Just (_,b)) a2 =
    let b2 = by a2
    in if b2 > b then Just (a2,b2) else cur
