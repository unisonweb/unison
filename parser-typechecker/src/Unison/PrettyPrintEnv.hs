{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv where

import Data.List (foldl')
import Data.Map (Map)
import Data.Text (Text)
import Unison.Reference (Reference)
import qualified Data.Map as Map
import qualified Data.Text as Text

type Histogram = Map Text Word

-- Maps terms, types, constructors and constructor patterns to a histogram of names.
data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms
  terms :: Reference -> Histogram,
  -- names for constructors that appear as terms
  constructors :: Reference -> Int -> Histogram,
  -- names for constructors that appear as patterns
  patterns :: Reference -> Int -> Histogram,
  -- names for types
  types :: Reference -> Histogram }

-- The monoid sums corresponding histograms

instance Semigroup PrettyPrintEnv where (<>) = mappend

instance Monoid PrettyPrintEnv where
  mempty = PrettyPrintEnv (const mempty) (\_ _ -> mempty) (\_ _ -> mempty) (const mempty)
  mappend e1 e2 =
    PrettyPrintEnv
      (\r -> Map.unionWith (+) (terms e1 r) (terms e2 r))
      (\r i -> Map.unionWith (+) (constructors e1 r i) (constructors e2 r i))
      (\r i -> Map.unionWith (+) (patterns e1 r i) (patterns e2 r i))
      (\r -> Map.unionWith (+) (types e1 r) (types e2 r))

-- These functions pick out the most common name and fall back
-- to showing the `Reference` if no names are available

termName :: PrettyPrintEnv -> Reference -> Text
termName env r = pickName r (terms env r)

typeName :: PrettyPrintEnv -> Reference -> Text
typeName env r = pickName r (types env r)

constructorName :: PrettyPrintEnv -> Reference -> Int -> Text
constructorName env r cid = pickNameCid r cid (constructors env r cid)

patternName :: PrettyPrintEnv -> Reference -> Int -> Text
patternName env r cid = pickNameCid r cid (constructors env r cid)

pickName :: Reference -> Histogram -> Text
pickName r h = case argmax snd (Map.toList h) of
  Nothing -> Text.pack (show r)
  Just (name,_) -> name

pickNameCid :: Reference -> Int -> Histogram -> Text
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
