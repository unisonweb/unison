{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnv where

import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Unison.Reference (Reference)
import qualified Data.Map as Map
import Unison.Name (Name)
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent

type Histogram = Map HashQualified Word

-- Maps terms, types, constructors and constructor patterns to a histogram of names.
data PrettyPrintEnv = PrettyPrintEnv {
  -- names for terms, constructors, and requests
  terms :: Referent -> Histogram,
  -- names for types
  types :: Reference -> Histogram }

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"



fromNames :: Names -> PrettyPrintEnv
fromNames ns =
  let terms =
        Map.fromList [ (r, HQ.fromName n) | (n, r) <- Map.toList (Names.termNames ns) ]
      types =
        Map.fromList [ (r, HQ.fromName n) | (n, r) <- Map.toList (Names.typeNames ns) ]
      hist :: Ord k => Map k HashQualified -> k -> Histogram
      hist m k = maybe mempty (\n -> Map.fromList [(n, 1)]) $ Map.lookup k m
  in  PrettyPrintEnv (hist terms) (hist types)

-- The monoid sums corresponding histograms

-- instance Semigroup PrettyPrintEnv where (<>) = mappend
--
-- instance Monoid PrettyPrintEnv where
--   mempty = PrettyPrintEnv (const mempty) (const mempty)
--   mappend e1 e2 =
--     PrettyPrintEnv
--       (\r -> Map.unionWith (+) (terms e1 r) (terms e2 r))
--       (\r -> Map.unionWith (+) (types e1 r) (types e2 r))

-- Left-biased union of environments
unionLeft :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
unionLeft e1 e2 = PrettyPrintEnv
  (\r -> prefer (terms e1 r) (terms e2 r))
  (\r -> prefer (types e1 r) (types e2 r))
  where prefer h1 h2 = if sum (Map.elems h1) > 0 then h1 else h2

assignTermName :: Referent -> HashQualified -> PrettyPrintEnv -> PrettyPrintEnv
assignTermName r name env = PrettyPrintEnv terms' (types env)
  where terms' r' = if r' == r then Map.singleton name 1 else (terms env r)

-- adjust :: (Word -> Word) -> PrettyPrintEnv -> PrettyPrintEnv
-- adjust by e = PrettyPrintEnv
--   (\r -> by <$> terms e r)
--   (\r -> by <$> types e r)
--
-- scale :: Word -> PrettyPrintEnv -> PrettyPrintEnv
-- scale by = adjust (by *)
--
-- incrementBy :: Word -> PrettyPrintEnv -> PrettyPrintEnv
-- incrementBy by = adjust (by +)

-- weightedSum :: [(Word,PrettyPrintEnv)] -> PrettyPrintEnv
-- weightedSum envs = mconcat (uncurry scale <$> envs)

-- fromTypeNames' :: [(Reference,HashQualified)] -> PrettyPrintEnv
-- fromTypeNames' types = let
--   m = Map.fromList types
--   toH Nothing = mempty
--   toH (Just t) = Map.fromList [(t, 1)]
--   in mempty { types = \r -> toH $ Map.lookup r m }

fromTypeNames :: [(Reference,Name)] -> PrettyPrintEnv
fromTypeNames = error "todo"

fromTypeNames' :: [(Reference,HashQualified)] -> PrettyPrintEnv
fromTypeNames' = error "todo"

-- fromTermNames :: [(Referent,HashQualified)] -> PrettyPrintEnv
-- fromTermNames tms = let
--   m = Map.fromList tms
--   toH Nothing = mempty
--   toH (Just n) = Map.fromList [(Name.toHashQualified n, 1)]
--   in mempty { terms = \r -> toH $ Map.lookup r m }

fromTermNames :: [(Referent,Name)] -> PrettyPrintEnv
fromTermNames = error "todo"

fromTermNames' :: [(Referent,HashQualified)] -> PrettyPrintEnv
fromTermNames' = error "todo"

-- fromConstructorNames :: [((Reference,Int), Name)] -> [((Reference,Int), Name)] -> PrettyPrintEnv
-- fromConstructorNames ctors reqs = let
--   cs = Map.fromList ctors
--   rs = Map.fromList reqs
--   toH Nothing = mempty
--   toH (Just n) = Map.fromList [(Name.toHashQualified n, 1)]
--   in mempty { terms = \r -> case r of
--                 Referent.Con r i -> toH $ Map.lookup (r,i) cs
--                 Referent.Req r i -> toH $ Map.lookup (r,i) rs
--                 _ -> mempty }

-- These functions pick out the most common name and fall back
-- to showing the `Reference` if no names are available

termName :: PrettyPrintEnv -> Referent -> HashQualified
termName env r = fromMaybe (HQ.fromReferent r) $ pickName (terms env r)

typeName :: PrettyPrintEnv -> Reference -> HashQualified
typeName env r =
  let r' = Referent.Ref r
  in fromMaybe (HQ.fromReferent r') (pickName (types env r))

-- constructorName :: PrettyPrintEnv -> Reference -> Int -> HashQualified
-- constructorName env r cid = termName env (Referent.Con r cid)

-- requestName :: PrettyPrintEnv -> Reference -> Int -> Name
-- requestName env r cid = termName env (Referent.Req r cid)

patternName :: PrettyPrintEnv -> Reference -> Int -> HashQualified
patternName env r cid =
  case fst <$> argmax snd (Map.toList (terms env $ Referent.Con r cid)) of
    Just name -> name
    Nothing ->
      case fst <$> argmax snd (Map.toList (terms env $ Referent.Req r cid)) of
        Just name -> name
        Nothing -> HQ.fromReferent (Referent.Con r cid)
          -- arbitrarily pick Con because it doesn't matter

-- patternName :: PrettyPrintEnv -> Reference -> Int -> Name
-- patternName env r cid = pickNameCid r cid histo
--  where
--   histo = Map.unionWith (+)
--                         (terms env (Referent.Con r cid))
--                         (terms env (Referent.Req r cid))

pickName :: Histogram -> Maybe HashQualified
pickName h = fst <$> argmax snd (Map.toList h)

-- pickName :: Reference -> Histogram -> HashQualified
-- pickName r h = case argmax snd (Map.toList h) of
--   Nothing -> HQ.fromReferenceOnly r
--   Just (name,_) -> name

pickNameReferent :: Referent -> Histogram -> HashQualified
pickNameReferent r h = case argmax snd (Map.toList h) of
  Nothing -> HQ.fromReferent r
  Just (name,_) -> name
--
-- pickNameCid :: Reference -> Int -> Histogram -> HashQualified
-- pickNameCid r cid h = case argmax snd (Map.toList h) of
--   Nothing -> Text.pack (show r) <> "#" <> Text.pack (show cid)
--   Just (name,_) -> name

-- this fn should really exist someplace else
argmax :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
argmax by as = fst <$> foldl' go Nothing as where
  go Nothing a = Just (a, by a)
  go cur@(Just (_,b)) a2 =
    let b2 = by a2
    in if b2 > b then Just (a2,b2) else cur
