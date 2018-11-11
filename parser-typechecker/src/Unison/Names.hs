{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Names where

import           Data.Bifunctor   (first)
import           Data.List        (foldl')
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Word        (Word64)
import           Unison.Hashable  (Hashable)
import qualified Unison.Hashable  as H
import           Unison.Reference (pattern Builtin, Reference)
import           Unison.Term      (AnnotatedTerm, AnnotatedTerm2)
import qualified Unison.Term      as Term
import           Unison.Type      (AnnotatedType)
import qualified Unison.Type      as Type
import           Unison.Var       (Var)
import qualified Unison.Var       as Var

type Name = Text

unqualified :: Name -> Name
unqualified = last . Text.splitOn "."

data Names = Names
  { termNames    :: Map Name Referent
  , patternNames :: Map Name (Reference, Int)
  , typeNames    :: Map Name Reference
  }

-- | The referent of a name
data Referent = Ref Reference | Req Reference Int | Con Reference Int
  deriving (Show, Ord, Eq)

instance Hashable Referent where
  tokens (Ref r) = [H.Tag 0] ++ H.tokens r
  tokens (Req r i) = [H.Tag 1] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)
  tokens (Con r i) = [H.Tag 2] ++ H.tokens r ++ H.tokens (fromIntegral i :: Word64)

referentToTerm :: Ord v => a -> Referent -> AnnotatedTerm2 vt at ap v a
referentToTerm a = \case
  Ref r -> Term.ref a r
  Req r i -> Term.request a r i
  Con r i -> Term.constructor a r i

termToReferent :: AnnotatedTerm2 vt at ap v a -> Maybe Referent
termToReferent t = case t of
  Term.Ref' r           -> Just $ Ref r
  Term.Request' r i     -> Just $ Req r i
  Term.Constructor' r i -> Just $ Con r i
  _                     -> Nothing

referentToReference :: Referent -> Reference
referentToReference = \case
  Ref r -> r
  Req r _i -> r
  Con r _i -> r

instance Show Names where
  -- really barebones, just to see what names are present
  show (Names es ps ts) =
    "terms: " ++ show (es) ++ "\n" ++
    "patterns: " ++ show (ps) ++ "\n" ++
    "types: " ++ show (ts)

lookupTerm :: Ord v => a -> Names -> Name -> Maybe (AnnotatedTerm v a)
lookupTerm a ns n = referentToTerm a <$> Map.lookup n (termNames ns)

lookupType :: Names -> Name -> Maybe Reference
lookupType ns n = Map.lookup n (typeNames ns)

fromPatterns :: [(Name,(Reference,Int))] -> Names
fromPatterns vs = mempty { patternNames = Map.fromList vs }

fromBuiltins :: [Reference] -> Names
fromBuiltins rs =
  mempty { termNames = Map.fromList [ (name, Ref r) | r@(Builtin name) <- rs ] }

fromTerms :: [(Name, Referent)] -> Names
fromTerms ts = mempty { termNames = Map.fromList ts }

fromTypesV :: Var v => [(v, Reference)] -> Names
fromTypesV env =
  Names mempty mempty . Map.fromList $ fmap (first $ Var.name) env

fromTypes :: [(Name, Reference)] -> Names
fromTypes env = Names mempty mempty $ Map.fromList env

filterTypes :: (Name -> Bool) -> Names -> Names
filterTypes f (Names {..}) = Names termNames patternNames m2
  where
  m2 = Map.fromList $ [(k,v) | (k,v) <- Map.toList typeNames, f k]

patternNameds :: Names -> String -> Maybe (Reference, Int)
patternNameds ns s = patternNamed ns (Text.pack s)

patternNamed :: Names -> Name -> Maybe (Reference, Int)
patternNamed ns n = Map.lookup n (patternNames ns)

bindType :: Var v => Names -> AnnotatedType v a -> AnnotatedType v a
bindType ns t = Type.bindBuiltins typeNames' t
  where
  typeNames' = [ (Var.named v, r) | (v, r) <- Map.toList $ typeNames ns ]

bindTerm
  :: forall v a . Var v => Names -> AnnotatedTerm v a -> AnnotatedTerm v a
bindTerm ns e = Term.bindBuiltins termBuiltins typeBuiltins e
 where
  termBuiltins =
    [ (Var.named v, referentToTerm () e) | (v, e) <- Map.toList (termNames ns) ]
  typeBuiltins :: [(v, Reference)]
  typeBuiltins = [ (Var.named v, t) | (v, t) <- Map.toList (typeNames ns) ]

-- Given a mapping from name to qualified name, update a `PEnv`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `PEnv`,
-- the alias `Some` will map to that same constructor
importing :: Var v => [(v,v)] -> Names -> Names
importing shortToLongName0 (Names {..}) = let
  go :: Ord k => Map k v -> (k, k) -> Map k v
  go m (shortname, qname) = case Map.lookup qname m of
    Nothing -> m
    Just v  -> Map.insert shortname v m
  shortToLongName = [
    (Var.name v, Var.name v2) | (v,v2) <- shortToLongName0 ]
  terms' = foldl' go termNames shortToLongName
  types' = foldl' go typeNames shortToLongName
  patterns' = foldl' go patternNames shortToLongName
  in Names terms' patterns' types'

instance Semigroup Names where (<>) = mappend

instance Monoid Names where
  mempty = Names mempty mempty mempty
  Names e1 p1 t1 `mappend` Names e2 p2 t2 =
    Names (e1 `unionL` e2) (p1 `unionL` p2) (t1 `unionL` t2)
    where
      unionL :: forall k v. Ord k => Map k v -> Map k v -> Map k v
      unionL = Map.unionWith const
