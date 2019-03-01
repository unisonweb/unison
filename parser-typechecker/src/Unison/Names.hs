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
import qualified Data.Set         as Set
import           Data.String      (fromString)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Unison.ConstructorType (ConstructorType)
import           Unison.Reference (pattern Builtin, Reference)
import qualified Unison.Name      as Name
import           Unison.Name      (Name)
import qualified Unison.Referent  as Referent
import           Unison.Referent  (Referent)
import           Unison.Term      (AnnotatedTerm)
import qualified Unison.Term      as Term
import           Unison.Type      (AnnotatedType)
import qualified Unison.Type      as Type
import           Unison.Var       (Var)

unqualified :: Name -> Name
unqualified = Name.unsafeFromText . unqualified' . Name.toText

unqualified' :: Text -> Text
unqualified' = last . Text.splitOn "."

-- Names is like Branch.Namespace, but:
-- - there are no conflicts
-- - lookup is one-directional
data Names = Names
  { termNames    :: Map Name Referent
  , typeNames    :: Map Name Reference
  }

data NameTarget = TermName | TypeName deriving (Eq, Ord, Show)

subtractTerms :: Var v => [v] -> Names -> Names
subtractTerms vs n = let
  taken = Set.fromList (Name.unsafeFromVar <$> vs)
  in n { termNames = Map.withoutKeys (termNames n) taken }

renderNameTarget :: NameTarget -> String
renderNameTarget = \case
  TermName -> "term"
  TypeName -> "type"
  -- PatternName -> "pattern"

instance Show Names where
  -- really barebones, just to see what names are present
  show (Names es ts) =
    "terms: " ++ show (es) ++ "\n" ++
    "types: " ++ show (ts)

lookupType :: Names -> Name -> Maybe Reference
lookupType ns n = Map.lookup n (typeNames ns)

fromBuiltins :: [Reference] -> Names
fromBuiltins rs =
  mempty { termNames = Map.fromList
          [ (Name.unsafeFromText t, Referent.Ref r) | r@(Builtin t) <- rs ] }

fromTerms :: [(Name, Referent)] -> Names
fromTerms ts = mempty { termNames = Map.fromList ts }

fromTypesV :: Var v => [(v, Reference)] -> Names
fromTypesV env =
  Names mempty . Map.fromList $ fmap (first $ Name.unsafeFromVar) env

fromTypes :: [(Name, Reference)] -> Names
fromTypes env = Names mempty $ Map.fromList env

filterTypes :: (Name -> Bool) -> Names -> Names
filterTypes f (Names {..}) = Names termNames m2
  where
  m2 = Map.fromList $ [(k,v) | (k,v) <- Map.toList typeNames, f k]

patternNameds :: Names -> String -> Maybe (Reference, Int)
patternNameds ns s = patternNamed ns (fromString s)

patternNamed :: Names -> Name -> Maybe (Reference, Int)
patternNamed ns n = Map.lookup n (termNames ns) >>= \case
  Referent.Con r cid -> Just (r, cid)
  _ -> Nothing

bindType :: Var v => Names -> AnnotatedType v a -> AnnotatedType v a
bindType ns t = Type.bindBuiltins typeNames' t
  where
  typeNames' = [ (Name.toVar v, r) | (v, r) <- Map.toList $ typeNames ns ]

bindTerm :: forall v a . Var v
         => (Reference -> ConstructorType)
         -> Names
         -> AnnotatedTerm v a
         -> AnnotatedTerm v a
bindTerm ctorType ns e = Term.bindBuiltins termBuiltins typeBuiltins e
 where
  termBuiltins =
    [ (Name.toVar v, Term.fromReferent ctorType () e) | (v, e) <- Map.toList (termNames ns) ]
  typeBuiltins :: [(v, Reference)]
  typeBuiltins = [ (Name.toVar v, t) | (v, t) <- Map.toList (typeNames ns) ]

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
    (Name.unsafeFromVar v, Name.unsafeFromVar v2) | (v,v2) <- shortToLongName0 ]
  terms' = foldl' go termNames shortToLongName
  types' = foldl' go typeNames shortToLongName
  in Names terms' types'

instance Semigroup Names where (<>) = mappend

instance Monoid Names where
  mempty = Names mempty mempty
  Names e1 t1 `mappend` Names e2 t2 =
    Names (e1 `unionL` e2) (t1 `unionL` t2)
    where
      unionL :: forall k v. Ord k => Map k v -> Map k v -> Map k v
      unionL = Map.unionWith const
