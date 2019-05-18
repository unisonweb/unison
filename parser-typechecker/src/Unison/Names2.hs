{-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Names2 where

-- import           Data.Bifunctor   (first)
import Data.Foldable (toList)
-- import           Data.List        (foldl')
import           Data.Map         (Map)
import           Data.Set (Set)
-- import qualified Data.Map         as Map
import qualified Data.Set         as Set
-- import           Data.String      (fromString)
-- import           Data.Text        (Text)
-- import qualified Data.Text        as Text
-- import           Unison.ConstructorType (ConstructorType)
import           Unison.Reference (pattern Builtin, Reference)
import           Unison.HashQualified   (HashQualified)
import qualified Unison.HashQualified as HQ
-- import qualified Unison.Name      as Name
import           Unison.Name      (Name)
--import qualified Unison.Referent  as Referent
import           Unison.Referent        (Referent(Con))
import           Unison.Util.Relation   ( Relation )
import qualified Unison.Util.Relation as R

-- import           Unison.Term      (AnnotatedTerm)
-- import qualified Unison.Term      as Term
-- import           Unison.Type      (AnnotatedType)
-- import qualified Unison.Type      as Type
-- import           Unison.Var       (Var)

-- This will support the APIs of both PrettyPrintEnv and the old Names.
-- For pretty-printing, we need to look up names for References; they may have
-- some hash-qualification, depending on the context.
-- For parsing (both .u files and command-line args)
data Names' n = Names
  { terms :: Relation n Referent
  , types :: Relation n Reference
  } deriving (Show)

type Names = Names' HashQualified
type Names0 = Names' Name

typeName :: Ord n => Names' n -> Reference -> n
typeName names r =
  case toList $ R.lookupRan r (types names) of
    hq : _ -> hq
    _ -> error
      ("Names construction should have included something for " <> show r)

termName :: Ord n => Names' n -> Referent -> n
termName names r =
  case toList $ R.lookupRan r (terms names) of
    hq : _ -> hq
    _ -> error
      ("Names construction should have included something for " <> show r)

patternName :: Ord n => Names' n -> Reference -> Int -> n
patternName names r cid = termName names (Con r cid)

termConflicts :: Ord n => Names' n -> n -> Set Referent
termConflicts = flip R.lookupDom . terms

typeConflicts :: Ord n => Names' n -> n -> Set Reference
typeConflicts = flip R.lookupDom . types

namesForReferent :: Names' n -> Referent -> Set n
namesForReferent names r = R.lookupRan r (terms names)

namesForReference :: Names' n -> Reference -> Set n
namesForReference names r = R.lookupRan r (types names)

termAliases :: Ord n => Names' n -> n -> Referent -> Set n
termAliases names n r = Set.delete n $ namesForReferent names r

typeAliases :: Ord n => Names' n -> n -> Reference -> Set n
typeAliases names n r = Set.delete n $ namesForReference names r

hqTypeAliases :: Names0 -> Name -> Reference -> Set HashQualified
hqTypeAliases names@(Names _ types) n r =
  Set.map (hq r) (typeAliases names n r)
  where hq r n = if Set.size (R.lookupDom n types) > 1
                 then HQ.fromNamedReference n r
                 else HQ.fromName n
hqTermAliases :: Names0 -> Name -> Referent -> Set HashQualified
hqTermAliases names@(Names terms _) n r =
  Set.map (hq r) (termAliases names n r)
  where hq r n = if Set.size (R.lookupDom n terms) > 1
                 then HQ.fromNamedReferent n r
                 else HQ.fromName n

-- subtractTerms :: Var v => [v] -> Names -> Names
-- subtractTerms vs n = let
--   taken = Set.fromList (Name.unsafeFromVar <$> vs)
--   in n { termNames = Map.withoutKeys (termNames n) taken }

-- renderNameTarget :: NameTarget -> String
-- renderNameTarget = \case
--   TermName -> "term"
--   TypeName -> "type"
  -- PatternName -> "pattern"

-- instance Show Names where
--   -- really barebones, just to see what names are present
--   show (Names es ts) =
--     "terms: " ++ show (es) ++ "\n" ++
--     "types: " ++ show (ts)
--
-- lookupType :: Names -> Name -> Maybe Reference
-- lookupType ns n = Map.lookup n (typeNames ns)
--
-- fromBuiltins :: [Reference] -> Names
-- fromBuiltins rs =
--   mempty { termNames = Map.fromList
--           [ (Name.unsafeFromText t, Referent.Ref r) | r@(Builtin t) <- rs ] }

fromTerms :: [(Name, Referent)] -> Names0
fromTerms ts = Names (R.fromList ts) mempty

-- fromTypesV :: Var v => [(v, Reference)] -> Names
-- fromTypesV env =
--   Names mempty . Map.fromList $ fmap (first $ Name.unsafeFromVar) env

fromTypes :: [(Name, Reference)] -> Names0
fromTypes ts = Names mempty (R.fromList ts)

-- filterTypes :: (Name -> Bool) -> Names -> Names
-- filterTypes f (Names {..}) = Names termNames m2
--   where
--   m2 = Map.fromList $ [(k,v) | (k,v) <- Map.toList typeNames, f k]
--
-- patternNameds :: Names -> String -> Maybe (Reference, Int)
-- patternNameds ns s = patternNamed ns (fromString s)
--
-- patternNamed :: Names -> Name -> Maybe (Reference, Int)
-- patternNamed ns n = Map.lookup n (termNames ns) >>= \case
--   Referent.Con r cid -> Just (r, cid)
--   _ -> Nothing
--
-- bindType :: Var v => Names -> AnnotatedType v a -> AnnotatedType v a
-- bindType ns t = Type.bindBuiltins typeNames' t
--   where
--   typeNames' = [ (Name.toVar v, r) | (v, r) <- Map.toList $ typeNames ns ]
--
-- bindTerm :: forall v a . Var v
--          => (Reference -> ConstructorType)
--          -> Names
--          -> AnnotatedTerm v a
--          -> AnnotatedTerm v a
-- bindTerm ctorType ns e = Term.bindBuiltins termBuiltins typeBuiltins e
--  where
--   termBuiltins =
--     [ (Name.toVar v, Term.fromReferent ctorType () e) | (v, e) <- Map.toList (termNames ns) ]
--   typeBuiltins :: [(v, Reference)]
--   typeBuiltins = [ (Name.toVar v, t) | (v, t) <- Map.toList (typeNames ns) ]
--
-- -- Given a mapping from name to qualified name, update a `PEnv`,
-- -- so for instance if the input has [(Some, Optional.Some)],
-- -- and `Optional.Some` is a constructor in the input `PEnv`,
-- -- the alias `Some` will map to that same constructor
-- importing :: Var v => [(v,v)] -> Names -> Names
-- importing shortToLongName0 (Names {..}) = let
--   go :: Ord k => Map k v -> (k, k) -> Map k v
--   go m (shortname, qname) = case Map.lookup qname m of
--     Nothing -> m
--     Just v  -> Map.insert shortname v m
--   shortToLongName = [
--     (Name.unsafeFromVar v, Name.unsafeFromVar v2) | (v,v2) <- shortToLongName0 ]
--   terms' = foldl' go termNames shortToLongName
--   types' = foldl' go typeNames shortToLongName
--   in Names terms' types'
--
instance Ord n => Semigroup (Names' n) where (<>) = mappend

instance Ord n => Monoid (Names' n) where
  mempty = Names mempty mempty
  Names e1 t1 `mappend` Names e2 t2 =
    Names (e1 <> e2) (t1 <> t2)
