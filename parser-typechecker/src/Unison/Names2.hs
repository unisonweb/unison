{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wwarn #-} -- todo: remove me later

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Names2 where

-- import           Data.Bifunctor   (first)
import Data.Foldable (toList)
import           Data.List        (foldl')
import           Data.Set (Set)
-- import qualified Data.Map         as Map
import qualified Data.Set         as Set
-- import           Data.String      (fromString)
import           Data.Text        (Text)
import qualified Data.Text        as Text
-- import           Unison.ConstructorType (ConstructorType)
import           Unison.Codebase.SearchResult   ( SearchResult )
import qualified Unison.Codebase.SearchResult  as SR
import           Unison.HashQualified'   (HashQualified)
import qualified Unison.HashQualified' as HQ
-- import qualified Unison.Name      as Name
import           Unison.Name      (Name)
import qualified Unison.Name      as Name
import qualified Unison.Referent  as Referent
import           Unison.Referent        (Referent(..))
import           Unison.Reference        (Reference)
import           Unison.Util.Relation   ( Relation )
import qualified Unison.Util.Relation as R
import Unison.Codebase.NameSegment (NameSegment)

-- This will support the APIs of both PrettyPrintEnv and the old Names.
-- For pretty-printing, we need to look up names for References; they may have
-- some hash-qualification, depending on the context.
-- For parsing (both .u files and command-line args)
data Names' n = Names
  { terms :: Relation n Referent
  , types :: Relation n Reference
  } deriving (Eq,Ord)

type Names = Names' HashQualified
type Names0 = Names' Name
type NamesSeg = Names' (HQ.HashQualified' NameSegment)

names0ToNames :: Names0 -> Names
names0ToNames names0 = Names terms' types' where
  terms' = R.map doTerm (terms names0)
  types' = R.map doType (types names0)
  length = numHashChars names0
  doTerm (n, r) =
    if Set.size (R.lookupDom n (terms names0)) > 1
    then (HQ.take length $ HQ.fromNamedReferent n r, r)
    else (HQ.NameOnly n, r)
  doType (n, r) =
    if Set.size (R.lookupDom n (types names0)) > 1
    then (HQ.take length $ HQ.fromNamedReference n r, r)
    else (HQ.NameOnly n, r)

hasTerm :: Referent -> Names' n -> Bool
hasTerm r = R.memberRan r . terms

hasType :: Reference -> Names' n -> Bool
hasType r = R.memberRan r . types

termReferents :: Names' n -> Set Referent
termReferents Names{..} = R.ran terms

termReferences, typeReferences, allReferences :: Names' n -> Set Reference
termReferences Names{..} = Set.map Referent.toReference $ R.ran terms
typeReferences Names{..} = R.ran types
allReferences n = termReferences n <> typeReferences n

restrictReferences :: Ord n => Set Reference -> Names' n -> Names' n
restrictReferences refs Names{..} = Names terms' types' where
  terms' = R.filterRan ((`Set.member` refs) . Referent.toReference) terms
  types' = R.filterRan (`Set.member` refs) types

-- | Guide to unionLeft*
-- Is it ok to create new aliases for parsing?
--    Sure.
-- Is it ok to create name conflicts for parsing?
--    It's okay but not great. The user will have to hash-qualify to disambiguate.
--
-- Is it ok to create new aliases for pretty-printing?
--    Not helpful, we need to choose a name to show.
--    We'll just have to choose one at random if there are aliases.
-- Is it ok to create name conflicts for pretty-printing?
--    Still okay but not great.  The pretty-printer will have to hash-qualify
--    to disambiguate.
--
-- Thus, for parsing:
--       unionLeftName is good if the name `n` on the left is the only `n` the
--           user will want to reference.  It allows the rhs to add aliases.
--       unionLeftRef allows new conflicts but no new aliases.  Lame?
--       (<>) is ok for parsing if we expect to add some conflicted names,
--           e.g. from history
--
-- For pretty-printing:
--       Probably don't want to add new aliases, unless we don't know which
--       `Names` is higher priority.  So if we do have a preferred `Names`,
--       don't use `unionLeftName` or (<>).
--       You don't want to create new conflicts either if you have a preferred
--       `Names`.  So in this case, don't use `unionLeftRef` either.
--       I guess that leaves `unionLeft`.
--
-- Not sure if the above is helpful or correct!

-- unionLeft two Names, excluding new name conflicts.
-- e.g. unionLeftName [foo -> #a, bar -> #a, cat -> #c]
--                    [foo -> #b, baz -> #c]
--                  = [foo -> #a, bar -> #a, baz -> #c, cat -> #c)]
-- Btw, it's ok to create name conflicts for parsing environments, if you don't
-- mind disambiguating.
unionLeftName :: Ord n => Names' n -> Names' n -> Names' n
unionLeftName = unionLeft' $ const . R.memberDom

-- unionLeft two Names, excluding new aliases.
-- e.g. unionLeftRef [foo -> #a, bar -> #a, cat -> #c]
--                   [foo -> #b, baz -> #c]
--                 = [foo -> #a, bar -> #a, foo -> #b, cat -> #c]
unionLeftRef :: Ord n => Names' n -> Names' n -> Names' n
unionLeftRef = unionLeft' $ const R.memberRan

-- unionLeft two Names, but don't create new aliases or new name conflicts.
-- e.g. unionLeft [foo -> #a, bar -> #a, cat -> #c]
--                [foo -> #b, baz -> #c]
--              = [foo -> #a, bar -> #a, cat -> #c]
unionLeft :: Ord n => Names' n -> Names' n -> Names' n
unionLeft = unionLeft' go
  where go n r acc = R.memberDom n acc || R.memberRan r acc

unionLeft'
  :: Ord n
  => (forall a b . (Ord a, Ord b) => a -> b -> Relation a b -> Bool)
  -> Names' n
  -> Names' n
  -> Names' n
unionLeft' p a b = Names terms' types'
 where
  terms' = foldl' go (terms a) (R.toList $ terms b)
  types' = foldl' go (types a) (R.toList $ types b)
  go :: (Ord a, Ord b) => Relation a b -> (a, b) -> Relation a b
  go acc (n, r) = if p n r acc then acc else R.insert n r acc

-- could move this to a read-only field in Names
numHashChars :: Names' n -> Int
numHashChars b = lenFor hashes
  where lenFor _hashes = 3
        hashes = foldl' f (foldl' g mempty (R.ran $ types b)) (R.ran $ terms b)
        g s r = Set.insert r s
        f s r = Set.insert (Referent.toReference r) s

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

termsNamed :: Ord n => Names' n -> n -> Set Referent
termsNamed = flip R.lookupDom . terms

refTermsNamed :: Ord n => Names' n -> n -> Set Reference
refTermsNamed names n =
  Set.fromList [ r | Referent.Ref r <- toList $ termsNamed names n ]

typesNamed :: Ord n => Names' n -> n -> Set Reference
typesNamed = flip R.lookupDom . types

namesForReferent :: Names' n -> Referent -> Set n
namesForReferent names r = R.lookupRan r (terms names)

namesForReference :: Names' n -> Reference -> Set n
namesForReference names r = R.lookupRan r (types names)

termAliases :: Ord n => Names' n -> n -> Referent -> Set n
termAliases names n r = Set.delete n $ namesForReferent names r

typeAliases :: Ord n => Names' n -> n -> Reference -> Set n
typeAliases names n r = Set.delete n $ namesForReference names r

addType :: Ord n => n -> Reference -> Names' n -> Names' n
addType n r = (<> fromTypes [(n, r)])

addTerm :: Ord n => n -> Referent -> Names' n -> Names' n
addTerm n r = (<> fromTerms [(n, r)])

-- Conditionally apply hash qualifier to term name.
-- Should be the same as the input name if the Names0 is unconflicted.
hqTermName :: Ord n => Names' n -> n -> Referent -> HQ.HashQualified' n
hqTermName b n r = if Set.size (termsNamed b n) > 1
  then hqTermName' b n r
  else HQ.fromName n

hqTypeName :: Ord n => Names' n -> n -> Reference -> HQ.HashQualified' n
hqTypeName b n r = if Set.size (typesNamed b n) > 1
  then hqTypeName b n r
  else HQ.fromName n

hqTypeAliases ::
  Ord n => Names' n -> n -> Reference -> Set (HQ.HashQualified' n)
hqTypeAliases b n r = Set.map (flip (hqTypeName b) r) (typeAliases b n r)

hqTermAliases :: Ord n => Names' n -> n -> Referent -> Set (HQ.HashQualified' n)
hqTermAliases b n r = Set.map (flip (hqTermName b) r) (termAliases b n r)

-- Unconditionally apply hash qualifier long enough to distinguish all the
-- References in this Names0.
hqTermName' :: Names' n -> n -> Referent -> HQ.HashQualified' n
hqTermName' b n r =
  HQ.take (numHashChars b) $ HQ.fromNamedReferent n r

hqTypeName' :: Names' n -> n -> Reference -> HQ.HashQualified' n
hqTypeName' b n r =
  HQ.take (numHashChars b) $ HQ.fromNamedReference n r

-- subtractTerms :: Var v => [v] -> Names -> Names
-- subtractTerms vs n = let
--   taken = Set.fromList (Name.fromVar <$> vs)
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

fromTerms :: Ord n => [(n, Referent)] -> Names' n
fromTerms ts = Names (R.fromList ts) mempty

-- fromTypesV :: Var v => [(v, Reference)] -> Names
-- fromTypesV env =
--   Names mempty . Map.fromList $ fmap (first $ Name.fromVar) env

fromTypes :: Ord n => [(n, Reference)] -> Names' n
fromTypes ts = Names mempty (R.fromList ts)

-- | You may want to sort this list differently afterward.
asSearchResults :: Names0 -> [SearchResult]
asSearchResults b =
  map (uncurry (typeSearchResult b)) (R.toList . types $ b) <>
  map (uncurry (termSearchResult b)) (R.toList . terms $ b)

fromSearchResults :: [SearchResult] -> Names
fromSearchResults = foldl' go mempty where
  go Names{..} (SR.Tp SR.TypeResult{..}) =
    Names terms (R.insert typeName reference types)
  go Names{..} (SR.Tm SR.TermResult{..}) =
    Names (R.insert termName referent terms) types

termSearchResult :: Names0 -> Name -> Referent -> SearchResult
termSearchResult b n r =
  SR.termResult (hqTermName b n r) r (hqTermAliases b n r)

typeSearchResult :: Names0 -> Name -> Reference -> SearchResult
typeSearchResult b n r =
  SR.typeResult (hqTypeName b n r) r (hqTypeAliases b n r)

prefix0 :: Name -> Names0 -> Names0
prefix0 n (Names terms types) = Names terms' types' where
  terms' = R.mapDom (Name.joinDot n) terms
  types' = R.mapDom (Name.joinDot n) types

filter :: Ord n => (n -> Bool) -> Names' n -> Names' n
filter f (Names terms types) = Names (R.filterDom f terms) (R.filterDom f types)

filterByHQs :: Set HashQualified -> Names0 -> Names0
filterByHQs hqs Names{..} = Names terms' types' where
  terms' = R.filter f terms
  types' = R.filter g types
  f (n, r) = any (HQ.matchesNamedReferent n r) hqs
  g (n, r) = any (HQ.matchesNamedReference n r) hqs

filterTerms, filterTypes :: Ord n => (n -> Bool) -> Names' n -> Names' n
filterTerms f (Names terms types) = Names (R.filterDom f terms) types
filterTypes f (Names terms types) = Names terms (R.filterDom f types)

difference :: Ord n => Names' n -> Names' n -> Names' n
difference a b = Names (R.difference (terms a) (terms b))
                       (R.difference (types a) (types b))

intersection :: Ord n => Names' n -> Names' n -> Names' n
intersection a b = Names (R.intersection (terms a) (terms b))
                         (R.intersection (types a) (types b))

contains :: Names' n -> Reference -> Bool
contains names r =
  -- this check makes `contains` O(n) instead of O(log n)
  (Set.member r . Set.map Referent.toReference . R.ran) (terms names)
  || R.memberRan r (types names)

-- | filters out everything from the domain except what's conflicted
conflicts :: Ord n => Names' n -> Names' n
conflicts Names{..} = Names (R.filterManyDom terms) (R.filterManyDom types)

unqualified :: Name -> Name
unqualified = Name.unsafeFromText . unqualified' . Name.toText

unqualified' :: Text -> Text
unqualified' = last . Text.splitOn "."

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
--     (Name.fromVar v, Name.fromVar v2) | (v,v2) <- shortToLongName0 ]
--   terms' = foldl' go termNames shortToLongName
--   types' = foldl' go typeNames shortToLongName
--   in Names terms' types'
--
instance Ord n => Semigroup (Names' n) where (<>) = mappend

instance Ord n => Monoid (Names' n) where
  mempty = Names mempty mempty
  Names e1 t1 `mappend` Names e2 t2 =
    Names (e1 <> e2) (t1 <> t2)

instance Show n => Show (Names' n) where
  show (Names terms types) = "Terms:\n" ++
    foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList terms) ++ "\n" ++
    "Types:\n" ++
    foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList types) ++ "\n"

