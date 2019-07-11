{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Names3 where

import Data.Foldable (toList)
import Data.List (foldl')
import Data.Sequence (Seq)
import Data.Set (Set)
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Referent as Referent
import Unison.Util.Relation (Relation)
import qualified Data.Set as Set
import qualified Unison.Name as Name
import qualified Unison.Names2
import qualified Unison.Names2 as Names
import qualified Unison.Util.Relation as R

data Names = Names { currentNames :: Names0, oldNames :: Names0 } deriving Show

type Names0 = Unison.Names2.Names0
pattern Names0 terms types = Unison.Names2.Names terms types

data ResolutionFailure v a
  = TermResolutionFailure v a (Set Referent)
  | TypeResolutionFailure v a (Set Reference)
  deriving (Eq,Ord,Show)

type ResolutionResult v a r = Either (Seq (ResolutionFailure v a)) r

filterTypes :: (Name -> Bool) -> Names0 -> Names0
filterTypes = Unison.Names2.filterTypes

unionLeft0 :: Names0 -> Names0 -> Names0
unionLeft0 = Unison.Names2.unionLeft

map0 :: (Name -> Name) -> Names0 -> Names0
map0 f (Names.Names terms types) = Names.Names terms' types' where
  terms' = R.mapDom f terms
  types' = R.mapDom f types

names0 :: Relation Name Referent -> Relation Name Reference -> Names0
names0 = Unison.Names2.Names

types0 :: Names0 -> Relation Name Reference
types0 = Names.types

terms0 :: Names0 -> Relation Name Referent
terms0 = Names.terms

makeAbsolute0:: Names0 -> Names0
makeAbsolute0 = map0 Name.makeAbsolute

-- do a prefix match on currentNames and, if no match, then check oldNames.
lookupHQType :: HashQualified -> Names -> Set Reference
lookupHQType hq Names{..} = case hq of
  HQ.NameOnly n -> R.lookupDom n (Names.types currentNames)
  HQ.HashQualified n sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Reference.isPrefixOf sh) (R.lookupDom n $ Names.types ns)
  HQ.HashOnly sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Reference.isPrefixOf sh) (R.ran $ Names.types ns)

hasTermNamed :: Name -> Names -> Bool
hasTermNamed n ns = not (Set.null $ lookupHQTerm (HQ.NameOnly n) ns)

hasTypeNamed :: Name -> Names -> Bool
hasTypeNamed n ns = not (Set.null $ lookupHQType (HQ.NameOnly n) ns)

lookupHQTerm :: HashQualified -> Names -> Set Referent
lookupHQTerm hq Names{..} = case hq of
  HQ.NameOnly n -> R.lookupDom n (Names.terms currentNames)
  HQ.HashQualified n sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Referent.isPrefixOf sh) (R.lookupDom n $ Names.terms ns)
  HQ.HashOnly sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Referent.isPrefixOf sh) (R.ran $ Names.terms ns)

-- If `r` is in "current" names, look up each of its names, and hash-qualify
-- them if they are conflicted names.  If `r` isn't in "current" names, look up
-- each of its "old" names and hash-qualify them.
typeName :: Int -> Reference -> Names -> Set HQ'.HashQualified
typeName length r Names{..} =
  if R.memberRan r . Names.types $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.types $ currentNames)
  else Set.map hq (R.lookupRan r . Names.types $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReference n r)
        isConflicted n = R.manyDom n (Names.types currentNames)

termName :: Int -> Referent -> Names -> Set HQ'.HashQualified
termName length r Names{..} =
  if R.memberRan r . Names.terms $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.terms $ currentNames)
  else Set.map hq (R.lookupRan r . Names.terms $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReferent n r)
        isConflicted n = R.manyDom n (Names.terms currentNames)

-- Set HashQualified -> Branch m -> Action' m v Names
-- Set HashQualified -> Branch m -> Free (Command m i v) Names
-- Set HashQualified -> Branch m -> Command m i v Names
-- populate historical names
lookupHQPattern :: HQ.HashQualified -> Names -> Set (Reference, Int)
lookupHQPattern hq names = Set.fromList
  [ (r, cid) | Referent.Con r cid _ <- toList $ lookupHQTerm hq names ]

-- Given a mapping from name to qualified name, update a `Names`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `Names`,
-- the alias `Some` will map to that same constructor and shadow
-- anything else that is currently called `Some`.
--
-- Only affects `currentNames`.
importing :: [(Name, Name)] -> Names -> Names
importing shortToLongName ns =
  ns { currentNames = Names.Names
         (foldl' go (Names.terms $ currentNames ns) shortToLongName)
         (foldl' go (Names.types $ currentNames ns) shortToLongName)
     }
  where
  go :: (Ord a, Ord b) => Relation a b -> (a, a) -> Relation a b
  go m (shortname, qname) = case R.lookupDom qname m of
    s | Set.null s -> m
      | otherwise -> R.insertManyRan shortname s (R.deleteDom shortname m)
