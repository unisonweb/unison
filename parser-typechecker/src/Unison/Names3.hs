{-# LANGUAGE RecordWildCards #-}

module Unison.Names3 where

import Unison.Names2 (Names0)
import Unison.HashQualified (HashQualified)
import Unison.HashQualified as HQ
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Referent as Referent
import Data.Set (Set)
import qualified Data.Set as Set
import Unison.Referent (Referent)
import Unison.PrettyPrintEnv (PrettyPrintEnv(..))
import qualified Unison.Util.Relation as R
import qualified Unison.Names2 as Names
import Safe (headMay)
import Data.Foldable (toList)

data Names3 = Names3 { currentNames :: Names0, oldNames :: Names0 }

toPrettyPrintEnv :: Int -> Names3 -> PrettyPrintEnv
toPrettyPrintEnv length names = PrettyPrintEnv terms' types' where
  terms' r = safeHead (termName length r names)
  types' r = safeHead (typeName length r names)

-- do a prefix match on currentNames and, if no match, then check oldNames.
lookupHQType :: HashQualified -> Names3 -> Set Reference
lookupHQType hq Names3{..} = case hq of
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

lookupHQTerm :: HashQualified -> Names3 -> Set Referent
lookupHQTerm hq Names3{..} = case hq of
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
typeName :: Int -> Reference -> Names3 -> Set HashQualified
typeName length r Names3{..} =
  if R.memberRan r . Names.types $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ.fromName n)
               (R.lookupRan r . Names.types $ currentNames)
  else Set.map hq (R.lookupRan r . Names.types $ oldNames)
  where hq n = HQ.take length (HQ.fromNamedReference n r)
        isConflicted n = R.manyDom n (Names.types currentNames)

termName :: Int -> Referent -> Names3 -> Set HashQualified
termName length r Names3{..} =
  if R.memberRan r . Names.terms $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ.fromName n)
               (R.lookupRan r . Names.terms $ currentNames)
  else Set.map hq (R.lookupRan r . Names.terms $ oldNames)
  where hq n = HQ.take length (HQ.fromNamedReferent n r)
        isConflicted n = R.manyDom n (Names.terms currentNames)

-- Set HashQualified -> Branch m -> Action' m v Names3
-- Set HashQualified -> Branch m -> Free (Command m i v) Names3
-- Set HashQualified -> Branch m -> Command m i v Names3
-- populate historical names

safeHead :: Foldable f => f a -> Maybe a
safeHead = headMay . toList

