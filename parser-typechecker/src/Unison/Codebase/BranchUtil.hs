module Unison.Codebase.BranchUtil where
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Branch2 as Branch
import Unison.Codebase.Branch2 (Branch, Branch0)
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.HashQualified (HashQualified'(NameOnly, HashOnly, HashQualified))
import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Relation as R

getNamedTerm :: Path.HQ'Split -> Branch0 m -> Set (Path.NameSegment, Referent)
getNamedTerm (p, hq') b = case hq' of
  HQ'.NameOnly n -> Set.map (n,) (R.lookupDom n terms)
  HQ'.HashQualified n sh -> Set.map (n,) . filter sh $ R.lookupDom n terms
  where
  filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Referent.toShortHash r)
  terms = Branch._terms (Branch.getAt0 p b)

getNamedType :: Path.HQ'Split -> Branch0 m -> Set (Path.NameSegment, Reference)
getNamedType (p, hq') b = case hq' of
  HQ'.NameOnly n -> Set.map (n,) (R.lookupDom n types)
  HQ'.HashQualified n sh -> Set.map (n,) . filter sh $ R.lookupDom n types
  where
  filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Reference.toShortHash r)
  types = Branch._types (Branch.getAt0 p b)

getTerm :: Path.HQSplit -> Branch0 m -> Set Referent
getTerm (p, hq) b = case hq of
    NameOnly n -> R.lookupDom n terms
    HashOnly sh -> filter sh $ Branch.deepReferents b
    HashQualified n sh -> filter sh $ R.lookupDom n terms
  where
  filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Referent.toShortHash r)
  terms = Branch._terms (Branch.getAt0 p b)

getType :: Path.HQSplit -> Branch0 m -> Set Reference
getType (p, hq) b = case hq of
    NameOnly n -> R.lookupDom n types
    HashOnly sh -> filter sh $ Branch.deepTypeReferences b
    HashQualified n sh -> filter sh $ R.lookupDom n types
  where
  filter sh = Set.filter (\r -> sh `SH.isPrefixOf` Reference.toShortHash r)
  types = Branch._types (Branch.getAt0 p b)

getBranch :: Path.Split -> Branch0 m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> snd <$> Map.lookup seg (Branch._children b)
  h : p -> 
    (Branch.head . snd <$> Map.lookup h (Branch._children b)) >>=
      getBranch (Path.fromList p, seg)
        

makeAddTermName, makeDeleteTermName :: 
  Path.Split -> Referent -> (Path.Path, Branch0 m -> Branch0 m)
makeAddTermName (p, name) r = (p, Branch.addTermName r name)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeAddTypeName, makeDeleteTypeName :: 
  Path.Split -> Reference -> (Path.Path, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r = (p, Branch.addTypeName r name)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

-- setTerm :: Path.PathSplit' -> Set Referent -> Branch0 m -> Branch0 m
-- setType :: Path.PathSplit' -> Set Reference -> Branch0 m -> Branch0 m
-- setBranch :: Path.PathSplit' -> Branch0 m -> Branch0 m -> Branch0 m
-- setBranch somewhere new oldroot === newroot
-- deleteAt :: Target -> Branch0 m -> Branch0 m
-- move :: Path.HQPathSplit Path.Absolute
--      -> Path.PathSplit Path.Absolute
--      -> Branch0 m
--      -> Branch0 m
-- move src dest b = foldl' step b (toList (targetTypes src)) where
--   step b TargetType = set
