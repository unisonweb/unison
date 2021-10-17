module Unison.Codebase.BranchUtil where

import Unison.Prelude

import qualified Data.Set as Set
import qualified Data.Map as Map
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Branch (Branch, BranchSnapshot)
import qualified Unison.Names2 as Names
import Unison.Names2 (Names0)
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.HashQualified' (HashQualified(NameOnly, HashQualified))
import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3
import Unison.Codebase.Metadata (Metadata)
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Util.List as List
import Unison.Codebase.Patch (Patch)
import Unison.NameSegment (NameSegment)
import Control.Lens (view)

fromNames0 :: Monad m => Names0 -> Branch m
fromNames0 names0 = Branch.one $ addFromNames0 names0 Branch.emptySnapshot

-- can produce a pure value because there's no history to traverse
hashesFromNames0 :: Monad m => Names0 -> Map Branch.Hash (Branch m)
hashesFromNames0 = deepHashes . fromNames0 where
  deepHashes :: Branch m -> Map Branch.Hash (Branch m)
  deepHashes b = Map.singleton (Branch.headHash b) b
    <> (foldMap deepHashes . view Branch.children_ . Branch.head) b

addFromNames0 :: Monad m => Names0 -> BranchSnapshot m -> BranchSnapshot m
addFromNames0 names0 = Branch.stepManyAt0 (typeActions <> termActions)
  where
  typeActions = map doType . R.toList $ Names.types names0
  termActions = map doTerm . R.toList $ Names.terms names0
--  doTerm :: (Name, Referent) -> (Path, BranchSnapshot m -> BranchSnapshot m)
  doTerm (n, r) = case Path.splitFromName n of
    Nothing -> errorEmptyName
    Just split -> makeAddTermName split r mempty -- no metadata
--  doType :: (Name, Reference) -> (Path, BranchSnapshot m -> BranchSnapshot m)
  doType (n, r) = case Path.splitFromName n of
             Nothing -> errorEmptyName
             Just split -> makeAddTypeName split r mempty -- no metadata
  errorEmptyName = error "encountered an empty name"

getTerm :: Path.HQSplit -> BranchSnapshot m -> Set Referent
getTerm (p, hq) b = case hq of
    NameOnly n -> Star3.lookupD1 n terms
    HashQualified n sh -> filter sh $ Star3.lookupD1 n terms
  where
  filter sh = Set.filter (SH.isPrefixOf sh . Referent.toShortHash)
  terms = Branch.terms (Branch.getAt0 p b)

getTermMetadataHQNamed
  :: (Path.Path, HQ'.HQSegment) -> BranchSnapshot m -> Metadata.R4 Referent NameSegment
getTermMetadataHQNamed (path, hqseg) b =
  R4.filter (\(r,n,_t,_v) -> HQ'.matchesNamedReferent n r hqseg) terms
  where terms = Metadata.starToR4 . Branch.terms $ Branch.getAt0 path b

getTypeMetadataHQNamed
  :: (Path.Path, HQ'.HQSegment)
  -> BranchSnapshot m
  -> Metadata.R4 Reference NameSegment
getTypeMetadataHQNamed (path, hqseg) b =
  R4.filter (\(r,n,_t,_v) -> HQ'.matchesNamedReference n r hqseg) types
  where types = Metadata.starToR4 . Branch.types $ Branch.getAt0 path b

-- todo: audit usages and maybe eliminate!
-- Only returns metadata for the term at the exact level given
getTermMetadataAt :: (Path.Path, a) -> Referent -> BranchSnapshot m -> Metadata
getTermMetadataAt (path,_) r b = Set.fromList <$> List.multimap mdList
  where
  mdList :: [(Metadata.Type, Metadata.Value)]
  mdList = Set.toList . R.ran . Star3.d3 . Star3.selectFact (Set.singleton r) $ terms
  terms = Branch.terms $ Branch.getAt0 path b

getType :: Path.HQSplit -> BranchSnapshot m -> Set Reference
getType (p, hq) b = case hq of
    NameOnly n -> Star3.lookupD1 n types
    HashQualified n sh -> filter sh $ Star3.lookupD1 n types
  where
  filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)
  types = Branch.types (Branch.getAt0 p b)

getTypeByShortHash :: SH.ShortHash -> BranchSnapshot m -> Set Reference
getTypeByShortHash sh b = filter sh $ Branch.deepTypeReferences b
  where
  filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)

getTypeMetadataAt :: (Path.Path, a) -> Reference -> BranchSnapshot m -> Metadata
getTypeMetadataAt (path,_) r b = Set.fromList <$> List.multimap mdList
  where
  mdList :: [(Metadata.Type, Metadata.Value)]
  mdList = Set.toList . R.ran . Star3.d3 . Star3.selectFact (Set.singleton r) $ types
  types = Branch.types $ Branch.getAt0 path b

getBranch :: Path.Split -> BranchSnapshot m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> Map.lookup seg (Branch.children b)
  h : p ->
    (Branch.head <$> Map.lookup h (Branch.children b)) >>=
      getBranch (Path.fromList p, seg)


makeAddTermName :: Path.Split -> Referent -> Metadata -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeAddTermName (p, name) r md = (p, Branch.addTermName r name md)

makeDeleteTermName :: Path.Split -> Referent -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeReplacePatch :: Applicative m => Path.Split -> Patch -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeReplacePatch (p, name) patch = (p, Branch.replacePatch name patch)

makeDeletePatch :: Path.Split -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeDeletePatch (p, name) = (p, Branch.deletePatch name)

makeAddTypeName :: Path.Split -> Reference -> Metadata -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeAddTypeName (p, name) r md = (p, Branch.addTypeName r name md)

makeDeleteTypeName :: Path.Split -> Reference -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

-- to delete, just set with Branch.empty
makeSetBranch ::
  Path.Split -> Branch m -> (Path, BranchSnapshot m -> BranchSnapshot m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)
