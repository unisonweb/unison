module Unison.Codebase.BranchUtil where

import Control.Lens (view)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.Codebase.Branch (Branch, Branch0)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Metadata (Metadata)
import qualified Unison.Codebase.Metadata as Metadata
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.HashQualified' (HashQualified' (HashQualified, NameOnly))
import qualified Unison.HashQualified' as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Names2 (Names0)
import qualified Unison.Names2 as Names
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.ShortHash as SH
import qualified Unison.Util.List as List
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation4 as R4
import qualified Unison.Util.Star3 as Star3

fromNames0 :: Monad m => Names0 -> Branch m
fromNames0 names0 = Branch.one $ addFromNames0 names0 Branch.empty0

-- can produce a pure value because there's no history to traverse
hashesFromNames0 :: Monad m => Names0 -> Map Branch.Hash (Branch m)
hashesFromNames0 = deepHashes . fromNames0
  where
    deepHashes :: Branch m -> Map Branch.Hash (Branch m)
    deepHashes b =
      Map.singleton (Branch.headHash b) b
        <> (foldMap deepHashes . view Branch.children . Branch.head) b

addFromNames0 :: Monad m => Names0 -> Branch0 m -> Branch0 m
addFromNames0 names0 = Branch.stepManyAt0 (typeActions <> termActions)
  where
    typeActions = map doType . R.toList $ Names.types names0
    termActions = map doTerm . R.toList $ Names.terms names0
    --  doTerm :: (Name, Referent) -> (Path, Branch0 m -> Branch0 m)
    doTerm (n, r) = case Path.splitFromName n of
      Nothing -> errorEmptyName
      Just split -> makeAddTermName split r mempty -- no metadata
      --  doType :: (Name, Reference) -> (Path, Branch0 m -> Branch0 m)
    doType (n, r) = case Path.splitFromName n of
      Nothing -> errorEmptyName
      Just split -> makeAddTypeName split r mempty -- no metadata
    errorEmptyName = error "encountered an empty name"

getTerm :: Path.HQSplit -> Branch0 m -> Set Referent
getTerm (p, hq) b = case hq of
  NameOnly n -> Star3.lookupD1 n terms
  HashQualified n sh -> filter sh $ Star3.lookupD1 n terms
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Referent.toShortHash)
    terms = Branch._terms (Branch.getAt0 p b)

getTermMetadataHQNamed ::
  (Path.Path, HQ'.HQSegment) -> Branch0 m -> Metadata.R4 Referent NameSegment
getTermMetadataHQNamed (path, hqseg) b =
  R4.filter (\(r, n, _t, _v) -> HQ'.matchesNamedReferent n r hqseg) terms
  where
    terms = Metadata.starToR4 . Branch._terms $ Branch.getAt0 path b

getTypeMetadataHQNamed ::
  (Path.Path, HQ'.HQSegment) ->
  Branch0 m ->
  Metadata.R4 Reference NameSegment
getTypeMetadataHQNamed (path, hqseg) b =
  R4.filter (\(r, n, _t, _v) -> HQ'.matchesNamedReference n r hqseg) types
  where
    types = Metadata.starToR4 . Branch._types $ Branch.getAt0 path b

-- todo: audit usages and maybe eliminate!
-- Only returns metadata for the term at the exact level given
getTermMetadataAt :: (Path.Path, a) -> Referent -> Branch0 m -> Metadata
getTermMetadataAt (path, _) r b = Set.fromList <$> List.multimap mdList
  where
    mdList :: [(Metadata.Type, Metadata.Value)]
    mdList = Set.toList . R.ran . Star3.d3 . Star3.selectFact (Set.singleton r) $ terms
    terms = Branch._terms $ Branch.getAt0 path b

getType :: Path.HQSplit -> Branch0 m -> Set Reference
getType (p, hq) b = case hq of
  NameOnly n -> Star3.lookupD1 n types
  HashQualified n sh -> filter sh $ Star3.lookupD1 n types
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)
    types = Branch._types (Branch.getAt0 p b)

getTypeByShortHash :: SH.ShortHash -> Branch0 m -> Set Reference
getTypeByShortHash sh b = filter sh $ Branch.deepTypeReferences b
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)

getTypeMetadataAt :: (Path.Path, a) -> Reference -> Branch0 m -> Metadata
getTypeMetadataAt (path, _) r b = Set.fromList <$> List.multimap mdList
  where
    mdList :: [(Metadata.Type, Metadata.Value)]
    mdList = Set.toList . R.ran . Star3.d3 . Star3.selectFact (Set.singleton r) $ types
    types = Branch._types $ Branch.getAt0 path b

getBranch :: Path.Split -> Branch0 m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> Map.lookup seg (Branch._children b)
  h : p ->
    (Branch.head <$> Map.lookup h (Branch._children b))
      >>= getBranch (Path.fromList p, seg)

makeAddTermName :: Path.Split -> Referent -> Metadata -> (Path, Branch0 m -> Branch0 m)
makeAddTermName (p, name) r md = (p, Branch.addTermName r name md)

makeDeleteTermName :: Path.Split -> Referent -> (Path, Branch0 m -> Branch0 m)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeReplacePatch :: Applicative m => Path.Split -> Patch -> (Path, Branch0 m -> Branch0 m)
makeReplacePatch (p, name) patch = (p, Branch.replacePatch name patch)

makeDeletePatch :: Path.Split -> (Path, Branch0 m -> Branch0 m)
makeDeletePatch (p, name) = (p, Branch.deletePatch name)

makeAddTypeName :: Path.Split -> Reference -> Metadata -> (Path, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r md = (p, Branch.addTypeName r name md)

makeDeleteTypeName :: Path.Split -> Reference -> (Path, Branch0 m -> Branch0 m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

-- to delete, just set with Branch.empty
makeSetBranch ::
  Path.Split -> Branch m -> (Path, Branch0 m -> Branch0 m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)
