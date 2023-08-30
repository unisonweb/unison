module Unison.Codebase.BranchUtil
  ( -- * Branch creation
    fromNames,

    -- * Branch queries
    getBranch,
    getTerm,
    getType,
    getTermMetadataAt,
    getTypeMetadataAt,
    getTermMetadataHQNamed,
    getTypeMetadataHQNamed,

    -- * Branch modifications
    makeSetBranch,
    makeAddTypeName,
    makeDeleteTypeName,
    makeAddTermName,
    makeDeleteTermName,
    makeDeletePatch,
    makeReplacePatch,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.ShortHash qualified as SH
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Metadata (Metadata)
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified' (HashQualified (HashQualified, NameOnly))
import Unison.HashQualified' qualified as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Util.List qualified as List
import Unison.Util.Relation qualified as R
import Unison.Util.Relation4 qualified as R4
import Unison.Util.Star3 qualified as Star3

-- | Creates a branch containing all of the given names, with a single history node.
fromNames :: (Monad m) => Names -> Branch m
fromNames names0 = Branch.stepManyAt (typeActions <> termActions) Branch.empty
  where
    typeActions = map doType . R.toList $ Names.types names0
    termActions = map doTerm . R.toList $ Names.terms names0
    --  doTerm :: (Name, Referent) -> (Path, Branch0 m -> Branch0 m)
    doTerm (n, r) = makeAddTermName (Path.splitFromName n) r mempty -- no metadata
    --  doType :: (Name, Reference) -> (Path, Branch0 m -> Branch0 m)
    doType (n, r) = makeAddTypeName (Path.splitFromName n) r mempty -- no metadata

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

makeReplacePatch :: (Applicative m) => Path.Split -> Patch -> (Path, Branch0 m -> Branch0 m)
makeReplacePatch (p, name) patch = (p, Branch.replacePatch name patch)

makeDeletePatch :: Path.Split -> (Path, Branch0 m -> Branch0 m)
makeDeletePatch (p, name) = (p, Branch.deletePatch name)

makeAddTypeName :: Path.Split -> Reference -> Metadata -> (Path, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r md = (p, Branch.addTypeName r name md)

makeDeleteTypeName :: Path.Split -> Reference -> (Path, Branch0 m -> Branch0 m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

makeSetBranch ::
  Path.Split -> Branch m -> (Path, Branch0 m -> Branch0 m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)
