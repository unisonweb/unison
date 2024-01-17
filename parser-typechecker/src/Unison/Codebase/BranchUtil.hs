module Unison.Codebase.BranchUtil
  ( -- * Branch creation
    fromNames,

    -- * Branch queries
    getBranch,
    getTerm,
    getType,

    -- * Branch modifications
    makeSetBranch,
    makeAddTypeName,
    makeDeleteTypeName,
    makeAnnihilateTypeName,
    makeAddTermName,
    makeDeleteTermName,
    makeAnnihilateTermName,
    makeDeletePatch,
    makeReplacePatch,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified' (HashQualified (HashQualified, NameOnly))
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as SH
import Unison.Util.Relation qualified as R
import Unison.Util.Star3 qualified as Star3

-- | Creates a branch containing all of the given names, with a single history node.
fromNames :: (Monad m) => Names -> Branch m
fromNames names0 = Branch.stepManyAt (typeActions <> termActions) Branch.empty
  where
    typeActions = map doType . R.toList $ Names.types names0
    termActions = map doTerm . R.toList $ Names.terms names0
    doTerm (n, r) = makeAddTermName (Path.splitFromName n) r
    doType (n, r) = makeAddTypeName (Path.splitFromName n) r

getTerm :: Path.HQSplit -> Branch0 m -> Set Referent
getTerm (p, hq) b = case hq of
  NameOnly n -> Star3.lookupD1 n terms
  HashQualified n sh -> filter sh $ Star3.lookupD1 n terms
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Referent.toShortHash)
    terms = Branch._terms (Branch.getAt0 p b)

getType :: Path.HQSplit -> Branch0 m -> Set Reference.TypeReference
getType (p, hq) b = case hq of
  NameOnly n -> Star3.lookupD1 n types
  HashQualified n sh -> filter sh $ Star3.lookupD1 n types
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)
    types = Branch._types (Branch.getAt0 p b)

getBranch :: Path.Split -> Branch0 m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> Map.lookup seg (Branch._children b)
  h : p ->
    (Branch.head <$> Map.lookup h (Branch._children b))
      >>= getBranch (Path.fromList p, seg)

makeAddTermName :: Path.Split -> Referent -> (Path, Branch0 m -> Branch0 m)
makeAddTermName (p, name) r = (p, Branch.addTermName r name)

makeDeleteTermName :: Path.Split -> Referent -> (Path, Branch0 m -> Branch0 m)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeAnnihilateTermName :: Path.Split -> (Path, Branch0 m -> Branch0 m)
makeAnnihilateTermName (p, name) = (p, Branch.annihilateTermName name)

makeAnnihilateTypeName :: Path.Split -> (Path, Branch0 m -> Branch0 m)
makeAnnihilateTypeName (p, name) = (p, Branch.annihilateTypeName name)

makeReplacePatch :: (Applicative m) => Path.Split -> Patch -> (Path, Branch0 m -> Branch0 m)
makeReplacePatch (p, name) patch = (p, Branch.replacePatch name patch)

makeDeletePatch :: Path.Split -> (Path, Branch0 m -> Branch0 m)
makeDeletePatch (p, name) = (p, Branch.deletePatch name)

makeAddTypeName :: Path.Split -> Reference -> (Path, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r = (p, Branch.addTypeName r name)

makeDeleteTypeName :: Path.Split -> Reference -> (Path, Branch0 m -> Branch0 m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

makeSetBranch :: Path.Split -> Branch m -> (Path, Branch0 m -> Branch0 m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)
