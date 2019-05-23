module Unison.Codebase.BranchUtil where
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Branch2 as Branch
import Unison.Codebase.Branch2 (Branch0)
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.HashQualified (HashQualified'(NameOnly, HashOnly, HashQualified))
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Relation as R

data BranchTarget = TargetType | TargetTerm | TargetBranch
  deriving (Eq, Ord, Show)
-- data Target p = Target { targetPath :: p
--                        , targetTypes :: Set TargetType }

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

getTerm' :: Set BranchTarget -> Path.HQSplit -> Branch0 m -> Set Referent
getTerm' t = if Set.member TargetTerm t then getTerm else \_ _ -> mempty

getType' :: Set BranchTarget -> Path.HQSplit -> Branch0 m -> Set Reference
getType' t = if Set.member TargetType t then getType else \_ _ -> mempty

-- getAt :: Path.HQSplit ->

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
