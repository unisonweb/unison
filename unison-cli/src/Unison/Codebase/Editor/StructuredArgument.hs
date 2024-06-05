module Unison.Codebase.Editor.StructuredArgument where

import GHC.Generics (Generic)
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)
import Unison.Server.Backend (ShallowListEntry)
import Unison.Server.SearchResult (SearchResult)
import Unison.Symbol (Symbol)

-- | The types that can be referenced by a numeric command argument.
data StructuredArgument
  = AbsolutePath Path.Absolute
  | Name Name
  | HashQualified (HQ.HashQualified Name)
  | Project ProjectName
  | ProjectBranch (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | Namespace CausalHash
  | NameWithBranchPrefix AbsBranchId Name
  | HashQualifiedWithBranchPrefix AbsBranchId (HQ'.HashQualified Name)
  | ShallowListEntry Path.Absolute (ShallowListEntry Symbol Ann)
  | SearchResult (Maybe Path) SearchResult
  deriving (Eq, Generic, Show)
