module Unison.Codebase.Editor.Input
  ( Input(..)
  , Event(..)
  , OutputLocation(..)
  , PatchPath
  ) where

import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import           Unison.Codebase.Path           ( Path' )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.Editor.RemoteRepo
import           Unison.Reference (Reference)

data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type PatchPath = Path.Split'

data Input
  -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    = ForkLocalBranchI Path' Path'
    -- merge first causal into destination
    | MergeLocalBranchI Path' Path'
    | PullRemoteBranchI RemoteRepo Path'
    | PushRemoteBranchI RemoteRepo Path'
    -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    -- change directory
    | SwitchBranchI Path'
    -- > names foo
    -- > names foo.bar
    -- > names .foo.bar
    -- > names .foo.bar#asdflkjsdf
    -- > names #sdflkjsdfhsdf
    | NamesI HQ.HashQualified
    | AliasTermI Path.HQSplit' Path.Split'
    | AliasTypeI Path.HQSplit' Path.Split'
    -- Move = Rename; It's an HQSplit' not an HQSplit', meaning the arg has to have a name.
    | MoveTermI Path.HQSplit' Path.Split'
    | MoveTypeI Path.HQSplit' Path.Split'
    | MoveBranchI Path.Split' Path.Split'
    | MovePatchI Path.Split' Path.Split'
    | CopyPatchI Path.Split' Path.Split'
    -- delete = unname
    -- | DeleteDefnI [Path.HQSplit']
    | DeleteTermI Path.HQSplit'
    | DeleteTypeI Path.HQSplit'
    | DeleteBranchI Path.Split'
    | DeletePatchI Path.Split'
    -- resolving naming conflicts within `branchpath`
      -- Add the specified name after deleting all others for a given reference
      -- within a given branch.
    | ResolveTermNameI Path.HQSplit'
    | ResolveTypeNameI Path.HQSplit'
  -- edits stuff:
    | AddI [HQ'.HashQualified]
    | UpdateI PatchPath [HQ'.HashQualified]
    | TodoI PatchPath Path'
    | PatchI PatchPath Path'
    | ListEditsI PatchPath
    -- -- create and remove update directives
    | DeprecateTermI PatchPath Path.HQSplit'
    | DeprecateTypeI PatchPath Path.HQSplit'
    | AddTermReplacementI PatchPath Reference Reference
    | AddTypeReplacementI PatchPath Reference Reference
    | RemoveTermReplacementI PatchPath Reference Reference
    | RemoveTypeReplacementI PatchPath Reference Reference
  | UndoI
  -- execute an IO object with arguments
  | ExecuteI String
  | TestI Bool Bool -- TestI showSuccesses showFailures
  -- metadata
  -- link from to
  | LinkI Path.HQSplit' Path.HQSplit'
  -- unlink from to
  | UnlinkI Path.HQSplit' Path.HQSplit'
  -- links from <type>
  | LinksI Path.HQSplit' (Maybe String)
  -- other
  | UndoRootI
  | SearchByNameI Bool [String]
  | FindPatchI
  | ShowDefinitionI OutputLocation [String]
  | ShowDefinitionByPrefixI OutputLocation [String]
  | UpdateBuiltinsI
  | MergeBuiltinsI
  | QuitI
  deriving (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath
  -- ClipboardLocation
  deriving (Eq, Show)
