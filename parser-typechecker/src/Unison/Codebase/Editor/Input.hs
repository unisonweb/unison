module Unison.Codebase.Editor.Input
  ( Input(..)
  , Event(..)
  , OutputLocation(..)
  ) where

import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Unison.Codebase.Branch2       as Branch
import           Unison.HashQualified           ( HashQualified )
import           Unison.Codebase.Path           ( Path, Path' )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.Editor.RemoteRepo

data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"

data Input
  -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    = ForkLocalBranchI Path' Path'
    | ForkRemoteBranchI RemoteRepo Path Path'
    -- merge first causal into destination
    | MergeLocalBranchI Path' Path'
    | MergeRemoteBranchI RemoteRepo Path Path.Split'
    -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    -- change directory
    | SwitchBranchI Path'
    | AliasTermI Path.HQSplit' Path.Split'
    | AliasTypeI Path.HQSplit' Path.Split'
    -- Move = Rename; It's an HQ'Split' not an HQSplit', meaning the arg has to have a name.
    | MoveTermI Path.HQ'Split' Path.Split'
    | MoveTypeI Path.HQ'Split' Path.Split'
    | MoveBranchI Path.Split' Path.Split'
    -- delete = unname
    -- | DeleteDefnI [Path.HQSplit']
    | DeleteTermI Path.HQ'Split'
    | DeleteTypeI Path.HQ'Split'
    | DeleteBranchI Path.Split'
    -- resolving naming conflicts within `branchpath`
      -- Add the specified name after deleting all others for a given reference
      -- within a given branch.
    | ResolveTermNameI Path.HQ'Split'
    | ResolveTypeNameI Path.HQ'Split'
  -- edits stuff:
    | TodoI Path.Split' Path'
    | PropagateI Path.Split' Path'
    -- -- create and remove update directives
    -- | CreateEditsI EditGuid -- implies SetEdits?
    -- | SetEditsI EditGuid
    -- | ClearEdits -- don't record (don't allow?) term edits
    -- | ListEditsI EditGuid
    -- | ReplaceTermI EditGuid Reference Reference
    -- | ReplaceTypeI EditGuid Reference Reference
    -- -- clear updates for a term or type
    -- | RemoveAllTermUpdatesI EditGuid Reference
    -- | RemoveAllTypeUpdatesI EditGuid Reference
    -- -- resolve update conflicts
    -- | ChooseUpdateForTermI EditGuid Reference Reference
    -- | ChooseUpdateForTypeI EditGuid Reference Reference
  -- execute an IO object with arguments
  | ExecuteI String
  -- other
  | AddI [HashQualified]
  | UpdateI Path.Split' [HashQualified]
  | UndoRootI
  | SearchByNameI [String]
  | ShowDefinitionI OutputLocation [String]
  | ShowDefinitionByPrefixI OutputLocation [String]
  | UpdateBuiltinsI
  | QuitI
  deriving (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath
  -- ClipboardLocation
  deriving (Eq, Show)
