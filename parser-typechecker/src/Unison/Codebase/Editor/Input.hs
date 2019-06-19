module Unison.Codebase.Editor.Input
  ( Input(..)
  , Event(..)
  , OutputLocation(..)
  ) where

import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Unison.Codebase.Branch2       as Branch
import           Unison.HashQualified           ( HashQualified )
import           Unison.Codebase.Path           ( Path' )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.Editor.RemoteRepo
import           Unison.Reference (Reference)

data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type EditPath = Path.Split'

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
    | AddI [HashQualified]
    | UpdateI EditPath [HashQualified]
    | TodoI EditPath Path'
    | PropagateI EditPath Path'
    | ListEditsI EditPath
    -- -- create and remove update directives
    | DeprecateTermI EditPath Path.HQ'Split'
    | DeprecateTypeI EditPath Path.HQ'Split'
    | AddTermReplacementI EditPath Reference Reference
    | AddTypeReplacementI EditPath Reference Reference
    | RemoveTermReplacementI EditPath Reference Reference
    | RemoveTypeReplacementI EditPath Reference Reference
  | UndoI
  -- execute an IO object with arguments
  | ExecuteI String
  -- metadata
  -- link src <link-key> apiDocsValue
  | LinkI Path.HQ'Split' Path.HQSplit' Path.HQSplit'
  -- unlink src <link-key> apiDocsValue
  | UnlinkI Path.HQ'Split' Path.HQSplit' Path.HQSplit'
  -- links src <link-key>
  | LinksI Path.HQ'Split' (Maybe Path.HQSplit')
  -- other
  | UndoRootI
  | SearchByNameI [String]
  | FindPatchI
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
