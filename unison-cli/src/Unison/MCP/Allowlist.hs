-- | Which UCM commands each of the MCP transcript tools allows.
--
-- The tiers nest: a tool allows every command at or below its own tier. The tier of a command is the least powerful
-- tool that may run it, and a command that isn't listed (including any command added later) is `All`.
module Unison.MCP.Allowlist
  ( Tier (..),
    toolName,
    commandTier,
    commandTiers,
    canonicalNames,
    Violation (..),
    validate,
    renderViolations,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Unison.Codebase.Transcript
import Unison.CommandLine.InputPattern (aliases, patternName)
import Unison.CommandLine.InputPatterns qualified as IP
import Unison.Prelude

data Tier
  = -- | Reads from the codebase or from Share. Never changes the codebase.
    ReadOnly
  | -- | Also makes changes that are easy to undo, with `undo`, the reflog, or by deleting a branch.
    Local
  | -- | Also runs arbitrary code, which can do anything IO can.
    IO
  | All
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The name of the MCP tool for a tier.
toolName :: Tier -> Text
toolName = \case
  ReadOnly -> "transcript-read-only"
  Local -> "transcript"
  IO -> "transcript-io"
  All -> "transcript-all"

-- | The least powerful tier that may run each command, by canonical name. Aliases take the tier of their command.
commandTiers :: Map String Tier
commandTiers =
  Map.fromList . concat $
    [ (,ReadOnly) <$> readOnly,
      (,Local) <$> local,
      (,IO) <$> io
    ]
  where
    readOnly =
      [ "find",
        "find.all",
        "find-in",
        "find-in.all",
        "find.verbose",
        "find.all.verbose",
        "text.find",
        "text.find.all",
        "list",
        "list-fuzzy",
        "rewrite.find",
        "view",
        "view.global",
        "display",
        "signature",
        "names",
        "dependencies",
        "dependents",
        "namespace.dependencies",
        "docs",
        "diff.namespace",
        "diff.branch",
        "diff.update",
        "history",
        "reflog",
        "project.reflog",
        "reflog.global",
        "deprecated.root-reflog",
        "todo",
        "test",
        "test.all",
        "projects",
        "branches",
        "config.get",
        "help",
        "help-topics",
        "version",
        "api",
        "load",
        -- These write a file outside the codebase.
        "display.to",
        "docs.to-html",
        "debug.file",
        -- In a transcript these only produce a scratch file that is part of the output.
        "edit",
        "edit.new",
        "edit.dependents",
        "edit.namespace",
        "edit.to",
        "edit.new.to",
        "edit.dependents.to",
        "edit.namespace.to",
        "rewrite",
        -- Navigation within the session.
        "switch",
        "back",
        "deprecated.cd",
        "deprecated.up",
        "cancel",
        "clear",
        "quit",
        "share.project-search",
        "share.search",
        "share.source",
        "share.render",
        "share.docs",
        "share.releases",
        "share.releases.detail"
      ]
    local =
      [ "update",
        "add.run",
        "builtins.merge",
        "builtins.mergeio",
        "builtins.update",
        "merge",
        "merge.commit",
        "lib.install",
        "lib.install.local",
        "lib.upgrade",
        "upgrade.commit",
        "branch",
        "branch.empty",
        "branch.rename",
        "branch.squash",
        "project.create",
        "project.create-empty",
        "project.rename",
        "fork",
        "clone",
        "release.draft",
        "move",
        "move.term",
        "move.type",
        "move.namespace",
        "moveTo",
        "rename",
        "alias.term",
        "alias.type",
        "alias.many",
        "create.author",
        "undo",
        "redo",
        "history.comment",
        "pull",
        "pull.without-history",
        "compile",
        "reset",
        -- Everything the reflog can bring back. Deleting a whole project or branch is `All`.
        "delete",
        "delete.term",
        "delete.type",
        "delete.term.force",
        "delete.type.force",
        "delete.namespace",
        "delete.namespace.force"
      ]
    io =
      [ "run",
        "run.profiled",
        "run.profiled.full",
        "io.test",
        "io.test.all"
      ]

-- | Every command name and alias, to its canonical name.
canonicalNames :: Map String String
canonicalNames =
  Map.fromList [(name, patternName pat) | pat <- IP.validInputs, name <- patternName pat : aliases pat]

-- | The tier of a command, by name or alias. Unknown commands are `All`.
commandTier :: String -> Tier
commandTier name =
  case Map.lookup name canonicalNames of
    Nothing -> All
    Just canonical -> Map.findWithDefault All canonical commandTiers

data Violation = Violation
  { location :: Text,
    offender :: Text,
    requires :: Tier
  }
  deriving stock (Eq, Show)

-- | Everything in the transcript that a tool at the given tier doesn't allow.
validate :: Tier -> Transcript -> [Violation]
validate allowed transcript =
  [ violation
  | violation@Violation {requires} <- frontmatterViolations ++ blockViolations,
    requires > allowed
  ]
  where
    frontmatterViolations =
      [Violation "frontmatter" "autoupdate" Local | Just True <- [autoupdate (behaviors (settings transcript))]]
    blocks = [block | Right block <- stanzas transcript]
    blockViolations = concat (zipWith blockViolations' [1 :: Int ..] blocks)
    blockViolations' n = \case
      Unison {} -> []
      Ucm _ lines ->
        [ Violation (blockLocation n <> ", line " <> tShow i) command (commandTier (Text.unpack name))
        | (i, UcmCommand _ command) <- zip [1 :: Int ..] lines,
          name : _ <- [Text.words command]
        ]
      API _ requests ->
        [ Violation (blockLocation n) ("POST " <> url) All
        | PostRequest url _ <- requests
        ]
    blockLocation n = "code block " <> tShow n

renderViolations :: Tier -> [Violation] -> Text
renderViolations allowed violations =
  Text.unlines $
    [ "Nothing was run: `" <> toolName allowed <> "` doesn't allow everything in this transcript.",
      ""
    ]
      ++ [ "  " <> location <> ": `" <> offender <> "` needs `" <> toolName requires <> "`"
         | Violation {location, offender, requires} <- violations
         ]
