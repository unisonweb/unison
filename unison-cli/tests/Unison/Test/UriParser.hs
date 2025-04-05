module Unison.Test.UriParser where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.These (These (..))
import Data.Void (Void)
import EasyTest
import Text.Megaparsec qualified as P
import Unison.Codebase.Editor.RemoteRepo
  ( ReadRemoteNamespace (..),
  )
import Unison.Codebase.Editor.UriParser qualified as UriParser
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash (..))
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Project (ProjectBranchSpecifier (..))

test :: Test ()
test =
  scope "uriparser" . tests $
    [ parserTests
        "repoPath"
        (UriParser.readRemoteNamespaceParser ProjectBranchSpecifier'Name <* P.eof)
        [ ("project", branchR (This "project")),
          ("/branch", branchR (That "branch")),
          ("project/branch", branchR (These "project" "branch"))
        ]
        [".unisonweb.base"],
      parserTests
        "writeRemoteNamespace"
        (UriParser.writeRemoteNamespace <* P.eof)
        [ ("project", branchW (This "project")),
          ("/branch", branchW (That "branch")),
          ("project/branch", branchW (These "project" "branch"))
        ]
        [ ".unisonweb.base"
        ]
    ]

mkPath :: [Text] -> Path.Path
mkPath = Path.fromList . fmap NameSegment

branchR :: These Text Text -> ReadRemoteNamespace (These ProjectName ProjectBranchName)
branchR =
  ReadShare'ProjectBranch . \case
    This project -> This (UnsafeProjectName project)
    That branch -> That (UnsafeProjectBranchName branch)
    These project branch -> These (UnsafeProjectName project) (UnsafeProjectBranchName branch)

branchW :: These Text Text -> (These ProjectName ProjectBranchName)
branchW =
  \case
    This project -> This (UnsafeProjectName project)
    That branch -> That (UnsafeProjectBranchName branch)
    These project branch -> These (UnsafeProjectName project) (UnsafeProjectBranchName branch)

sch :: Text -> Maybe ShortCausalHash
sch = Just . ShortCausalHash

-- | @parserTests name parser goodCases badCases@ tests @parser@ against each case in @goodCases@ and @badCases@,
-- expecting success or failure, respectively.
parserTests :: (Eq a, Show a) => Text -> P.Parsec Void Text a -> [(Text, a)] -> [Text] -> Test ()
parserTests name parser goodCases badCases =
  scope (Text.unpack name) (tests (map f goodCases ++ map g badCases))
  where
    f (input, expected) =
      scope
        (Text.unpack input)
        case P.parse parser "" input of
          Left err -> crash (P.errorBundlePretty err)
          Right actual -> expectEqual expected actual

    g input =
      scope
        (Text.unpack input)
        case P.parse parser "" input of
          Left _err -> ok
          Right actual -> crash ("Expected parse failure, but got: " ++ show actual)
