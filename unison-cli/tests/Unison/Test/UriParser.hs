module Unison.Test.UriParser where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.These (These (..))
import Data.Void (Void)
import EasyTest
import qualified Text.Megaparsec as P
import Unison.Codebase.Editor.RemoteRepo (ReadGitRepo (..), ReadRemoteNamespace (..), ShareCodeserver (..), ShareUserHandle (..), WriteGitRemoteNamespace (..), WriteGitRepo (..), WriteRemoteNamespace (..), WriteShareRemoteNamespace (..), pattern ReadGitRemoteNamespace, pattern ReadShareLooseCode)
import qualified Unison.Codebase.Editor.UriParser as UriParser
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash (..))
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.NameSegment (NameSegment (..))

test :: Test ()
test =
  scope "uriparser" . tests $
    [ parserTests
        "repoPath"
        (UriParser.repoPath <* P.eof)
        [ ("unisonweb.base._releases.M4", looseR "unisonweb" ["base", "_releases", "M4"]),
          ("project", branchR (This "project")),
          ("/branch", branchR (That "branch")),
          ("project/branch", branchR (These "project" "branch")),
          ("git(/srv/git/project.git)", gitR "/srv/git/project.git" Nothing Nothing []),
          ("git(/srv/git/project.git:abc)#def.hij.klm", gitR "/srv/git/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(srv/git/project.git)", gitR "srv/git/project.git" Nothing Nothing []),
          ("git(srv/git/project.git:abc)#def.hij.klm", gitR "srv/git/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(file:///srv/git/project.git)", gitR "file:///srv/git/project.git" Nothing Nothing []),
          ("git(file:///srv/git/project.git:abc)#def.hij.klm", gitR "file:///srv/git/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(file://srv/git/project.git)", gitR "file://srv/git/project.git" Nothing Nothing []),
          ("git(file://srv/git/project.git:abc)#def.hij.klm", gitR "file://srv/git/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(https://example.com/git/project.git)", gitR "https://example.com/git/project.git" Nothing Nothing []),
          ("git(https://user@example.com/git/project.git:abc)#def.hij.klm", gitR "https://user@example.com/git/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(ssh://git@8.8.8.8:222/user/project.git)", gitR "ssh://git@8.8.8.8:222/user/project.git" Nothing Nothing []),
          ("git(ssh://git@github.com/user/project.git:abc)#def.hij.klm", gitR "ssh://git@github.com/user/project.git" (Just "abc") (sch "def") ["hij", "klm"]),
          ("git(git@github.com:user/project.git)", gitR "git@github.com:user/project.git" Nothing Nothing []),
          ("git(github.com:user/project.git)", gitR "github.com:user/project.git" Nothing Nothing []),
          ("git(git@github.com:user/project.git:abc)#def.hij.klm", gitR "git@github.com:user/project.git" (Just "abc") (sch "def") ["hij", "klm"])
        ]
        [".unisonweb.base"],
      parserTests
        "writeRemoteNamespace"
        (UriParser.writeRemoteNamespace <* P.eof)
        [ ("unisonweb.base._releases.M4", looseW "unisonweb" ["base", "_releases", "M4"]),
          ("project", branchW (This "project")),
          ("/branch", branchW (That "branch")),
          ("project/branch", branchW (These "project" "branch")),
          ("git(/srv/git/project.git)", gitW "/srv/git/project.git" Nothing []),
          ("git(srv/git/project.git)", gitW "srv/git/project.git" Nothing []),
          ("git(file:///srv/git/project.git)", gitW "file:///srv/git/project.git" Nothing []),
          ("git(file://srv/git/project.git)", gitW "file://srv/git/project.git" Nothing []),
          ("git(https://example.com/git/project.git)", gitW "https://example.com/git/project.git" Nothing []),
          ("git(ssh://git@8.8.8.8:222/user/project.git)", gitW "ssh://git@8.8.8.8:222/user/project.git" Nothing []),
          ("git(git@github.com:user/project.git)", gitW "git@github.com:user/project.git" Nothing []),
          ("git(github.com:user/project.git)", gitW "github.com:user/project.git" Nothing [])
        ]
        [ ".unisonweb.base",
          "git(/srv/git/project.git:abc)#def.hij.klm",
          "git(srv/git/project.git:abc)#def.hij.klm",
          "git(file:///srv/git/project.git:abc)#def.hij.klm",
          "git(file://srv/git/project.git:abc)#def.hij.klm",
          "git(https://user@example.com/git/project.git:abc)#def.hij.klm",
          "git(ssh://git@github.com/user/project.git:abc)#def.hij.klm",
          "git(git@github.com:user/project.git:abc)#def.hij.klm"
        ]
    ]

gitR :: Text -> Maybe Text -> Maybe ShortCausalHash -> [NameSegment] -> ReadRemoteNamespace void
gitR url ref sch path = ReadRemoteNamespaceGit (ReadGitRemoteNamespace (ReadGitRepo url ref) sch (Path.fromList path))

gitW :: Text -> Maybe Text -> [NameSegment] -> WriteRemoteNamespace void
gitW url branch path = WriteRemoteNamespaceGit (WriteGitRemoteNamespace (WriteGitRepo url branch) (Path.fromList path))

looseR :: Text -> [NameSegment] -> ReadRemoteNamespace void
looseR user path =
  ReadShare'LooseCode (ReadShareLooseCode DefaultCodeserver (ShareUserHandle user) (Path.fromList path))

looseW :: Text -> [NameSegment] -> WriteRemoteNamespace void
looseW user path =
  WriteRemoteNamespaceShare (WriteShareRemoteNamespace DefaultCodeserver (ShareUserHandle user) (Path.fromList path))

branchR :: These Text Text -> ReadRemoteNamespace (These ProjectName ProjectBranchName)
branchR =
  ReadShare'ProjectBranch . \case
    This project -> This (UnsafeProjectName project)
    That branch -> That (UnsafeProjectBranchName branch)
    These project branch -> These (UnsafeProjectName project) (UnsafeProjectBranchName branch)

branchW :: These Text Text -> WriteRemoteNamespace (These ProjectName ProjectBranchName)
branchW =
  WriteRemoteProjectBranch . \case
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
