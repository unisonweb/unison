{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Test.UriParser where

import Data.Functor (void)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.These (These (..))
import EasyTest
import qualified Text.Megaparsec as P
import Unison.Codebase.Editor.RemoteRepo (ReadGitRepo (..), ReadRemoteNamespace (..), ShareCodeserver (..), ShareUserHandle (..), pattern ReadGitRemoteNamespace, pattern ReadShareLooseCode)
import qualified Unison.Codebase.Editor.UriParser as UriParser
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash (..))
import Unison.Core.Project (ProjectBranchName (..), ProjectName (..))
import Unison.NameSegment (NameSegment (..))

test :: Test ()
test = scope "uriparser" . tests $ [testShare, testGit]

gitHelper :: (ReadGitRepo, Maybe ShortCausalHash, Path) -> ReadRemoteNamespace void
gitHelper (repo, sch, path) = ReadRemoteNamespaceGit (ReadGitRemoteNamespace repo sch path)

testShare :: Test ()
testShare =
  scope "share" . tests $
    [ parseAugmented
        ( "unisonweb.base._releases.M4",
          ReadShare'LooseCode (ReadShareLooseCode DefaultCodeserver (ShareUserHandle "unisonweb") (path ["base", "_releases", "M4"]))
        ),
      parseAugmented ("project", ReadShare'ProjectBranch (This (UnsafeProjectName "project"))),
      parseAugmented ("/branch", ReadShare'ProjectBranch (That (UnsafeProjectBranchName "branch"))),
      parseAugmented
        ( "project/branch",
          ReadShare'ProjectBranch (These (UnsafeProjectName "project") (UnsafeProjectBranchName "branch"))
        ),
      expectParseFailure ".unisonweb.base"
    ]

testGit :: Test ()
testGit =
  scope "git" . tests $
    -- Local Protocol
    --  $ git clone /srv/git/project.git
    --  $ git clone /srv/git/project.git[:treeish][:[#hash][.path]]
    [ scope "local-protocol" . tests . map parseAugmented $
        [ ( "git(/srv/git/project.git)",
            gitHelper (ReadGitRepo "/srv/git/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(/srv/git/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "/srv/git/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          ),
          ( "git(srv/git/project.git)",
            gitHelper (ReadGitRepo "srv/git/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(srv/git/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "srv/git/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          )
        ],
      -- File Protocol
      --  $ git clone file:///srv/git/project.git[:treeish][:[#hash][.path]] <- imagined
      scope "file-protocol" . tests . map parseAugmented $
        [ ( "git(file:///srv/git/project.git)",
            gitHelper (ReadGitRepo "file:///srv/git/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(file:///srv/git/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "file:///srv/git/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          ),
          ( "git(file://srv/git/project.git)",
            gitHelper (ReadGitRepo "file://srv/git/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(file://srv/git/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "file://srv/git/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          )
        ],
      -- Smart / Dumb HTTP protocol
      --  $ git clone https://example.com/gitproject.git[:treeish][:[#hash][.path]] <- imagined
      scope "http-protocol" . tests . map parseAugmented $
        [ ( "git(https://example.com/git/project.git)",
            gitHelper (ReadGitRepo "https://example.com/git/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(https://user@example.com/git/project.git:abc)#def.hij.klm]",
            gitHelper (ReadGitRepo "https://user@example.com/git/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          )
        ],
      -- SSH Protocol
      --  $ git clone ssh://[user@]server/project.git[:treeish][:[#hash][.path]]
      scope "ssh-protocol" . tests . map parseAugmented $
        [ ( "git(ssh://git@8.8.8.8:222/user/project.git)",
            gitHelper (ReadGitRepo "ssh://git@8.8.8.8:222/user/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(ssh://git@github.com/user/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "ssh://git@github.com/user/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          )
        ],
      --  $ git clone [user@]server:project.git[:treeish][:[#hash][.path]]
      scope "scp-protocol" . tests . map parseAugmented $
        [ ( "git(git@github.com:user/project.git)",
            gitHelper (ReadGitRepo "git@github.com:user/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(github.com:user/project.git)",
            gitHelper (ReadGitRepo "github.com:user/project.git" Nothing, Nothing, Path.empty)
          ),
          ( "git(git@github.com:user/project.git:abc)#def.hij.klm",
            gitHelper (ReadGitRepo "git@github.com:user/project.git" (Just "abc"), sch "def", path ["hij", "klm"])
          )
        ]
    ]

parseAugmented :: (Text, ReadRemoteNamespace (These ProjectName ProjectBranchName)) -> Test ()
parseAugmented (s, r) = scope (Text.unpack s) $
  case P.parse (UriParser.repoPath <* P.eof) "test case" s of
    Left x -> crash $ P.errorBundlePretty x
    Right x -> expectEqual x r

expectParseFailure :: Text -> Test ()
expectParseFailure s = void . scope (Text.unpack s) . expectLeft . P.parse (UriParser.repoPath <* P.eof) "negative test case" $ s

path :: [Text] -> Path
path = Path . Seq.fromList . fmap NameSegment

sch :: Text -> Maybe ShortCausalHash
sch = Just . ShortCausalHash
