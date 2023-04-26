module Main where

import qualified Data.Text as Text
import Data.These (These (..))
import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Core.Project
import Unison.Prelude
import Unison.Project ()

main :: IO ()
main =
  withCP65001 $
    (run . tests)
      [ scope "Project" (tests projectTests)
      ]

projectTests :: [Test ()]
projectTests =
  [ scope "project name" do
      let t s = scope (Text.unpack s) (void (expectRight (tryFrom @Text @ProjectName s)))
      t "project"
      t "@user/project",
    scope "project branch name" do
      let t s = scope (Text.unpack s) (void (expectRight (tryFrom @Text @ProjectBranchName s)))
      t "branch"
      t "@user/branch"
      t "releases/1.2.3"
      t "releases/drafts/1.2.3",
    scope "project name, project branch name, or both" do
      let t input expected0 =
            scope (Text.unpack input) $
              expectEqual
                expected
                (either (const Nothing) Just (tryFrom @Text @(These ProjectName ProjectBranchName) input))
            where
              expected =
                case expected0 of
                  This project -> Just (This (UnsafeProjectName project))
                  That branch -> Just (That (UnsafeProjectBranchName branch))
                  These project branch -> Just (These (UnsafeProjectName project) (UnsafeProjectBranchName branch))
      t "project" (This "project")
      t "@user/project" (This "@user/project")
      t "/branch" (That "branch")
      t "/@user/branch" (That "@user/branch")
      t "/releases/1.2.3" (That "releases/1.2.3")
      t "/releases/drafts/1.2.3" (That "releases/drafts/1.2.3")
      t "project/branch" (These "project" "branch")
      t "project/@user/branch" (These "project" "@user/branch")
      t "project/releases/1.2.3" (These "project" "releases/1.2.3")
      t "project/releases/drafts/1.2.3" (These "project" "releases/drafts/1.2.3")
      t "@user/project/branch" (These "@user/project" "branch")
      t "@user/project/@user/branch" (These "@user/project" "@user/branch")
      t "@user/project/releases/1.2.3" (These "@user/project" "releases/1.2.3")
      t "@user/project/releases/drafts/1.2.3" (These "@user/project" "releases/drafts/1.2.3"),
    scope "project branch name with optional project name" do
      let t input project branch =
            scope (Text.unpack input) $
              expectEqual
                (Just (ProjectAndBranch (UnsafeProjectName <$> project) (UnsafeProjectBranchName branch)))
                (either (const Nothing) Just (tryFrom @Text @(ProjectAndBranch (Maybe ProjectName) ProjectBranchName) input))
      t "branch" Nothing "branch"
      t "@user/branch" Nothing "@user/branch"
      t "releases/1.2.3" Nothing "releases/1.2.3"
      t "releases/drafts/1.2.3" Nothing "releases/drafts/1.2.3"
      t "/branch" Nothing "branch"
      t "/@user/branch" Nothing "@user/branch"
      t "/releases/1.2.3" Nothing "releases/1.2.3"
      t "/releases/drafts/1.2.3" Nothing "releases/drafts/1.2.3"
      t "project/branch" (Just "project") "branch"
      t "project/@user/branch" (Just "project") "@user/branch"
      t "project/releases/1.2.3" (Just "project") "releases/1.2.3"
      t "project/releases/drafts/1.2.3" (Just "project") "releases/drafts/1.2.3"
      t "@user/project/branch" (Just "@user/project") "branch"
      t "@user/project/@user/branch" (Just "@user/project") "@user/branch"
      t "@user/project/releases/1.2.3" (Just "@user/project") "releases/1.2.3"
      t "@user/project/releases/drafts/1.2.3" (Just "@user/project") "releases/drafts/1.2.3"
  ]
