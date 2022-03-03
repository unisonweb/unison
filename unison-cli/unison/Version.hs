{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Data.Text
import Language.Haskell.TH (Exp (TupE), runIO)
import Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL))
import Shellmet

-- | A formatted descriptor of when and against which commit this unison executable was built
-- E.g. latest-149-g5cef8f851 (built on 2021-10-04)
--      release/M2i (built on 2021-10-05)
gitDescribeWithDate :: String
gitDescribeWithDate =
  let formatDate d = " (built on " <> d <> ")"
      (gitRef, date) = gitDescribe
   in gitRef <> formatDate date

type CommitDate = String

type GitRef = String

-- | Uses Template Haskell to embed a git descriptor of the commit
--   which was used to build the executable.
-- E.g. latest-149-g5cef8f851 (built on 2021-10-04)
--      release/M2i (built on 2021-10-05)
gitDescribe :: (GitRef, CommitDate)
gitDescribe =
  $( runIO $ do
       -- Outputs date of current commit; E.g. 2021-08-06
       let getDate = "git" $| ["show", "-s", "--format=%cs"]
       date <- getDate $? pure ""
       -- Fetches a unique tag-name to represent the current commit.
       -- Uses human-readable names whenever possible.
       -- Marks version with a `'` suffix if building on a dirty worktree.
       let getTag = "git" $| ["describe", "--tags", "--always", "--dirty='"]
       tag <- getTag $? pure "unknown"
       pure (TupE [Just . LitE . StringL . unpack $ tag, Just . LitE . StringL . unpack $ date])
   )
