module Unison.Project
  ( ProjectName,
    ProjectBranchName,
  )
where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Unison.Prelude
import Witch

-- | The name of a project.
--
-- Convert to and from text with the 'From' and 'TryFrom' instances.
newtype ProjectName
  = ProjectName Text
  deriving stock (Eq, Ord, Show)

instance From ProjectName Text

instance TryFrom Text ProjectName where
  -- project-name            = project-name-start-char project-name-char*
  -- project-name-start-char = alpha | _ | @
  -- project-name-char       = project-name-start-char | num | - | /
  tryFrom = do
    maybeTryFrom \name -> do
      (c, cs) <- Text.uncons name
      guard (isValidStartChar c && Text.all isValidChar cs)
      Just (ProjectName name)
    where
      isValidStartChar :: Char -> Bool
      isValidStartChar c =
        Char.isAlpha c || c == '_' || c == '@'

      isValidChar :: Char -> Bool
      isValidChar c =
        isValidStartChar c || Char.isNumber c || c == '-' || c == '/'

-- | The name of a branch of a project.
--
-- Convert to and from text with the 'From' and 'TryFrom' instances.
newtype ProjectBranchName
  = ProjectBranchName Text
  deriving stock (Eq, Ord, Show)

instance From ProjectBranchName Text

instance TryFrom Text ProjectBranchName where
  -- branch-name            = branch-name-start-char branch-name-char*
  -- branch-name-start-char = alpha | _
  -- branch-name-char       = branch-name-start-char | num | - | . | /
  tryFrom = do
    maybeTryFrom \name -> do
      (c, cs) <- Text.uncons name
      guard (isValidStartChar c && Text.all isValidChar cs)
      Just (ProjectBranchName name)
    where
      isValidStartChar :: Char -> Bool
      isValidStartChar c =
        Char.isAlpha c || c == '_'

      isValidChar :: Char -> Bool
      isValidChar c =
        isValidStartChar c || Char.isNumber c || c == '-' || c == '.' || c == '/'
