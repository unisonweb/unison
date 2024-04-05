{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Version where

import Data.Text (Text)

data Version = Version {gitDescribeWithDate :: Text, gitDescribe :: (GitRef, CommitDate)}

type CommitDate = Text

type GitRef = Text
