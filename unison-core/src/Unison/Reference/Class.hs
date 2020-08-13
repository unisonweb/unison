{-# LANGUAGE OverloadedStrings #-}

module Unison.Reference.Class where

import Data.Text (Text)

class CanBuiltin r where
  builtin :: Text -> r

