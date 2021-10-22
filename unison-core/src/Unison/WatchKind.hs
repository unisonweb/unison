{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.WatchKind where

type WatchKind = String

pattern RegularWatch = ""
pattern TestWatch = "test"
