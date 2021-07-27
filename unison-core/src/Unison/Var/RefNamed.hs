{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}

module Unison.Var.RefNamed where

import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Var (Var)
import qualified Unison.Var as Var

refNamed :: Var v => Reference -> v
refNamed ref = Var.named ("ℍ" <> Reference.toText ref)