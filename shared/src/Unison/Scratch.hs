{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Unison.Type
import Unison.Node.MemNode
import Unison.Views
import Unison.Dimensions
import Unison.Doc

t :: Type V
t = forall' ["v"] (vectorOf (v' "v") `arrow` vectorOf (v' "v"))
-- t = lit Number `arrow` lit Number -- forall' ["v"] (vectorOf (v' "v") `arrow` vectorOf (v' "v"))

d = type' defaultSymbol t
s = formatText (Width 80) d

main = putStrLn s
