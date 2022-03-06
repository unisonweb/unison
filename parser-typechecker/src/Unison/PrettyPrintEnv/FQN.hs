{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.FQN (Imports, Prefix, Suffix, elideFQN) where

import qualified Data.Map as Map
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude

-- Type aliases relating to Fully-Qualified Names, e.g. 'Acme.API.foo'
-- Used primarily by the FQN elision code - see TermPrinter.PrintAnnotation.

-- Note that a Suffix can include dots.
type Suffix = Text

-- Each member of a Prefix list is dot-free.
type Prefix = [Text]

-- Keys are FQNs, values are shorter names which are equivalent, thanks to use
-- statements that are in scope.
type Imports = Map Name Suffix

-- Give the shortened version of an FQN, if there's been a `use` statement for that FQN.
elideFQN :: Imports -> HQ.HashQualified Name -> HQ.HashQualified Name
elideFQN imports hq =
  let hash = HQ.toHash hq
      name' = do
        name <- HQ.toName hq
        let hit = fmap Name.unsafeFromText (Map.lookup name imports)
        -- Cut out the "const id $" to get tracing of FQN elision attempts.
        let t = const id $ trace ("hit: " ++ show hit ++ " finding: " ++ show hq ++ " in imports: " ++ show imports)
        t (pure $ fromMaybe name hit)
   in HQ.fromNameHash name' hash
