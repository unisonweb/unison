{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Hashing.V1.Referent (Referent, pattern Ref, pattern Con) where

import Unison.ConstructorType (ConstructorType)
import Unison.Hashing.V1.Reference (Reference)
import Unison.Referent' (Referent' (..))

-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.
type Referent = Referent' Reference
type ConstructorId = Int

pattern Ref :: Reference -> Referent
pattern Ref r = Ref' r
pattern Con :: Reference -> ConstructorId -> ConstructorType -> Referent
pattern Con r i t = Con' r i t
{-# COMPLETE Ref, Con #-}
