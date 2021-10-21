{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Hashing.V2.Referent
  ( Referent,
    pattern Ref,
    pattern Con,
    ConstructorId,
    toReference,
    Unison.Hashing.V2.Referent.fold,
  )
where

import Unison.Referent' ( Referent'(..), toReference' )
import Unison.Hashing.V2.Reference (Reference)
import Unison.ConstructorType (ConstructorType)

-- | Specifies a term.
--
-- Either a term 'Reference', a data constructor, or an effect constructor.
--
-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.
type Referent = Referent' Reference
type ConstructorId = Int
pattern Ref :: Reference -> Referent
pattern Ref r = Ref' r
pattern Con :: Reference -> ConstructorId -> ConstructorType -> Referent
pattern Con r i t = Con' r i t
{-# COMPLETE Ref, Con #-}

toReference :: Referent -> Reference
toReference = toReference'

fold :: (r -> a) -> (r -> ConstructorId -> ConstructorType -> a) -> Referent' r -> a
fold fr fc = \case
  Ref' r -> fr r
  Con' r i ct -> fc r i ct
