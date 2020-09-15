{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.V1.Referent where

import           Unison.Codebase.V1.Reference       (Reference)
import qualified Unison.Codebase.V1.Reference       as R

import Unison.Codebase.V1.ConstructorType (ConstructorType)
import Data.Word (Word64)

-- Slightly odd naming. This is the "referent of term name in the codebase",
-- rather than the target of a Reference.
type Referent = Referent' Reference
pattern Ref :: Reference -> Referent
pattern Ref r = Ref' r
pattern Con :: Reference -> Int -> ConstructorType -> Referent
pattern Con r i t = Con' r i t
{-# COMPLETE Ref, Con #-}

type Id = Referent' R.Id

data Referent' r = Ref' r | Con' r Int ConstructorType
  deriving (Show, Ord, Eq, Functor)

type Pos = Word64
type Size = Word64

isConstructor :: Referent -> Bool
isConstructor Con{} = True
isConstructor _     = False

toTermReference :: Referent -> Maybe Reference
toTermReference = \case
  Ref r -> Just r
  _ -> Nothing

toReference :: Referent -> Reference
toReference = toReference'

toReference' :: Referent' r -> r
toReference' = \case
  Ref' r -> r
  Con' r _i _t -> r
  
fromId :: Id -> Referent
fromId = fmap R.DerivedId  

toTypeReference :: Referent -> Maybe Reference
toTypeReference = \case
  Con r _i _t -> Just r
  _ -> Nothing
