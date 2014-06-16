module Unison.Language.Type (Type, hash, hashes) where

import qualified Unison.Syntax.Type as ST
import qualified Unison.Syntax.Hash as H
import qualified Unison.Language.Type.Literal as L

-- | A type in the Unison language
type Type = ST.Type L.Literal ()

-- | Computes the nameless hash of the given type
hash :: Type -> H.Hash
hash t = H.typ hashLit hashConstraint t

-- | Computes the nameless hash of the given types, where
-- the types may have mutual dependencies
hashes :: [Type] -> [H.Hash]
hashes ts = H.types hashLit hashConstraint ts

-- private

hashLit :: L.Literal -> H.Hash
hashLit (L.Hash h) = h
hashLit L.Number = H.zero
hashLit L.String = H.one
hashLit L.Vector = H.two

hashConstraint :: () -> H.Hash
hashConstraint _ = H.zero
