{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.V1.Type where

import qualified Control.Monad.Writer as Writer
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.Codebase.V1.ABT as ABT
import Unison.Codebase.V1.Reference (Reference)
import qualified Unison.Codebase.V1.Reference as Reference
import qualified Unison.Codebase.V1.Type.Kind as K

-- | Base functor for types in the Unison language
data F a
  = Ref Reference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a -- binder like ∀, used to introduce variables that are
      -- bound by outer type signatures, to support scoped type
      -- variables
  deriving (Foldable, Functor, Traversable)

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v a = ABT.Term F v a

dependencies :: Ord v => Type v a -> Set Reference
dependencies t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where
    f t@(Ref r) = Writer.tell [r] $> t
    f t = pure t

intRef, natRef, floatRef, booleanRef, textRef, charRef, vectorRef, bytesRef, effectRef, termLinkRef, typeLinkRef :: Reference
intRef = Reference.Builtin "Int"
natRef = Reference.Builtin "Nat"
floatRef = Reference.Builtin "Float"
booleanRef = Reference.Builtin "Boolean"
textRef = Reference.Builtin "Text"
charRef = Reference.Builtin "Char"
vectorRef = Reference.Builtin "Sequence"
bytesRef = Reference.Builtin "Bytes"
effectRef = Reference.Builtin "Effect"
termLinkRef = Reference.Builtin "Link.Term"
typeLinkRef = Reference.Builtin "Link.Type"
