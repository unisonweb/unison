{-# LANGUAGE TypeFamilies #-}
module Unison.Codebase.Editor.TermsAndTypes where

import Data.Distributive
import Data.Functor.Rep

data TermOrType = TypeTag | TermTag

data TermsAndTypes a = TermsAndTypes {terms :: a, types :: a}
  deriving (Functor)

instance Applicative TermsAndTypes where
  pure a = TermsAndTypes a a
  TermsAndTypes f g <*> TermsAndTypes a b = TermsAndTypes (f a) (g b)

instance Distributive TermsAndTypes where
  distribute = distributeRep

instance Representable TermsAndTypes where
  type Rep TermsAndTypes = TermOrType
  index tt = \case
    TermTag -> terms tt
    TypeTag -> types tt
  tabulate f = TermsAndTypes {terms = f TermTag, types = f TypeTag}
