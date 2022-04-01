{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Util.Servant.Client where

import Data.Generics.Sum
import Data.Proxy
import qualified Data.SOP as SOP
import Servant.API
import qualified Servant.API.UVerb as UVerb

-- | This typeclass allows us to handle Servant Union responses in a type-safe way.
class HasConstructor typ member where
  injectConstructor :: member -> typ

instance HasConstructor typ member => HasConstructor typ (WithStatus n member) where
  injectConstructor (WithStatus member) = injectConstructor member

instance {-# OVERLAPPABLE #-} AsType member typ => HasConstructor typ member where
  injectConstructor = injectTyped

collectUnion :: forall typ xs. (SOP.All (HasConstructor typ) xs) => Union xs -> typ
collectUnion = UVerb.foldMapUnion (Proxy @(HasConstructor typ)) injectConstructor
