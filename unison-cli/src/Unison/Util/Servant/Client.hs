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
--
-- Servant 'UVerb' responses look like this:
--
-- @@
-- UVerb 'POST '[JSON] '[WithStatus 204 (), WithStatus 404 (NeedDependencies Hash), WithStatus 412 HashMismatch]
-- @@
--
-- Each response is tagged with its status code, and each response type can be different.
--
-- We can't pack all the responses into a single type because servant needs the status code at the
-- type-level.
--
-- servant-client treats the return type of a method like this as:
--
-- @@
-- (Union '[WithStatus 204 (), WithStatus 404 (NeedDependencies Hash), WithStatus 412 HashMismatch])
-- @@
--
-- Which is an SOP Union under the hood which we need to unpack.
--
-- We can pattern match on it, but it ends up very complex, e.g.
--
-- @@
-- case result of
--   (SOP.Z (SOP.I (WithStatus a))) -> UpdatePathSuccess
--   SOP.S (SOP.Z (SOP.I (WithStatus a))) -> UpdatePathHashMismatch a
--   SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus a)))) -> UpdatePathMissingDependencies a
-- @@
--
-- And the error messages are pretty atrocious when we mess it up.
--
-- Instead, we can leverage the 'AsType' class from `generic-lens`, which allows us to
-- automatically inject subtypes into the appropriate sum-type constructor for us.
--
-- E.g. if we have:
--
-- @@
-- data UpdatePathResponse
--   = UpdatePathSuccess
--   | UpdatePathHashMismatch HashMismatch
--   | UpdatePathMissingDependencies (NeedDependencies Hash)
-- @@
--
-- Then @injectTyped (hashMismatch :: HashMismatch)@ gives us: @UpdatePathHashMismatch
-- hashMismatch@
-- And @injectTyped ()@ gives us @UpdatePathSuccess@
--
-- We can then use this as a constraint with 'foldMapUnion' to collapse the set of
-- responses down to exactly the UpdatePathResponse type!
--
-- generic-lens will now provide us with really nice error messages if we're missing a
-- constructor for a given response, e.g.:
--
--  • The type UpdatePathResponse does not contain a constructor whose field is of type HashMismatch
--  • In the expression: liftHandlerUnion updatePathHandler
class HasConstructor typ member where
  injectConstructor :: member -> typ

-- | This instance teaches HasConstructor to unpack WithStatus wrappers before
-- using `AsType` to select a constructor.
instance HasConstructor typ member => HasConstructor typ (WithStatus n member) where
  injectConstructor (WithStatus member) = injectConstructor member

instance {-# OVERLAPPABLE #-} AsType member typ => HasConstructor typ member where
  injectConstructor = injectTyped

-- | Helper to collapse a union type into a single type which has a constructor for each
-- value.
--
-- E.g.
--
-- @@
-- data MySum =
--   A One
--   | B Two
--   | C Three
--
-- result :: Union '[One, Two, Three]
-- result = respond Two
--
-- -- collectUnion finds the correct constructor for us.
-- >>> collectUnion @'[One, Two, Three] result
-- B Two
-- @@
collectUnion :: forall typ xs. (SOP.All (HasConstructor typ) xs) => Union xs -> typ
collectUnion = UVerb.foldMapUnion (Proxy @(HasConstructor typ)) injectConstructor
