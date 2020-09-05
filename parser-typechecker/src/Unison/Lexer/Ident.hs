{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Lexer.Ident
  ( Ident (..),
    prettyIdent,
  )
where

import Control.Lens ((.~))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy)
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Text as Text
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

-- | An "identifier" is a generic structure that consists of three parts:
--
--     * Whether it is absolute or relative to the current context (ucm).
--     * A name segment.
--     * A hash.
--
-- Each part is parameterized on a different functor, which allows us to reuse this type to cover many different similar
-- cases.
--
-- For example, an identifier that *may*, *must*, or *must not* be hash-qualified can simply be instantiated with
-- `Maybe`, `Identity`, or `Proxy` in its third argument.
--
-- The name segment functor is typically just `Seq` or `NESeq` (non-empty sequence), but could also be `Proxy` for
-- representing something like "just a hash".
--
-- "Is absolute" has a few variants: `Identity` indicates we don't know in the type system whether the identifier is
-- absolute or relative - we must check at runtime. `Proxy` indicates we don't even track this. One-off, well-named
-- functors isomorphic to `Proxy` like `data Abs a = Abs` and `data Rel a = Rel` could be used to indicate absolute and
-- relative, respectively.
data Ident f g h = Ident
  { isAbsolute :: f Bool,
    segments :: g NameSegment,
    hash :: h ShortHash
  }
  deriving stock (Generic)

deriving instance
  ( forall a. Eq a => Eq (f a),
    forall a. Eq a => Eq (g a),
    forall a. Eq a => Eq (h a)
  ) =>
  Eq (Ident f g h)

-- ahh, why doesn't the quantified constraints version work?
deriving instance Ord (Ident Identity NESeq Maybe)

deriving instance
  ( forall a. Show a => Show (f a),
    forall a. Show a => Show (g a),
    forall a. Show a => Show (h a)
  ) =>
  Show (Ident f g h)

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4
-- separate functions if the type class starts to get in the way for some reason.
class PrettyIdent f g h where
  prettyIdent :: Ident f g h -> String

instance Foldable g => PrettyIdent Identity g Maybe where
  prettyIdent :: Ident Identity g Maybe -> String
  prettyIdent (Ident (Identity isAbsolute) segments maybeHash) =
    (Text.unpack . mconcat)
      [ if isAbsolute then "." else "",
        Text.intercalate "." (map NameSegment.toText (toList segments)),
        maybe "" SH.toText maybeHash
      ]

instance Foldable g => PrettyIdent Identity g Proxy where
  prettyIdent :: Ident Identity g Proxy -> String
  prettyIdent id =
    prettyIdent (id & #hash .~ Nothing)
