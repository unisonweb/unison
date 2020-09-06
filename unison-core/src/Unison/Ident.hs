{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Ident
  ( Ident (..),
    Position (..),
    unPosition,
    absolute,
    relative,
    Absolute (..),
    Relative (..),
    prettyIdent,
  )
where

import Control.Lens ((%~), (.~), (^.))
import Data.Proxy (Proxy)
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Text as Text
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

data Ident f g = Ident
  { segments :: f NameSegment,
    hash :: g ShortHash
  }
  deriving stock (Generic)

deriving instance (forall a. Eq a => Eq (f a), forall a. Eq a => Eq (g a)) => Eq (Ident f g)

deriving instance (forall a. Show a => Show (f a), forall a. Show a => Show (g a)) => Show (Ident f g)

-- ahh, why doesn't the quantified constraints version work?
deriving instance Ord (Ident (Position NESeq) Maybe)

data Position f a
  = PosAbsolute (Absolute f a)
  | PosRelative (Relative f a)
  deriving stock (Eq, Ord, Show)

unPosition :: Position f a -> f a
unPosition = \case
  PosAbsolute x -> unAbsolute x
  PosRelative x -> unRelative x

absolute :: f a -> Position f a
absolute =
  PosAbsolute . Absolute

relative :: f a -> Position f a
relative =
  PosRelative . Relative

newtype Absolute f a = Absolute {unAbsolute :: f a}
  deriving stock (Eq, Ord, Show)

newtype Relative f a = Relative {unRelative :: f a}
  deriving stock (Eq, Ord, Show)

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4
-- separate functions if the type class starts to get in the way for some reason.
--
-- FIXME This pretty-printing stuff belongs in some syntax-specific part of the code, not unison-core.
class PrettyIdent f g where
  prettyIdent :: Ident f g -> String

instance Foldable f => PrettyIdent (Absolute f) Maybe where
  prettyIdent :: Ident (Absolute f) Maybe -> String
  prettyIdent id =
    prettyIdent (id & #segments %~ PosAbsolute)

instance Foldable f => PrettyIdent (Position f) Maybe where
  prettyIdent :: Ident (Position f) Maybe -> String
  prettyIdent id =
    (Text.unpack . mconcat)
      [ case id ^. #segments of
          PosAbsolute _ -> "."
          PosRelative _ -> "",
        Text.intercalate "." (map NameSegment.toText (toList (unPosition (id ^. #segments)))),
        maybe "" SH.toText (id ^. #hash)
      ]

instance Foldable f => PrettyIdent (Relative f) Maybe where
  prettyIdent :: Ident (Relative f) Maybe -> String
  prettyIdent id =
    prettyIdent (id & #segments %~ PosRelative)

instance PrettyIdent f Maybe => PrettyIdent f Proxy where
  prettyIdent :: Ident f Proxy -> String
  prettyIdent id =
    prettyIdent (id & #hash .~ Nothing)
