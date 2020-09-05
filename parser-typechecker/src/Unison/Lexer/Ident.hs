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
    Position(..),
    unPosition,
    absolute,
    relative,
    Absolute (..),
    Relative (..),
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

newtype Absolute f a
  = Absolute { unAbsolute :: f a }
  deriving stock (Eq, Ord, Show)

newtype Relative f a
  = Relative { unRelative :: f a }
  deriving stock (Eq, Ord, Show)

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4
-- separate functions if the type class starts to get in the way for some reason.
class PrettyIdent f g where
  prettyIdent :: Ident f g -> String

instance PrettyIdent f g where
  prettyIdent = undefined

-- instance PrettyIdent  Maybe where
--   prettyIdent :: Ident f Maybe -> String
--   prettyIdent (Ident (Identity isAbsolute) segments maybeHash) =
--     (Text.unpack . mconcat)
--       [ if isAbsolute then "." else "",
--         Text.intercalate "." (map NameSegment.toText (toList segments)),
--         maybe "" SH.toText maybeHash
--       ]

-- instance Foldable g => PrettyIdent Identity g Proxy where
--   prettyIdent :: Ident Identity g Proxy -> String
--   prettyIdent id =
--     prettyIdent (id & #hash .~ Nothing)
