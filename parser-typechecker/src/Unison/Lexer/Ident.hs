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

-- foo.bar = Ident False ["foo", "bar"] Nothing
-- .foo#hhh = Ident True ["foo"] (Just "hhh")
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

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4 separate
-- functions if the type class starts to get in the way for some reason.
class PrettyIdent f g h where
  prettyIdent :: Ident f g h -> String

instance PrettyIdent Identity NESeq Maybe where
  prettyIdent :: Ident Identity NESeq Maybe -> String
  prettyIdent (Ident (Identity isAbsolute) segments maybeHash) =
    (Text.unpack . mconcat)
      [ if isAbsolute then "." else "",
        Text.intercalate "." (map NameSegment.toText (toList segments)),
        maybe "" SH.toText maybeHash
      ]

instance PrettyIdent Identity NESeq Proxy where
  prettyIdent :: Ident Identity NESeq Proxy -> String
  prettyIdent id =
    prettyIdent (id & #hash .~ Nothing)
