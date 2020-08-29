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
data Ident f g = Ident
  { isAbsolute :: Bool,
    segments :: f NameSegment,
    hash :: g ShortHash
  }
  deriving stock (Generic)

deriving instance (forall a. Eq a => Eq (f a), forall a. Eq a => Eq (g a)) => Eq (Ident f g)

deriving instance Ord (Ident NESeq Maybe) -- ahh, why doesn't the quantified constraints version work?

deriving instance (forall a. Show a => Show (f a), forall a. Show a => Show (g a)) => Show (Ident f g)

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4 separate
-- functions if the type class starts to get in the way for some reason.
class PrettyIdent f g where
  prettyIdent :: Ident f g -> String

instance PrettyIdent NESeq Maybe where
  prettyIdent :: Ident NESeq Maybe -> String
  prettyIdent (Ident isAbsolute segments maybeHash) =
    (Text.unpack . mconcat)
      [ if isAbsolute then "." else "",
        Text.intercalate "." (map NameSegment.toText (toList segments)),
        maybe "" SH.toText maybeHash
      ]

instance PrettyIdent NESeq Proxy where
  prettyIdent :: Ident NESeq Proxy -> String
  prettyIdent id =
    prettyIdent (id & #hash .~ Nothing)
