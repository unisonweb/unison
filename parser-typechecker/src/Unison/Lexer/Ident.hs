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
data Ident f = Ident
  { isAbsolute :: Bool,
    segments :: NESeq NameSegment,
    hash :: f ShortHash
  }
  deriving stock (Generic)

deriving instance (forall a. Eq a => Eq (f a)) => Eq (Ident f)

deriving instance Ord (Ident Maybe) -- ahh, why doesn't the quantified constraints version work?

deriving instance (forall a. Show a => Show (f a)) => Show (Ident f)

-- | Eh, one-off type class so it's not so tedious to render an ident as a string. This could be made into 3-4 separate
-- functions if the type class starts to get in the way for some reason.
class PrettyIdent f where
  prettyIdent :: Ident f -> String

instance PrettyIdent Maybe where
  prettyIdent :: Ident Maybe -> String
  prettyIdent (Ident isAbsolute segments maybeHash) =
    (Text.unpack . mconcat)
      [ if isAbsolute then "." else "",
        Text.intercalate "." (map NameSegment.toText (toList segments)),
        maybe "" SH.toText maybeHash
      ]

instance PrettyIdent Proxy where
  prettyIdent :: Ident Proxy -> String
  prettyIdent id =
    prettyIdent (id & #hash .~ Nothing)
