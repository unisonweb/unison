{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Lexer.Ident
  ( Ident (..),
    prettyIdent,
    prettyIdentWithoutHash,
  )
where

import Data.HKD (FFunctor (..))
import Data.Proxy (Proxy (Proxy))
import Data.Sequence.NonEmpty (NESeq)
import qualified Data.Text as Text
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH

-- foo.bar = Ident False ["foo", "bar"] Nothing
-- .foo#hhh = Ident True ["foo"] (Just "hhh")
data Ident f
  = Ident Bool {- is absolute -} (NESeq NameSegment) (f ShortHash)

instance FFunctor Ident where
  ffmap :: (forall x. f x -> g x) -> Ident f -> Ident g
  ffmap f (Ident isAbsolute segments sh) =
    Ident isAbsolute segments (f sh)

deriving instance (forall a. Eq a => Eq (f a)) => Eq (Ident f)

deriving instance Ord (Ident Maybe) -- ahh, why doesn't the quantified constraints version work?

deriving instance (forall a. Show a => Show (f a)) => Show (Ident f)

prettyIdent :: Ident Maybe -> String
prettyIdent (Ident isAbsolute segments maybeHash) =
  (Text.unpack . mconcat)
    [ if isAbsolute then "." else "",
      Text.intercalate "." (map NameSegment.toText (toList segments)),
      maybe "" SH.toText maybeHash
    ]

prettyIdentWithoutHash :: Ident Proxy -> String
prettyIdentWithoutHash (Ident isAbsolute segments Proxy) =
  prettyIdent (Ident isAbsolute segments Nothing)
