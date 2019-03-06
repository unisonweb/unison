{-# LANGUAGE LambdaCase #-}

module Unison.NamePrinter where

import           Data.String          (IsString, fromString)
import           Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import           Unison.Name          (Name)
import qualified Unison.Name          as Name
import           Unison.ShortHash     (ShortHash)
import qualified Unison.ShortHash     as SH
import           Unison.Util.Pretty   (Pretty)
import qualified Unison.Util.Pretty   as PP

prettyName :: IsString s => Name -> Pretty s
prettyName = PP.text . Name.toText

prettyHashQualified :: IsString s => HashQualified -> Pretty s
prettyHashQualified = PP.text . HQ.toText

prettyShortHash :: IsString s => ShortHash -> Pretty s
prettyShortHash = fromString . SH.toString

styleHashQualified ::
  IsString s => (Pretty s -> Pretty s) -> HashQualified -> Pretty s
styleHashQualified style = \case
  HQ.NameOnly n -> style (prettyName n)
  HQ.HashOnly h -> prettyShortHash $ h
  HQ.HashQualified n h -> PP.group $ style (prettyName n) <> prettyShortHash h
