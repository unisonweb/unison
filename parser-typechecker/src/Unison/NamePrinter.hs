{-# LANGUAGE LambdaCase #-}

module Unison.NamePrinter where

import           Data.String          (IsString, fromString)
import           Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import           Unison.Name          (Name)
import qualified Unison.Name          as Name
import qualified Unison.Referent      as Referent
import           Unison.Util.Pretty   (Pretty)
import qualified Unison.Util.Pretty   as PP

prettyName :: IsString s => Name -> Pretty s
prettyName = PP.text . Name.toText

prettyHashQualified :: IsString s => HashQualified -> Pretty s
prettyHashQualified = PP.text . HQ.toText

styleHashQualified ::
  IsString s => (Pretty s -> Pretty s) -> HashQualified -> Pretty s
styleHashQualified style = \case
  HQ.NameOnly n -> style (prettyName n)
  HQ.HashOnly r -> fromString . Referent.toString $ r
  HQ.HashQualified n h -> PP.group $ style (prettyName n) <> PP.text h
