{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Control.Error (fromMaybe)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LZ
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Data.OpenApi ( ToSchema(..) )
import qualified Unison.HashQualified as HQ
import Unison.ConstructorType (ConstructorType)
import Unison.Name (Name)
import Unison.Pattern (SeqOp)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty (Width, render)
import Unison.Util.AnnotatedText ( Segment )
import Unison.Util.SyntaxText (SyntaxText')
import qualified Unison.Util.SyntaxText as SyntaxText
import Unison.Var (Var)
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Type (Type)
import Unison.Term (Term)
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.TermPrinter as TermPrinter

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

instance ToJSON Name
deriving instance ToSchema Name

instance ToJSON ShortHash
deriving instance ToSchema ShortHash

instance ToJSON n => ToJSON (HQ.HashQualified n)
deriving instance ToSchema n => ToSchema (HQ.HashQualified n)

instance ToJSON a => ToJSON (Segment a)
instance ToSchema a => ToSchema (Segment a)

instance ToSchema r => ToSchema (Seq r) where
  declareNamedSchema _ = declareNamedSchema (Proxy @[r])

instance ToJSON ConstructorType

deriving instance ToSchema ConstructorType

instance ToJSON SeqOp

deriving instance ToSchema SeqOp

instance ToJSON r => ToJSON (Referent.TermRef r)

deriving instance ToSchema r => ToSchema (Referent.TermRef r)

instance ToJSON r => ToJSON (SyntaxText.Element r)

deriving instance ToSchema r => ToSchema (SyntaxText.Element r)

instance ToJSON r => ToJSON (SyntaxText' r)

deriving instance ToSchema r => ToSchema (SyntaxText' r)

data QueryResult = QueryResult
  { misses :: [HQ.HashQualified Name]
  , hits :: [SR.SearchResult]
  }

formatType
  :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText' ShortHash
formatType ppe w =
  fmap (fmap Reference.toShortHash) . render w . TypePrinter.pretty0 ppe
                                                                     mempty
                                                                     (-1)

formatTerm
  :: Var v
  => PPE.PrettyPrintEnv
  -> Width
  -> Term3 v PrintAnnotation
  -> SyntaxText' ShortHash
formatTerm ppe w =
  fmap (fmap Reference.toShortHash) . render w . TermPrinter.pretty0 ppe mempty

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.fromStrict

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.pack

defaultWidth :: Width
defaultWidth = 80

discard :: Applicative m => a -> m ()
discard = const $ pure ()

mayDefault :: Maybe Width -> Width
mayDefault = fromMaybe defaultWidth

