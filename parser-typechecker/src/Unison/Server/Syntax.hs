{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Duplicate of the Unison.Util.SyntaxText module, but we expect these to
-- evolve separately. This is the version which is outward facing
-- to the server frontend.
module Unison.Server.Syntax where

import           Data.Aeson                     ( ToJSON )
import           Data.OpenApi                   ( ToSchema(..) )
import           Unison.Prelude
import qualified Unison.HashQualified           as HashQualified
import           Unison.Pattern                 ( SeqOp(..) )

import           Unison.Util.AnnotatedText      ( AnnotatedText(..)
                                                , Segment(..)
                                                , annotate
                                                , segment
                                                )
import qualified Unison.Util.SyntaxText        as SyntaxText
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import           Data.Proxy                     ( Proxy(..) )

type SyntaxText = AnnotatedText Element

instance ToJSON Element

deriving instance ToSchema Element

instance ToJSON a => ToJSON (Segment a)

deriving instance ToSchema a => ToSchema (Segment a)

instance ToJSON SeqOp

deriving instance ToSchema SeqOp

instance ToJSON SyntaxText

deriving instance ToSchema SyntaxText

instance ToSchema r => ToSchema (Seq r) where
  declareNamedSchema _ = declareNamedSchema (Proxy @[r])

convertElement :: SyntaxText.Element Reference -> Element
convertElement = \case
  SyntaxText.NumericLiteral      -> NumericLiteral
  SyntaxText.TextLiteral         -> TextLiteral
  SyntaxText.BytesLiteral        -> BytesLiteral
  SyntaxText.CharLiteral         -> CharLiteral
  SyntaxText.BooleanLiteral      -> BooleanLiteral
  SyntaxText.Blank               -> Blank
  SyntaxText.Var                 -> Var
  SyntaxText.Referent  r         -> TermReference $ Referent.toText r
  SyntaxText.Reference r         -> TypeReference $ Reference.toText r
  SyntaxText.Op        s         -> Op s
  SyntaxText.Constructor         -> Constructor
  SyntaxText.Request             -> Request
  SyntaxText.AbilityBraces       -> AbilityBraces
  SyntaxText.ControlKeyword      -> ControlKeyword
  SyntaxText.TypeOperator        -> TypeOperator
  SyntaxText.BindingEquals       -> BindingEquals
  SyntaxText.TypeAscriptionColon -> TypeAscriptionColon
  SyntaxText.DataTypeKeyword     -> DataTypeKeyword
  SyntaxText.DataTypeParams      -> DataTypeParams
  SyntaxText.Unit                -> Unit
  SyntaxText.DataTypeModifier    -> DataTypeModifier
  SyntaxText.UseKeyword          -> UseKeyword
  SyntaxText.UsePrefix           -> UsePrefix
  SyntaxText.UseSuffix           -> UseSuffix
  SyntaxText.HashQualifier n     -> HashQualifier (HashQualified.toText n)
  SyntaxText.DelayForceChar      -> DelayForceChar
  SyntaxText.DelimiterChar       -> DelimiterChar
  SyntaxText.Parenthesis         -> Parenthesis
  SyntaxText.LinkKeyword         -> LinkKeyword
  SyntaxText.DocDelimiter        -> DocDelimiter
  SyntaxText.DocKeyword          -> DocKeyword

type UnisonHash = Text
type HashQualifiedName = Text

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element = NumericLiteral
             | TextLiteral
             | BytesLiteral
             | CharLiteral
             | BooleanLiteral
             | Blank
             | Var
             | TypeReference UnisonHash
             | TermReference UnisonHash
             | Op SeqOp
             | Constructor
             | Request
             | AbilityBraces
             -- let|handle|in|where|match|with|cases|->|if|then|else|and|or
             | ControlKeyword
             -- forall|->
             | TypeOperator
             | BindingEquals
             | TypeAscriptionColon
             -- type|ability
             | DataTypeKeyword
             | DataTypeParams
             | Unit
             -- unique
             | DataTypeModifier
             -- `use Foo bar` is keyword, prefix, suffix
             | UseKeyword
             | UsePrefix
             | UseSuffix
             | HashQualifier HashQualifiedName
             | DelayForceChar
             -- ? , ` [ ] @ |
             -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
             -- out characters emitted by Pretty.hs helpers like Pretty.commas.
             | DelimiterChar
             -- ! '
             | Parenthesis
             | LinkKeyword -- `typeLink` and `termLink`
             -- [: :] @[]
             | DocDelimiter
             -- the 'include' in @[include], etc
             | DocKeyword
             deriving (Eq, Ord, Show, Generic)

syntax :: Element -> SyntaxText -> SyntaxText
syntax = annotate

-- Convert a `SyntaxText` to a `String`, ignoring syntax markup
toPlain :: SyntaxText -> String
toPlain (AnnotatedText at) = join (toList $ segment <$> at)

