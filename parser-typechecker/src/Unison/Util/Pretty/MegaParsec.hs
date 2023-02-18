module Unison.Util.Pretty.MegaParsec where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Void
import qualified Text.Megaparsec as Parser
import Unison.Prelude
import qualified Unison.Util.Pretty as P

prettyPrintParseError :: String -> Parser.ParseErrorBundle Text Void -> P.Pretty P.ColorText
prettyPrintParseError input errBundle =
  let (firstError, sp) = NE.head . fst $ Parser.attachSourcePos Parser.errorOffset (Parser.bundleErrors errBundle) (Parser.bundlePosState errBundle)
   in case firstError of
        Parser.TrivialError _errorOffset ue ee ->
          P.lines
            [ printLocation sp,
              P.newline,
              printTrivial ue ee
            ]
        Parser.FancyError _errorOffset ee ->
          let errors = foldMap (P.string . mappend "\n" . showErrorFancy) ee
           in P.lines
                [ printLocation sp,
                  errors
                ]
  where
    printLocation :: Parser.SourcePos -> P.Pretty P.ColorText
    printLocation sp =
      let col = (Parser.unPos $ Parser.sourceColumn sp) - 1
          row = (Parser.unPos $ Parser.sourceLine sp) - 1
          errorLine = lines input !! row
       in P.lines
            [ P.newline,
              P.string errorLine,
              P.string $ Prelude.replicate col ' ' <> "^-- This is where I gave up."
            ]

    printTrivial :: (Maybe (Parser.ErrorItem Char)) -> (Set (Parser.ErrorItem Char)) -> P.Pretty P.ColorText
    printTrivial ue ee =
      let expected = "I expected " <> foldMap (P.singleQuoted . P.string . showErrorItem) ee
          found = P.string . mappend "I found " . showErrorItem <$> ue
          message = [expected] <> catMaybes [found]
       in P.oxfordCommasWith "." message

showErrorFancy :: (Parser.ShowErrorComponent e) => Parser.ErrorFancy e -> String
showErrorFancy (Parser.ErrorFail msg) = msg
showErrorFancy (Parser.ErrorIndentation ord ref actual) =
  "incorrect indentation (got "
    <> show (Parser.unPos actual)
    <> ", should be "
    <> p
    <> show (Parser.unPos ref)
    <> ")"
  where
    p = case ord of
      LT -> "less than "
      EQ -> "equal to "
      GT -> "greater than "
showErrorFancy (Parser.ErrorCustom a) = Parser.showErrorComponent a

showErrorItem :: Parser.ErrorItem (Parser.Token Text) -> String
showErrorItem (Parser.Tokens ts) = Parser.showTokens (Proxy @Text) ts
showErrorItem (Parser.Label label) = NE.toList label
showErrorItem Parser.EndOfInput = "end of input"
