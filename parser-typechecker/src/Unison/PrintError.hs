module Unison.PrintError where

import qualified Data.List.NonEmpty as Nel
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Lexer as L
import           Unison.Result (Note(..))
import qualified Unison.Parser as Parser
import           Unison.Parser (Ann(..))
import           Unison.Var (Var, qualifiedName)

showLexerOutput :: Bool
showLexerOutput = True

printNoteWithSource :: Var v => String -> Note v Ann -> String
printNoteWithSource s (Parsing e) = prettyParseError s e
printNoteWithSource s (Typechecking e) = prettyTypecheckError s e
printNoteWithSource s (InvalidPath path term) =
  "Invalid Path: " ++ show path ++ "\n" ++
    case ABT.annotation term of
      Intrinsic -> "  in Intrinsic " ++ show term
      Ann start end -> printPosRange s start end
printNoteWithSource s (UnknownSymbol v ann) =
  "Unknown symbol `" ++ (Text.unpack $ qualifiedName v) ++
    case ann of
      Intrinsic -> "` (Intrinsic)"
      Ann (L.Pos startLine startCol) _end ->
        -- todo: multi-line ranges
        -- todo: ranges
        "`:\n\n" ++ printArrowsAtPos s startLine startCol
printNoteWithSource _s (UnknownReference r) =
  "Unknown reference: " ++ show r

printPosRange :: String -> L.Pos -> L.Pos -> String
printPosRange s (L.Pos startLine startCol) _end =
  -- todo: multi-line ranges
  -- todo: ranges
  printArrowsAtPos s startLine startCol


printArrowsAtPos :: String -> Int -> Int -> String
printArrowsAtPos s line column =
  let lineCaret s i = s ++ if i == line
                           then "\n" ++ columnCaret
                           else ""
      columnCaret = replicate (column - 1) '-' ++ "^"
      source = unlines (uncurry lineCaret <$> lines s `zip` [1..])
  in source

prettyParseError :: Var v => String -> Parser.Err v  -> String
prettyParseError s e =
  let errorColumn = P.unPos . P.sourceColumn . Nel.head . P.errorPos $ e
      errorLine = P.unPos . P.sourceLine . Nel.head . P.errorPos $ e
  in P.parseErrorPretty e ++ "\n" ++
     printArrowsAtPos s errorLine errorColumn ++
     if showLexerOutput
     then "\nLexer output:\n" ++ L.debugLex' s
     else ""

prettyTypecheckError :: (Var v, Show loc) => String -> Context.Note v loc -> String
prettyTypecheckError _s note = show note -- todo
