{-# LANGUAGE OverloadedLists, OverloadedStrings, TupleSections #-}

module Unison.PrintError where

import Data.Foldable
import Data.Maybe (listToMaybe, catMaybes)
import           Unison.Parser (Ann(..))
import           Unison.Result (Note(..))
import           Unison.Var (Var, qualifiedName)
import Data.Map (Map)
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.Lexer as L
import qualified Unison.Parser as Parser
import Unison.Parser (start, end, ann, showLineCol)
import qualified Unison.Typechecker.Context as C
import qualified Unison.Reference as R
import qualified Unison.Util.ColorText as Color
import Unison.Util.ColorText ()
import Data.Sequence (Seq(..))
import Unison.Type (AnnotatedType)

data Env = Env { referenceNames :: Map R.Reference String
               , constructorNames :: Map (R.Reference, Int) String }

data TypeError v loc
  = Mismatch { overallType1 :: C.Type v loc
             , overallType2 :: C.Type v loc
             , leaf1 :: C.Type v loc
             , leaf2 :: C.Type v loc
             , mismatchSite :: loc }
  | Other (C.Note v loc)

renderTypeError :: Var v => Env -> TypeError v Ann -> String -> Color.Rendered
renderTypeError env e src =
  (fromString . annToEnglish) (mismatchSite e)
    <> " has a type mismatch:\n\n"
    <> (Color.splitAndRenderWithColor 1 $ Color.markup (fromString src)
              (Set.fromList $ catMaybes
                [ (,Color.Color1) <$> rangeForType (overallType1 e)
                , (,Color.Color2) <$> rangeForType (overallType2 e)
                , (,Color.Color3) <$> rangeForAnn (mismatchSite e)
                ]) :: Color.Rendered)
    <> "\n"
    <> "The two types involved are:\n\n"
    <> renderTypePosColor env (leaf1 e) Color.Color1
    <> "  and\n"
    <> renderTypePosColor env (leaf2 e) Color.Color2

renderType :: Var v => Env -> C.Type v loc -> String
renderType _e t = show t

renderTypePosColor :: Var v => Env -> C.Type v Ann -> Color.Color -> Color.Rendered
renderTypePosColor e t c =
  (Color.renderStyleTextWithColor $ Color.color c (fromString $ renderType e t))
  <> " (" <> (fromString . annToEnglish) (ABT.annotation t) <> ")"

posToEnglish :: L.Pos -> String
posToEnglish (L.Pos l c) = "Line " ++ show l ++ ", column " ++ show c

annToEnglish :: Ann -> String
annToEnglish Intrinsic = "An intrinsic"
annToEnglish (Ann start _end) = posToEnglish start

rangeForType :: C.Type v Ann -> Maybe Color.Range
rangeForType = rangeForAnn . ABT.annotation

rangeForAnn :: Ann -> Maybe Color.Range
rangeForAnn Intrinsic = Nothing
rangeForAnn (Ann start end) = Just $ Color.Range start end


-- highlightString :: String -> [()]

--
typeErrorFromNote :: C.Note v loc -> TypeError v loc
typeErrorFromNote n@(C.Note (C.TypeMismatch _) path) =
  let
    pathl = toList path
    subtypes = [ (t1, t2) | C.InSubtype t1 t2 <- pathl ]
    terms = pathl >>= \elem -> case elem of
      C.InCheck e _ -> [e]
      C.InSynthesizeApp _ e -> [e]
      C.InSynthesize e -> [e]
      _ -> []
    firstSubtype = listToMaybe subtypes
    lastSubtype = if null subtypes then Nothing else Just (last subtypes)
    innermostTerm = listToMaybe terms
  in case (firstSubtype, lastSubtype, innermostTerm) of
       (Just (leaf1, leaf2), Just (overall1, overall2), Just mismatchSite) ->
         Mismatch overall1 overall2 leaf1 leaf2 (ABT.annotation mismatchSite)
       _ -> Other n
typeErrorFromNote n@(C.Note _ _) = Other n

env0 :: Env
env0 = Env Map.empty Map.empty

showLexerOutput :: Bool
showLexerOutput = True

printNoteWithSource :: Var v => Env -> String -> Note v Ann -> String
printNoteWithSource _env s (Parsing e) = prettyParseError s e
printNoteWithSource env s (Typechecking e) = prettyTypecheckError env s e
printNoteWithSource _env s (InvalidPath path term) =
  "Invalid Path: " ++ show path ++ "\n" ++
    case ABT.annotation term of
      Intrinsic -> "  in Intrinsic " ++ show term
      Ann start end -> printPosRange s start end
printNoteWithSource _env s (UnknownSymbol v ann) =
  "Unknown symbol `" ++ (Text.unpack $ qualifiedName v) ++
    case ann of
      Intrinsic -> "` (Intrinsic)"
      Ann (L.Pos startLine startCol) _end ->
        -- todo: multi-line ranges
        -- todo: ranges
        "`:\n\n" ++ printArrowsAtPos s startLine startCol
printNoteWithSource _env _s (UnknownReference r) =
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

debugMode :: Bool
debugMode = True

findTerm :: Seq (C.PathElement v loc) -> Maybe loc
findTerm = go
  where go (C.InSynthesize t :<| _) = Just $ ABT.annotation t
        go (C.InCheck t _ :<| _) = Just $ ABT.annotation t
        go (C.InSynthesizeApp _ t :<| _) = Just $ ABT.annotation t
        go (_ :<| t) = go t
        go Empty = Nothing

prettyType :: Var a => Env -> AnnotatedType a b -> String
prettyType _env = show

prettyTypecheckError :: (Var v, Show loc, Parser.Annotated loc)
                     => Env
                     -> String
                     -> C.Note v loc -> String
prettyTypecheckError env input n@(C.Note cause path) =
  case cause of
    C.TypeMismatch _ -> case path of
      C.InCheck term typ :<| _ ->
        let loc = ann term
        in "\n" ++ showLineCol term ++ " had a type mismatch. " ++
        "The highlighted term below is not of type " ++ prettyType env typ ++
        "\n" ++ printPosRange input (start loc) (end loc)
      C.InSubtype t1 t2 :<| p ->
        let (loc1, loc2) = (ann t1, ann t2)
            (pretty1, pretty2) = (prettyType env t1, prettyType env t2)
        in case findTerm p of
          Just t ->
            "\n" ++ showLineCol t ++
            " (highlighted below) had a type mismatch.\n" ++
            "  " ++ pretty1 ++ " (which comes from " ++ showLineCol loc1 ++ ")\n"
            ++ "  " ++ pretty2 ++ " (which comes from " ++ showLineCol loc2 ++ ")"
            ++ printPosRange input (start (ann t)) (end (ann t))
          Nothing -> show n
      _ -> show n
    _ -> show n
