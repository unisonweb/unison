{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Unison.PrintError where

import           Data.Foldable
import qualified Data.List.NonEmpty         as Nel
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Sequence              (Seq (..))
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import           Data.String                (fromString)
import qualified Data.Text                  as Text
import qualified Text.Megaparsec            as P
import qualified Unison.ABT                 as ABT
import qualified Unison.Lexer               as L
import           Unison.Parser              (Ann (..), Annotated, ann,
                                             showLineCol)
import qualified Unison.Parser              as Parser
import qualified Unison.Reference           as R
import           Unison.Result              (Note (..))
import           Unison.Type                (AnnotatedType)
import qualified Unison.Type                as Type
import qualified Unison.Typechecker.Context as C
import qualified Unison.Util.AnnotatedText  as AT
import           Unison.Util.ColorText      ()
import qualified Unison.Util.ColorText      as Color
import           Unison.Util.Range          (Range (..))
import           Unison.Var                 (Var, qualifiedName)

data Env = Env { referenceNames   :: Map R.Reference String
               , constructorNames :: Map (R.Reference, Int) String }

data TypeError v loc
  = Mismatch { overallType1 :: C.Type v loc
             , overallType2 :: C.Type v loc
             , leaf1        :: C.Type v loc
             , leaf2        :: C.Type v loc
             , mismatchSite :: loc }
  | Other (C.Note v loc)

renderTypeError :: (Var v, Annotated a)
                => Env
                -> TypeError v a
                -> String
                -> AT.AnnotatedDocument Color.Color
renderTypeError env e src = AT.AnnotatedDocument . Seq.fromList $
  [ (fromString . annotatedToEnglish) (mismatchSite e)
  , " has a type mismatch:\n\n"
  , AT.Blockquote $ AT.markup (fromString src)
                      (Set.fromList $ catMaybes
                        [ (,Color.Color1) <$> rangeForAnnotated (mismatchSite e)
                        , (,Color.Color2) <$> rangeForType (overallType1 e)
                        , (,Color.Color3) <$> rangeForType (overallType2 e)
                        ])
  , "\n"
  , "The two types involved are:\n\n"
  , AT.Text $ styleInOverallType env (overallType1 e) (leaf1 e) Color.Color1
  , "  and\n"
  , AT.Text $ styleInOverallType env (overallType2 e) (leaf2 e) Color.Color2
  ]

renderType :: Var v => Env -> C.Type v loc -> String
renderType _e t = show t

styleInOverallType :: (Var v, Annotated a)
                   => Env
                   -> C.Type v a
                   -> C.Type v a
                   -> Color.Color
                   -> Color.StyledText
styleInOverallType e overallType leafType c =
  if leafType == overallType
  then Color.color c (fromString (prettyType e overallType))
  else case overallType of
    Type.Ref' _  -> fromString (prettyType e overallType)
    _ -> error "todo"
  -- (Color.color c . fromString $ renderType e leafType)
  --   <> " (" <> (fromString . annotatedToEnglish) (ABT.annotation leafType) <> ")"

posToEnglish :: L.Pos -> String
posToEnglish (L.Pos l c) = "Line " ++ show l ++ ", column " ++ show c

annotatedToEnglish :: Annotated a => a -> String
annotatedToEnglish a = case ann a of
  Intrinsic      -> "An intrinsic"
  Ann start _end -> posToEnglish start

rangeForType :: Annotated a => C.Type v a -> Maybe Range
rangeForType = rangeForAnnotated . ABT.annotation

rangeForAnnotated :: Annotated a => a -> Maybe Range
rangeForAnnotated a = case ann a of
  Intrinsic     -> Nothing
  Ann start end -> Just $ Range start end


-- highlightString :: String -> [()]

--
typeErrorFromNote :: C.Note v loc -> TypeError v loc
typeErrorFromNote n@(C.Note (C.TypeMismatch _) path) =
  let
    pathl = toList path
    subtypes = [ (t1, t2) | C.InSubtype t1 t2 <- pathl ]
    terms = pathl >>= \case
      C.InCheck e _         -> [e]
      C.InSynthesizeApp _ e -> [e]
      C.InSynthesize e      -> [e]
      _                     -> []
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

printNoteWithSource :: (Var v, Annotated a, Show a)
                    => Env -> String -> Note v a -> String
printNoteWithSource _env s (Parsing e) = prettyParseError s e
printNoteWithSource env s (Typechecking e) = prettyTypecheckError env s e
printNoteWithSource _env s (InvalidPath path term) =
  "Invalid Path: " ++ show path ++ "\n" ++
    case ann $ ABT.annotation term of
      Intrinsic     -> "  in Intrinsic " ++ show term
      Ann start end -> printPosRange s start end
printNoteWithSource _env s (UnknownSymbol v a) =
  "Unknown symbol `" ++ Text.unpack (qualifiedName v) ++
    case ann a of
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
  where go (C.InSynthesize t :<| _)      = Just $ ABT.annotation t
        go (C.InCheck t _ :<| _)         = Just $ ABT.annotation t
        go (C.InSynthesizeApp _ t :<| _) = Just $ ABT.annotation t
        go (_ :<| t)                     = go t
        go Empty                         = Nothing

prettyType :: Var v => Env -> AnnotatedType v a -> String
prettyType _env t = show t

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
        "\n" ++ printPosRange input (Parser.start loc) (Parser.end loc)
      C.InSubtype t1 t2 :<| p ->
        let (loc1, loc2) = (ann t1, ann t2)
            (pretty1, pretty2) = (prettyType env t1, prettyType env t2)
        in case findTerm p of
          Just t ->
            "\n" ++ showLineCol t ++
            " (highlighted below) had a type mismatch.\n" ++
            "  " ++ pretty1 ++ " (which comes from " ++ showLineCol loc1 ++ ")\n"
            ++ "  " ++ pretty2 ++ " (which comes from " ++ showLineCol loc2 ++ ")"
            ++ printPosRange input (Parser.start (ann t)) (Parser.end (ann t))
          Nothing -> show n
      _ -> show n
    _ -> show n
