{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}

module Unison.PrintError where

import qualified Data.Char                  as Char
import           Data.Foldable
import qualified Data.List.NonEmpty         as Nel
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, listToMaybe, fromMaybe)
import           Data.Sequence              (Seq (..))
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import           Data.String                (IsString, fromString)
import qualified Data.Text                  as Text
import qualified Text.Megaparsec            as P
import qualified Unison.ABT                 as ABT
-- import qualified Unison.Builtin             as Builtin
import qualified Unison.Kind                as Kind
import           Unison.Kind                (Kind)
import qualified Unison.Lexer               as L
import           Unison.Parser              (Ann (..), Annotated, ann)
-- import           Unison.Parser              (showLineCol)
import qualified Unison.Parser              as Parser
import qualified Unison.Reference           as R
import           Unison.Result              (Note (..))
import qualified Unison.Type                as Type
import qualified Unison.Typechecker.Context as C
import qualified Unison.Util.AnnotatedText  as AT
import           Unison.Util.ColorText      (StyledText)
import qualified Unison.Util.ColorText      as Color
import           Unison.Util.Monoid         (intercalateMap)
import           Unison.Util.Range          (Range (..))
import           Unison.Var                 (Var, qualifiedName)

data Env = Env { referenceNames   :: Map R.Reference String
               , constructorNames :: Map (R.Reference, Int) String }

env0 :: Env
env0 = Env mempty mempty

data TypeError v loc
  = Mismatch { overallType1 :: C.Type v loc
             , overallType2 :: C.Type v loc
             , leaf1        :: C.Type v loc
             , leaf2        :: C.Type v loc
             , mismatchSite :: loc }
  | Other (C.Note v loc)

renderTypeError :: (Var v, Annotated a, Eq a, Show a)
                => Env
                -> TypeError v a
                -> String
                -> AT.AnnotatedDocument Color.Style
renderTypeError env e src = case e of
  Mismatch {..} -> AT.AnnotatedDocument . Seq.fromList $
    [ (fromString . annotatedToEnglish) mismatchSite
    , " has a type mismatch (", AT.Describe Color.ErrorSite, " below):\n\n"
    , AT.Blockquote $ AT.markup (fromString src)
                        (Set.fromList $ catMaybes
                          [ (,Color.ErrorSite) <$> rangeForAnnotated mismatchSite
                          , (,Color.Type1) <$> rangeForType overallType1
                          , (,Color.Type2) <$> rangeForType overallType2
                          ])
    , "\n"
    , "The two types involved are:\n\n"
    , AT.Text $ styleInOverallType env overallType1 leaf1 Color.Type1
    , " (", fromString (Char.toLower <$> annotatedToEnglish overallType1)
    , ")\n and\n"
    , AT.Text $ styleInOverallType env overallType2 leaf2 Color.Type2
    , " (" , fromString (Char.toLower <$> annotatedToEnglish overallType2)
    , ")\n\n"
    , "loc debug:"
    , "\n  mismatchSite: ", fromString $ annotatedToEnglish mismatchSite
    , "\n  overallType1: ", fromString $ annotatedToEnglish overallType1
    , "\n         leaf1: ", fromString $ annotatedToEnglish leaf1
    , "\n  overallType2: ", fromString $ annotatedToEnglish overallType2
    , "\n         leaf2: ", fromString $ annotatedToEnglish leaf2
    , "\n"
    ]
  Other note -> fromString . show $ note

renderType :: Var v
           => Env
           -> (loc -> StyledText -> StyledText)
           -> C.Type v loc
           -> StyledText
renderType env f = renderType0 env f (0 :: Int) where
  paren :: (IsString a, Semigroup a) => Bool -> a -> a
  paren test s =
    if test then "(" <> s <> ")" else s
  renderType0 env f p t = f (ABT.annotation t) $ case t of
    Type.Ref' r -> showRef' env r
    Type.Arrows' ts -> paren (p >= 2) $ arrows (go 2) ts
    Type.Ann' t k -> paren True $ go 1 t <> " : " <> renderKind k
    Type.Tuple' ts -> paren True $ commas (go 0) ts
    Type.Apps' f' args -> paren (p >= 3) $ spaces (go 3) (f':args)
    Type.Effect' [] t -> go p t
    Type.Effect' es t -> paren (p >= 3) $ "{" <> commas (go 0) es <> "} " <> go 3 t
    Type.ForallsNamed' vs body -> paren (p >= 1) $
      if p == 0 then go 0 body
      else "forall " <> spaces renderVar vs <> " . " <> go 1 body
    Type.Var' v -> renderVar v
    _ -> error "pattern match failure in PrintError.renderType"
    where go = renderType0 env f
          spaces = intercalateMap " "
          arrows = intercalateMap " -> "
          commas = intercalateMap ", "

renderVar :: Var v => v -> StyledText
renderVar = fromString . Text.unpack . qualifiedName

renderKind :: Kind -> StyledText
renderKind Kind.Star = "*"
renderKind (Kind.Arrow k1 k2) = renderKind k1 <> " -> " <> renderKind k2

showRef :: Env -> R.Reference -> String
showRef env r = fromMaybe (show r) (Map.lookup r (referenceNames env))

showRef' :: Env -> R.Reference -> StyledText
showRef' e r = fromString $ showRef e r

-- todo: do something different/better if cid not found
showConstructor :: Env -> R.Reference -> Int -> String
showConstructor env r cid =
  fromMaybe (showRef env r ++ "/" ++ show cid)
            (Map.lookup (r,cid) (constructorNames env))

showConstructor' :: Env -> R.Reference -> Int -> StyledText
showConstructor' env r cid = fromString $ showConstructor env r cid


styleInOverallType :: (Var v, Annotated a, Eq a)
                   => Env
                   -> C.Type v a
                   -> C.Type v a
                   -> Color.Style
                   -> StyledText
styleInOverallType e overallType leafType c =
  renderType e f overallType
    where f loc s = if loc == ABT.annotation leafType then Color.style c s else s

_posToEnglish :: L.Pos -> String
_posToEnglish (L.Pos l c) = "Line " ++ show l ++ ", Column " ++ show c

rangeToEnglish :: Range -> String
rangeToEnglish (Range (L.Pos l c) (L.Pos l' c')) =
  if l == l'
    then if c == c'
      then "Line " ++ show l ++ ", column " ++ show c
      else "Line " ++ show l ++ ", columns " ++ show c ++ "-" ++ show c'
    else "Line " ++ show l ++ ", column " ++ show c ++ " through " ++
         "line " ++ show l' ++ ", column " ++ show c'

annotatedToEnglish :: Annotated a => a -> String
annotatedToEnglish a = case ann a of
  Intrinsic      -> "an intrinsic"
  Ann start end -> rangeToEnglish $ Range start end

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

showLexerOutput :: Bool
showLexerOutput = True

printNoteWithSource :: (Var v, Annotated a, Show a, Eq a)
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

prettyTypecheckError :: (Var v, Eq loc, Show loc, Parser.Annotated loc)
                     => Env
                     -> String
                     -> C.Note v loc -> String
prettyTypecheckError env input n =
  show . Color.renderDocANSI 3 $
    (renderTypeError env (typeErrorFromNote n) input)
  -- case cause of
  --   C.TypeMismatch _ -> case path of
  --     C.InCheck term typ :<| _ ->
  --       let loc = ann term
  --       in "\n" ++ showLineCol term ++ " had a type mismatch. " ++
  --       "The highlighted term below is not of type " ++ prettyType env typ ++
  --       "\n" ++ printPosRange input (Parser.start loc) (Parser.end loc)
  --     C.InSubtype t1 t2 :<| p ->
  --       let (loc1, loc2) = (ann t1, ann t2)
  --           (pretty1, pretty2) = (prettyType env t1, prettyType env t2)
  --       in case findTerm p of
  --         Just t ->
  --           "\n" ++ showLineCol t ++
  --           " (highlighted below) had a type mismatch.\n" ++
  --           "  " ++ pretty1 ++ " (which comes from " ++ showLineCol loc1 ++ ")\n"
  --           ++ "  " ++ pretty2 ++ " (which comes from " ++ showLineCol loc2 ++ ")"
  --           ++ printPosRange input (Parser.start (ann t)) (Parser.end (ann t))
  --         Nothing -> show n
  --     _ -> show n
  --   _ -> show n
