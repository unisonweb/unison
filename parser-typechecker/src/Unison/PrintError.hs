{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Unison.PrintError where

-- import           Unison.Parser              (showLineCol)
import           Control.Lens               ((<&>))
import           Control.Monad              (join)
import qualified Data.Char                  as Char
import           Data.Foldable
import           Data.List                  (intersperse)
import qualified Data.List.NonEmpty         as Nel
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe, listToMaybe)
import           Data.Sequence              (Seq (..))
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import           Data.String                (IsString, fromString)
import qualified Data.Text                  as Text
import           Data.Void                  (Void)
import qualified Text.Megaparsec            as P
import qualified Unison.ABT                 as ABT
import qualified Unison.Blank               as B
import           Unison.Kind                (Kind)
import qualified Unison.Kind                as Kind
import qualified Unison.Lexer               as L
import           Unison.Parser              (Ann (..), Annotated, ann)
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
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var

data Env = Env { referenceNames   :: Map R.Reference String
               , constructorNames :: Map (R.Reference, Int) String }

env0 :: Env
env0 = Env mempty mempty

data TypeError v loc
  = Mismatch { overallType1 :: C.Type v loc
             , overallType2 :: C.Type v loc
             , leaf1        :: C.Type v loc
             , leaf2        :: C.Type v loc
             , mismatchSite :: loc
             , note         :: C.Note v loc
             }
  | AbilityCheckFailure { ambient                 :: [C.Type v loc]
                        , requested               :: [C.Type v loc]
                        , abilityCheckFailureSite :: loc
                        , note :: C.Note v loc
                        }
  | UnknownTerm { unknownTerm :: v
                , termSite :: loc
                , suggestions :: [C.Suggestion v loc]
                , expectedType :: C.Type v loc
                , note :: C.Note v loc
                }
  | Other (C.Note v loc)

renderTypeError
  :: forall v a
   . (Var v, Annotated a, Ord a, Show a)
  => Env
  -> TypeError v a
  -> String
  -> AT.AnnotatedDocument Color.Style
renderTypeError env e src =
  AT.AnnotatedDocument
    . Seq.fromList
    $ case e of
        Mismatch {..} ->
          [ (fromString . annotatedToEnglish) mismatchSite
            , " has a type mismatch ("
            , AT.Describe Color.ErrorSite
            , " below):\n\n"
            , annotatedAsErrorSite src mismatchSite
            , "\n"
            , "The two types involved are:\n\n"
            , "  "
            , AT.Text $ styleInOverallType env overallType1 leaf1 Color.Type1
            , " ("
            , fromString (Char.toLower <$> annotatedToEnglish leaf1)
            , ", "
            , AT.Describe Color.Type1
            , ")\n"
            , "  "
            , AT.Text $ styleInOverallType env overallType2 leaf2 Color.Type2
            , " ("
            , fromString (Char.toLower <$> annotatedToEnglish leaf2)
            , ", "
            , AT.Describe Color.Type2
            , ")\n"
            , "\n"
            , AT.Blockquote $ AT.markup
              (fromString src)
              (Set.fromList $ catMaybes
                [ -- these are overwriting the colored ranges for some reason?
                    --   (,Color.ForceShow) <$> rangeForAnnotated mismatchSite
                    -- , (,Color.ForceShow) <$> rangeForType overallType1
                    -- , (,Color.ForceShow) <$> rangeForType overallType2
                    -- ,
                  (, Color.Type1) <$> rangeForType leaf1
                , (, Color.Type2) <$> rangeForType leaf2
                ]
              )
            , "\n"
            , "loc debug:"
            , "\n  mismatchSite: "
            , fromString $ annotatedToEnglish mismatchSite
            , "\n  overallType1: "
            , fromString $ annotatedToEnglish overallType1
            , "\n         leaf1: "
            , fromString $ annotatedToEnglish leaf1
            , "\n  overallType2: "
            , fromString $ annotatedToEnglish overallType2
            , "\n         leaf2: "
            , fromString $ annotatedToEnglish leaf2
            , "\n"
            , "note debug:\n"
            ]
            ++ summary note
        AbilityCheckFailure {..} ->
          [ "The expression at "
            , (fromString . annotatedToEnglish) abilityCheckFailureSite
            , " ("
            , AT.Describe Color.ErrorSite
            , " below)"
            , " is requesting\n"
            , "    {"
            , AT.Text $ commas (renderType' env) requested
            , "}"
            , " effects, but this location only has access to\n"
            , "    {"
            , AT.Text $ commas (renderType' env) ambient
            , "}"
            , "\n\n"
            , annotatedAsErrorSite src abilityCheckFailureSite
            ]
            ++ summary note
        UnknownTerm {..} ->
          [ "I'm not sure what "
            , AT.Text . Color.style Color.ErrorSite $ (fromString . show)
              unknownTerm
            , " means at "
            , (fromString . annotatedToEnglish) termSite
            , "\n\n"
            , annotatedAsErrorSite src termSite
            , "\nWhatever it is, it has a type that conforms to "
            , AT.Text . Color.style Color.Type1 $ (renderType' env) expectedType
            , "\n\n"
            ]
            ++ case suggestions of
                 [] -> []
                 suggestions ->
                   [ "I found some terms in scope that have matching names and types. "
                     , "Maybe you meant one of these:\n\n"
                     ]
                     ++  join (
                     (   intersperse ["\n"]
                     (   suggestions
                     <&> \(C.Suggestion name typ) ->
                           [ "  - "
                           , fromString $ Text.unpack name
                           , " : "
                           , AT.Text $ renderType' env typ
                           ])))
        Other note ->
          [ "Sorry, you hit an error we didn't make a nice message for yet."
            , "\n\n"
            , "Here is a summary of the Note:\n"
            ]
            ++ summary note
  where
    maxTermDisplay = 20
    renderTerm e = let s = show e in -- todo: pretty print
      if length s > maxTermDisplay
      then fromString (take maxTermDisplay s <> "...")
      else fromString s
    summary :: C.Note v a -> [AT.Section Color.Style]
    summary note =
      [ "  simple cause:\n"
      , "    "
      ] ++ simpleCause (C.cause note) ++ [ "\n"
      ] ++ case toList (C.path note) of
            [] -> ["  path: (empty)\n"]
            l ->  "  path:\n" : mconcat (simplePath <$> l)
    simplePath :: C.PathElement v a -> [AT.Section Color.Style]
    simplePath e = ["    "] ++ simplePath' e ++ ["\n"]
    simplePath' :: C.PathElement v a -> [AT.Section Color.Style]
    simplePath' = \case
      C.InSynthesize e -> ["InSynthesize e=", renderTerm e]
      C.InSubtype t1 t2 -> ["InSubtype t1="
                           , AT.Text $ renderType' env t1
                           , ", t2="
                           , AT.Text $ renderType' env t2]
      C.InCheck e t ->
        ["InCheck e=", renderTerm e, ","
        ," t=", AT.Text $ renderType' env t]
      C.InInstantiateL v t ->
        ["InInstantiateL v=", AT.Text $ renderVar v
                     ,", t=", AT.Text $ renderType' env t]
      C.InInstantiateR t v ->
        ["InInstantiateR t=", AT.Text $ renderType' env t
                      ," v=", AT.Text $ renderVar v]
      C.InSynthesizeApp t e -> ["InSynthesizeApp"
        ," t=", AT.Text $ renderType' env t
        ,", e=", renderTerm e]
    simpleCause :: C.Cause v a -> [AT.Section Color.Style]
    simpleCause = \case
      C.TypeMismatch c ->
        ["TypeMismatch\n"
        ,"  context:\n"
        ,fromString . init . unlines . (fmap ("  "++)) . lines . show $ c]
      C.IllFormedType c ->
        ["IllFormedType\n"
        ,"  context:\n"
        ,fromString . init . unlines . (fmap ("  "++)) . lines . show $ c]
      C.UnknownSymbol loc v ->
        [ "UnknownSymbol: ", (fromString . show) loc
        , " ", (fromString . show) v, "\n\n"
        , annotatedAsErrorSite src loc
        ]
      C.UnknownTerm loc v suggestions typ ->
        ["UnknownTerm: ", (fromString . show) loc
        , " ", (fromString . show) v, "\n\n"
        , annotatedAsErrorSite src loc
        , "Suggestions: ", (fromString . show) suggestions, "\n\n"
        , "Type: ", (fromString . show) typ
        ]
      C.CompilerBug c -> ["CompilerBug: ", fromString (show c)]
      C.AbilityCheckFailure ambient requested ctx ->
        [ "AbilityCheckFailure: "
        , "ambient={"] ++ (AT.Text . renderType' env <$> ambient) ++
        [ "} requested={"] ++
        (AT.Text . renderType' env <$> requested)
        ++ ["}\n"] ++ [fromString (show ctx)]
      C.EffectConstructorWrongArgCount e a r cid ->
        [ "EffectConstructorWrongArgCount:"
        , "  expected=", (fromString . show) e
        , ", actual=", (fromString . show) a
        , ", reference=", AT.Text (showConstructor' env r cid)
        ]
      C.MalformedEffectBind ctorType ctorResult es ->
        [ "MalformedEffectBind: "
        , "  ctorType=", AT.Text $ renderType env (\_ s -> s) ctorType
        , "  ctorResult=", AT.Text $ renderType env (\_ s -> s) ctorResult
        , "  effects=", fromString $ show es ]
      C.SolvedBlank recorded v t ->
        [ "SolvedBlank: "
        , case recorded of
            B.Placeholder loc s ->
              fromString ("Placeholder " ++ show s ++ " " ++ annotatedToEnglish loc)
            B.Resolve loc s ->
              fromString ("Resolve " ++ show s ++ " "++ annotatedToEnglish loc)
        , " v="
        , (fromString . show) v
        , " t="
        , AT.Text . renderType' env $ t
        ]

-- | renders a type with no special styling
renderType' :: Var v => Env -> Type.AnnotatedType v loc -> AT.AnnotatedText (Maybe a)
renderType' env typ = renderType env (const id) typ

-- | `f` may do some styling based on `loc`.
-- | You can pass `(const id)` if no styling is needed, or call `renderType'`.
renderType :: Var v
           => Env
           -> (loc -> AT.AnnotatedText (Maybe a) -> AT.AnnotatedText (Maybe a))
           -> Type.AnnotatedType v loc
           -> AT.AnnotatedText (Maybe a)
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
    Type.Effects' es -> paren (p >= 3) $ "{" <> commas (go 0) es <> "} "
    Type.Effect' es t -> case es of
      [] -> go p t
      _ -> paren (p >= 3) $ "{" <> commas (go 0) es <> "} " <> go 3 t
    Type.Effect1' e t -> paren (p >= 3) $ "{" <> go 0 e <> "}" <> go 3 t
    Type.ForallsNamed' vs body -> paren (p >= 1) $
      if p == 0 then go 0 body
      else "forall " <> spaces renderVar vs <> " . " <> go 1 body
    Type.Var' v -> renderVar v
    _ -> error $ "pattern match failure in PrintError.renderType " ++ show t
    where go = renderType0 env f

spaces :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
spaces = intercalateMap " "

arrows :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
arrows = intercalateMap " -> "

commas :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
commas = intercalateMap ", "

renderVar :: Var v => v -> AT.AnnotatedText (Maybe a)
renderVar = fromString . Text.unpack . Var.shortName

renderKind :: Kind -> AT.AnnotatedText (Maybe a)
renderKind Kind.Star          = "*"
renderKind (Kind.Arrow k1 k2) = renderKind k1 <> " -> " <> renderKind k2

showRef :: Env -> R.Reference -> String
showRef env r = fromMaybe (show r) (Map.lookup r (referenceNames env))

showRef' :: Env -> R.Reference -> AT.AnnotatedText (Maybe a)
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

rangeForToken :: L.Token a -> Range
rangeForToken t = Range (L.start t) (L.end t)

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
  Intrinsic     -> "an intrinsic"
  Ann start end -> rangeToEnglish $ Range start end

rangeForType :: Annotated a => C.Type v a -> Maybe Range
rangeForType = rangeForAnnotated . ABT.annotation

rangeForAnnotated :: Annotated a => a -> Maybe Range
rangeForAnnotated a = case ann a of
  Intrinsic     -> Nothing
  Ann start end -> Just $ Range start end

typeErrorFromNote :: forall loc v. (Ord loc, Var v) => C.Note v loc -> TypeError v loc
typeErrorFromNote n@(C.Note (C.TypeMismatch ctx) path) =
  let
    pathl = toList path
    subtypes = [ (t1, t2) | C.InSubtype t1 t2 <- pathl ]
    firstSubtype = listToMaybe subtypes
    lastSubtype = if null subtypes then Nothing else Just (last subtypes)
    innermostTerm = C.innermostErrorTerm n
    -- replace any type vars with their solutions before returning
    sub t = C.apply ctx t
  in case (firstSubtype, lastSubtype, innermostTerm) of
       (Just (leaf1, leaf2), Just (overall1, overall2), Just mismatchSite) ->
         Mismatch (sub overall1) (sub overall2)
                  (sub leaf1) (sub leaf2)
                  (ABT.annotation mismatchSite)
                  n
       _ -> Other n
typeErrorFromNote n@(C.Note (C.AbilityCheckFailure amb req _) _) =
  let go :: C.Term v loc -> TypeError v loc
      go e = AbilityCheckFailure amb req (ABT.annotation e) n
  in fromMaybe (Other n) $ go <$> C.innermostErrorTerm n
typeErrorFromNote n@(C.Note (C.UnknownTerm loc v suggs typ) _) =
  UnknownTerm v loc suggs typ n
typeErrorFromNote n@(C.Note _ _) = Other n

showLexerOutput :: Bool
showLexerOutput = False

printNoteWithSourceAsAnsi :: (Var v, Annotated a, Show a, Ord a)
                          => Env -> String -> Note v a -> String
printNoteWithSourceAsAnsi e s n =
  show . Color.renderDocANSI 6 $ printNoteWithSource e s n

printNoteWithSource :: (Var v, Annotated a, Show a, Ord a)
                    => Env
                    -> String
                    -> Note v a
                    -> AT.AnnotatedDocument Color.Style
printNoteWithSource _env s (Parsing e) = prettyParseError s e
printNoteWithSource env s (Typechecking e) = prettyTypecheckError env s e
printNoteWithSource _env s (InvalidPath path term) =
  (fromString $ "Invalid Path: " ++ show path ++ "\n")
  <> AT.sectionToDoc (annotatedAsErrorSite s term)
printNoteWithSource _env s (UnknownSymbol v a) =
  fromString ("Unknown symbol `" ++ Text.unpack (Var.name v) ++ "`\n\n")
  <> AT.sectionToDoc (annotatedAsErrorSite s a)

_printPosRange :: String -> L.Pos -> L.Pos -> String
_printPosRange s (L.Pos startLine startCol) _end =
  -- todo: multi-line ranges
  -- todo: ranges
  _printArrowsAtPos s startLine startCol

_printArrowsAtPos :: String -> Int -> Int -> String
_printArrowsAtPos s line column =
  let lineCaret s i = s ++ if i == line
                           then "\n" ++ columnCaret
                           else ""
      columnCaret = replicate (column - 1) '-' ++ "^"
      source = unlines (uncurry lineCaret <$> lines s `zip` [1..])
  in source

prettyParseError :: forall v . Var v
                 => String
                 -> Parser.Err v
                 -> AT.AnnotatedDocument Color.Style
prettyParseError s = \case
  P.TrivialError sp unexpected expected ->
    fromString (P.parseErrorPretty @_ @Void (P.TrivialError sp unexpected expected))
    <> lexerOutput

  P.FancyError sp fancyErrors ->
    mconcat (AT.AnnotatedDocument . Seq.fromList . go' <$>
      Set.toList fancyErrors) <> dumpSourcePos sp <> lexerOutput
  where
    dumpSourcePos :: Nel.NonEmpty P.SourcePos -> AT.AnnotatedDocument a
    dumpSourcePos sp = AT.AnnotatedDocument . Seq.fromList . Nel.toList $
      (fromString . (\s -> "  " ++ show s ++ "\n") <$> sp)
    go' :: P.ErrorFancy (Parser.Error v)
        -> [AT.Section Color.Style]
    go' (P.ErrorFail s) =
      [ "The parser failed with this message:\n"
      , fromString s ]
    go' (P.ErrorIndentation ordering indent1 indent2) =
      [ "The parser was confused by the indentation.\n"
      , "It was expecting the reference level (", fromString (show indent1)
      , ")\nto be ", fromString (show ordering), " than/to the actual level ("
      , fromString (show indent2), ").\n"
      ]
    go' (P.ErrorCustom e) = go e
    go :: Parser.Error v
       -> [AT.Section Color.Style]
    go (Parser.SignatureNeedsAccompanyingBody tok) =
      [ "You provided a type signature, but I didn't find an accompanying\n"
      , "binding after it.  Could it be a spelling mismatch?\n"
      , tokenAsErrorSite s tok
      ]
     -- we would include the last binding term if we didn't have to have an Ord instance for it
    go (Parser.BlockMustEndWithExpression blockAnn lastBindingAnn) =
      [ "The last line of the block starting at "
      , fromString . (fmap Char.toLower) . annotatedToEnglish $ blockAnn, "\n"
      , "has to be an expression, not a binding/import/etc:"
      , annotatedAsErrorSite s lastBindingAnn
      ]
    go (Parser.EmptyBlock tok) =
      [ "I expected a block after this (", AT.Describe Color.ErrorSite, "),"
      , ", but there wasn't one.  Maybe check your indentation:\n"
      , tokenAsErrorSite s tok
      ]
    go (Parser.UnknownEffectConstructor tok) = unknownConstructor "effect" tok
    go (Parser.UnknownDataConstructor tok) = unknownConstructor "data" tok
    unknownConstructor ::
      String -> L.Token String -> [AT.Section Color.Style]
    unknownConstructor ctorType tok =
      [ "I don't know about any ", fromString ctorType, " constructor named "
      , AT.Text (Color.errorSite . fromString . show $ L.payload tok), ".\n"
      , "Maybe make sure it's correctly spelled and that you've imported it:\n"
      , tokenAsErrorSite s tok
      ]
    lexerOutput :: AT.AnnotatedDocument a
    lexerOutput =
      if showLexerOutput
      then "\nLexer output:\n" <> fromString (L.debugLex' s)
      else mempty

annotatedAsErrorSite :: Annotated a => String -> a -> AT.Section Color.Style
annotatedAsErrorSite s ann =
  showSourceMaybes s [(,Color.ErrorSite) <$> rangeForAnnotated ann]

tokenAsErrorSite :: String -> L.Token a -> AT.Section Color.Style
tokenAsErrorSite s tok = showSource1 s (rangeForToken tok, Color.ErrorSite)

showSourceMaybes :: String -> [Maybe (Range, Color.Style)] -> AT.Section Color.Style
showSourceMaybes source annotations = showSource source $ catMaybes annotations

showSource :: String -> [(Range, Color.Style)] -> AT.Section Color.Style
showSource source annotations = AT.Blockquote $ AT.markup (fromString source) (Set.fromList annotations)

showSource1 :: String -> (Range, Color.Style) -> AT.Section Color.Style
showSource1 source annotation = showSource source [annotation]

debugMode :: Bool
debugMode = True

findTerm :: Seq (C.PathElement v loc) -> Maybe loc
findTerm = go
  where go (C.InSynthesize t :<| _)      = Just $ ABT.annotation t
        go (C.InCheck t _ :<| _)         = Just $ ABT.annotation t
        go (C.InSynthesizeApp _ t :<| _) = Just $ ABT.annotation t
        go (_ :<| t)                     = go t
        go Empty                         = Nothing

prettyTypecheckError :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
                     => Env
                     -> String
                     -> C.Note v loc -> AT.AnnotatedDocument Color.Style
prettyTypecheckError env input n =
  renderTypeError env (typeErrorFromNote n) input

parseErrorToAnsiString :: Var v => String -> Parser.Err v -> String
parseErrorToAnsiString s = show . Color.renderDocANSI 3 . prettyParseError s
