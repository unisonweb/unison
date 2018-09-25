{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}


module Unison.PrintError where

-- import           Unison.Parser              (showLineCol)
import qualified Data.Char                  as Char
import           Data.Foldable
import qualified Data.List.NonEmpty         as Nel
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
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
import qualified Unison.Settings            as Settings
import qualified Unison.Type                as Type
import qualified Unison.Typechecker.Context as C
import           Unison.Typechecker.TypeError
import qualified Unison.TypeVar             as TypeVar
import qualified Unison.Util.AnnotatedText  as AT
import           Unison.Util.ColorText      (StyledText)
import qualified Unison.Util.ColorText      as Color
import           Unison.Util.Monoid         (intercalateMap)
-- import           Unison.Util.Monoid         (whenM)
import           Unison.Util.Range          (Range (..))
import           Unison.Var                 (Var)
import qualified Unison.Var                 as Var
import Debug.Trace

data Env = Env { referenceNames   :: Map R.Reference String
               , constructorNames :: Map (R.Reference, Int) String }

env0 :: Env
env0 = Env mempty mempty

mustBeBool :: (Var v, Annotated a, Eq a) =>
              [AT.Section Color.Style]
              -> Env
              -> String
              -> a
              -> Type.AnnotatedType v a
              -> [AT.Section Color.Style]
mustBeBool initial env src mismatchSite mismatchedType =
  initial ++
  [ " ", AT.Text $ Color.type1 "Boolean", ", but this one is "
  , AT.Text . Color.type2 . renderType' env $ mismatchedType
  , ":\n\n"
  , showSourceMaybes src [siteS]
  ] ++ fromOverHere' src [typeS] [siteS]
  where siteS = styleAnnotated Color.Type2 mismatchSite
        typeS = styleAnnotated Color.Type2 mismatchedType

fromOverHere' :: Ord a
              => String
              -> [Maybe (Range, a)]
              -> [Maybe (Range, a)]
              -> [AT.Section a]
fromOverHere' s spots0 removing =
  fromOverHere s (catMaybes spots0) (catMaybes removing)

fromOverHere :: Ord a
             => String
             -> [(Range, a)]
             -> [(Range, a)]
             -> [AT.Section a]
fromOverHere src spots0 removing =
  let spots = toList $ Set.fromList spots0 Set.\\ Set.fromList removing
  in case length spots of
    0 -> mempty
    1 -> [ "\n  from right here:\n\n"
         , showSource src spots
         ]
    _ -> [ "\n  from these spots, respectively:\n\n"
         , showSource src spots
         ]

showTypeWithProvenance :: (Var v, Annotated a, Ord style)
  => Env -> String -> style -> Type.AnnotatedType v a -> [AT.Section style]
showTypeWithProvenance env src color typ =
  [AT.Text . Color.style color $ renderType' env typ] ++ [".\n"] ++
  fromOverHere' src [styleAnnotated color typ] []

styleAnnotated :: Annotated a => sty -> a -> Maybe (Range,sty)
styleAnnotated sty a = (,sty) <$> rangeForAnnotated a

mustBeType :: (Var v, Annotated a, Eq a) =>
              [AT.Section Color.Style]
              -> Env
              -> String
              -> a
              -> a
              -> Type.AnnotatedType v a
              -> Type.AnnotatedType v a
              -> [AT.Section Color.Style]
mustBeType initial env src expectedLoc mismatchSite expectedType mismatchedType =
  initial ++
  [ "  Here, one is "
  , AT.Text $ Color.type1 . renderType' env $ expectedType
  , " and another is "
  , AT.Text $ Color.type2 . renderType' env $ mismatchedType, ":\n\n"
  , showSourceMaybes src [mismatchSiteS, expectedLocS]
  ] ++ fromOverHere' src [expectedTypeS, mismatchedTypeS]
                         [mismatchSiteS, expectedLocS]
  where mismatchedTypeS = styleAnnotated Color.Type2 mismatchedType
        mismatchSiteS   = styleAnnotated Color.Type2 mismatchSite
        expectedTypeS   = styleAnnotated Color.Type1 expectedType
        expectedLocS    = styleAnnotated Color.Type1 expectedLoc

renderTypeError :: forall v a. (Var v, Annotated a, Ord a, Show a)
                => Env
                -> TypeError v a
                -> String
                -> AT.AnnotatedDocument Color.Style
renderTypeError env e src = AT.AnnotatedDocument . Seq.fromList $ case e of
  BooleanMismatch {..} ->
    mustBeBool which env src mismatchSite foundType
    ++
    (debugNoteLoc
      [ "loc debug:"
      , "\n  mismatchSite: ", annotatedToEnglish mismatchSite
      , "\n     foundType: ", annotatedToEnglish foundType
      , "\n"
      ])
    ++ debugSummary note
    where which =
            case getBooleanMismatch of
              CondMismatch ->
                [ "The condition for an ", AT.Text . Color.errorSite $ "if"
                , "-expression has to be"]
              AndMismatch ->
                [ "The arguments to ", AT.Text . Color.errorSite $ "and"
                , " have to be"]
              OrMismatch ->
                [ "The arguments to ", AT.Text . Color.errorSite $ "or"
                , " have to be"]
              GuardMismatch ->
                [ "The guard expression for a ", AT.Text . Color.errorSite $ "case"
                , " has to be"]

  ExistentialMismatch {..} ->
    mustBeType which env src expectedLoc mismatchSite expectedType foundType
    ++
    (debugNoteLoc [ "\nloc debug:"
    , "\n  mismatchSite: ", annotatedToEnglish mismatchSite
    , "\n     foundType: ", annotatedToEnglish foundType
    , "\n  expectedType: ", annotatedToEnglish expectedType
    , "\n   expectedLoc: ", annotatedToEnglish expectedLoc
    -- , "\n      (should be the location of the first case body)"
    , "\n"
    ])
    ++ debugSummary note
    where which =
            case getExistentialMismatch of
              IfBody ->
                [ "The ", AT.Text . Color.errorSite $ "else"
                , " clause of an ", AT.Text . Color.errorSite $ "if"
                , " expression needs to have the same type as the "
                , AT.Text . Color.errorSite $ "then", " clause."]
              VectorBody ->
                [ "The elements of a vector all need to have the same type."]
              CaseBody ->
                [ "Each case of a ", AT.Text . Color.errorSite $ "case"
                , "/", AT.Text . Color.errorSite $ "of", " expression "
                , "need to have the same type."]
  NotFunctionApplication {..} ->
    [ "This looks like a function call, but with a "
    , AT.Text $ Color.type1 . renderType' env $ ft
    , " where the function should be.  Are you missing an operator?\n\n"
    , annotatedAsStyle Color.Type1 src f
    ] ++ debugSummary note
  FunctionApplication {..} ->
    let fte = Type.ungeneralizeEffects ft
        fteFreeVars = Set.map TypeVar.underlying $ ABT.freeVars fte
        showVar (v,_t) = Set.member v fteFreeVars
        solvedVars' = filter showVar solvedVars
    in
      [ "The ", ordinal argNum, " argument to the function "
      , AT.Text . Color.errorSite . renderTerm $ f
      , " is "
      , AT.Text $ Color.type2 . renderType' env $ foundType, ", "
      , "but I was expecting "
                  , AT.Text $ Color.type1 . renderType' env $ expectedType
      , ":\n\n"
      , showSourceMaybes src
        [ (,Color.Type1)     <$> rangeForAnnotated expectedType
        , (,Color.Type2)     <$> rangeForAnnotated foundType
        , (,Color.Type2)     <$> rangeForAnnotated arg
        , (,Color.ErrorSite) <$> rangeForAnnotated f
        ]
      ]
      ++ case solvedVars' of
        _ : _ ->
          let go :: (v, C.Type v a) -> [AT.Section Color.Style]
              go (v,t) =
               [ " ", renderVar v
               , " = ", AT.Text $ Color.errorSite $ renderType' env t
               , ", from here:\n\n"
               , showSourceMaybes src [(,Color.ErrorSite) <$> rangeForAnnotated t]
               , "\n"
               ]
          in
            [ "\n"
            , "because the function has type"
            , "\n\n"
            , "  "
            , AT.Text $ renderType' env fte
            , "\n\n"
            , "where:"
            , "\n\n"
            ] ++ (solvedVars' >>= go)
        [] -> []
      ++ (debugNoteLoc
          [ "\nloc debug:"
          , AT.Text $ Color.errorSite "\n             f: ", annotatedToEnglish f
          , AT.Text $ Color.type2     "\n     foundType: ", annotatedToEnglish foundType
          , AT.Text $ Color.type1     "\n  expectedType: ", annotatedToEnglish expectedType
          -- , "\n   expectedLoc: ", annotatedToEnglish expectedLoc
          ])
      ++ debugSummary note
  Mismatch {..} ->
    -- [ annotatedToEnglish mismatchSite
    -- , " has a type mismatch (", AT.Describe Color.ErrorSite, " below):\n\n"
    -- , annotatedAsErrorSite src mismatchSite
    -- , "\n"
    -- , "The two types involved are:\n\n"
    -- , "  ", AT.Text $ styleInOverallType env foundType foundLeaf Color.Type1
    -- , " (", fromString (Char.toLower <$> annotatedToEnglish foundLeaf)
    --       , ", ", AT.Describe Color.Type1, ")\n"
    -- , "  ", AT.Text $ styleInOverallType env expectedType expectedLeaf Color.Type2
    -- , " (", fromString (Char.toLower <$> annotatedToEnglish expectedLeaf)
    --       , ", ", AT.Describe Color.Type2, ")\n"
    -- , "\n"
    [ "I found a value of type "
    , AT.Text $ Color.type1 . renderType' env $ foundLeaf
    , " where I expected to find one of type "
    , AT.Text $ Color.type2 . renderType' env $ expectedLeaf
    , ":\n\n"
    , showSourceMaybes src
        [ -- these are overwriting the colored ranges for some reason?
        --   (,Color.ForceShow) <$> rangeForAnnotated mismatchSite
        -- , (,Color.ForceShow) <$> rangeForType foundType
        -- , (,Color.ForceShow) <$> rangeForType expectedType
        -- ,
          (,Color.Type1) <$> rangeForAnnotated mismatchSite
        , (,Color.Type2) <$> rangeForAnnotated expectedLeaf
        ]
    ]
    ++ fromOverHere' src [styleAnnotated Color.Type1 foundLeaf]
                         [styleAnnotated Color.Type1 mismatchSite]
    ++ debugNoteLoc
    [ "\nloc debug:"
    , "\n  mismatchSite: ", annotatedToEnglish mismatchSite
    , "\n     foundType: ", annotatedToEnglish foundType
    , "\n     foundLeaf: ", annotatedToEnglish foundLeaf
    , "\n  expectedType: ", annotatedToEnglish expectedType
    , "\n  expectedLeaf: ", annotatedToEnglish expectedLeaf
    , "\n"
    ] ++ debugSummary note
  AbilityCheckFailure {..} ->
    [ "The expression "
    , AT.Describe Color.ErrorSite
    , " "
    ]
    ++ case toList requested of
      [] -> error "unpossible"
      [e] -> ["needs the {", AT.Text $ renderType' env e, "} ability,"]
      requested -> [ " needs these abilities: {"
            , AT.Text $ commas (renderType' env) requested
            , "},"]
    ++
    [ " "
    , "but "
    ]
    ++ case toList ambient of
      [] -> ["this location does not have access to any abilities."]
      [e] -> ["this location only has access to the {"
             , AT.Text $ renderType' env e, "} ability,"]
      ambient -> ["this location only has access to these abilities: "
                 , "{", AT.Text $ commas (renderType' env) ambient, "}"]
    ++
    [ "\n\n"
    , annotatedAsErrorSite src abilityCheckFailureSite
    ] ++ debugSummary note
  UnknownType {..} ->
    [ "I don't know about the type "
    , AT.Text . Color.style Color.ErrorSite $ renderVar unknownTypeV
    , ".  Make sure it's imported and spelled correctly:\n\n"
    , annotatedAsErrorSite src typeSite
    ]
  UnknownTerm {..} | Type.isArrow expectedType && Var.isKind Var.askInfo unknownTermV ->
    let Type.Arrow' i o = case expectedType of
          Type.ForallsNamed' _ body -> body
          _ -> expectedType
    in [ "Here's what I know about the expression at "
       , annotatedToEnglish termSite
       , ":\n\n"
       , annotatedAsErrorSite src termSite
       , "\n"
       , "Its type is: ", AT.Text . Color.style Color.ErrorSite $ renderType' env (Type.ungeneralizeEffects i), ".\n\n" ] ++
       case o of
         Type.Existential' _ _ -> ["It can be replaced with a value of any type.\n"]
         _ -> [
           "A well-typed replacement must conform to: "
          , AT.Text . Color.style Color.Type2 $
              renderType' env (Type.ungeneralizeEffects o)
          , ".\n"]
  UnknownTerm {..} | Var.isKind Var.missingResult unknownTermV ->
    [ "I found a block that ends with a binding instead of an expression at "
    , annotatedToEnglish termSite
    , ":\n\n"
    , annotatedAsErrorSite src termSite, "\n" ] ++
      case expectedType of
        Type.Existential' _ _ ->
          ["To complete the block, add an expression after this binding.\n\n"]
        _ -> [ "Based on the context, I'm expecting an expression of type "
             , AT.Text . Color.style Color.Type1 $ (renderType' env) expectedType
             , " after this binding. \n\n" ]
  UnknownTerm {..} ->
    [ "I'm not sure what "
    , AT.Text . Color.style Color.ErrorSite $ (fromString . show) unknownTermV
    , " means at "
    , annotatedToEnglish termSite
    , "\n\n"
    , annotatedAsErrorSite src termSite ] ++
      case expectedType of
        Type.Existential' _ _ -> ["\nThere are no constraints on its type."]
        _ -> ["\nWhatever it is, it has a type that conforms to "
             , AT.Text . Color.style Color.Type1 $ (renderType' env) expectedType
             , ".\n"]
             -- ++ showTypeWithProvenance env src Color.Type1 expectedType
    ++ case suggestions of
      [] -> []
      suggestions ->
        [ "I found some terms in scope that have matching names and types. "
        , "Maybe you meant one of these:\n\n"
        ]
        ++ intercalateMap (pure "\n") formatSuggestion suggestions
      where
        formatSuggestion :: C.Suggestion v loc -> [AT.Section Color.Style]
        formatSuggestion (C.Suggestion name typ) =
          [ "  - ", fromString $ Text.unpack name
          , " : ", AT.Text $ renderType' env typ
          ]
  Other note ->
    [ "Sorry, you hit an error we didn't make a nice message for yet.\n\n"
    , "Here is a summary of the Note:\n"
    ] ++ summary note

  where
    maxTermDisplay = 20
    ordinal :: (IsString a') => Int -> a'
    ordinal n = fromString $ show n ++ case last (show n) of
      '1' -> "st"
      '2' -> "nd"
      '3' -> "rd"
      _ -> "th"
    renderTerm (ABT.Var' v) | Settings.demoHideVarNumber = fromString (Text.unpack $ Var.name v)
    renderTerm e = let s = show e in -- todo: pretty print
      if length s > maxTermDisplay
      then fromString (take maxTermDisplay s <> "...")
      else fromString s
    debugNoteLoc a = if Settings.debugNoteLoc then a else []
    debugSummary note = if Settings.debugNoteSummary then summary note else []
    summary :: C.Note v a -> [AT.Section Color.Style]
    summary note =
      [ "\n"
      , "  simple cause:\n"
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
      C.InSynthesizeApp t e n ->
        ["InSynthesizeApp t=", AT.Text $ renderType' env t
                      ,", e=", renderTerm e
                      ,", n=", fromString $ show n]
      C.InFunctionCall vs f ft es ->
        ["InFunctionCall vs=[", AT.Text $ commas renderVar vs, "]"
                       ,", f=", AT.Text $ renderTerm f
                     ,", ft=", AT.Text $ renderType' env ft
                    ,", es=[", AT.Text $ commas renderTerm es, "]"]
      C.InIfCond -> ["InIfCond"]
      C.InIfBody loc ->
        ["InIfBody thenBody=", annotatedToEnglish loc]
      C.InAndApp -> ["InAndApp"]
      C.InOrApp -> ["InOrApp"]
      C.InVectorApp loc ->
        ["InVectorApp firstTerm=", annotatedToEnglish loc]
      C.InMatch loc ->
        ["InMatch firstBody=", annotatedToEnglish loc]
      C.InMatchGuard -> ["InMatchGuard"]
      C.InMatchBody -> ["InMatchBody"]
    simpleCause :: C.Cause v a -> [AT.Section Color.Style]
    simpleCause = \case
      C.TypeMismatch c ->
        [ "TypeMismatch\n"
        , "  context:\n"
        , AT.Text $ renderContext env c]
      C.IllFormedType c ->
        ["IllFormedType\n"
        ,"  context:\n"
        , AT.Text $ renderContext env c]
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
        , "ambient={"]     ++ [AT.Text . commas (renderType' env) $ ambient] ++
        [ "} requested={"] ++ [AT.Text . commas (renderType' env) $ requested]
        ++ ["}\n"] ++ [fromString (show ctx)]
      C.EffectConstructorWrongArgCount e a r cid ->
        [ "EffectConstructorWrongArgCount:"
        , "  expected=", (fromString . show) e
        , ", actual=", (fromString . show) a
        , ", reference=", showConstructor env r cid
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
      C.PatternArityMismatch loc typ args ->
        [ "PatternArityMismatch:"
        , "  loc=", annotatedToEnglish loc
        , "  typ=", AT.Text . renderType' env $ typ
        , "  args=", fromString $ show args
        ]

renderContext :: (Var v, Ord a) => Env -> C.Context v a -> AT.AnnotatedText (Maybe b)
renderContext env ctx@(C.Context es) =
  "  Γ\n    " <> intercalateMap "\n    " (showElem ctx . fst) (reverse es)
  where
    shortName :: (Var v, IsString a) => v -> a
    shortName = fromString . Text.unpack . Var.shortName
    showElem :: (Var v, Ord a) => C.Context v a -> C.Element v a -> AT.AnnotatedText (Maybe b)
    showElem _ctx (C.Var v) = case v of
      TypeVar.Universal x -> "@" <> renderVar x
      TypeVar.Existential _ x -> "'" <> renderVar x
    showElem ctx (C.Solved _ v (Type.Monotype t)) =
      "'" <> shortName v <> " = " <> renderType' env (C.apply ctx t)
    showElem ctx (C.Ann v t) =
      shortName v <> " : " <> renderType' env (C.apply ctx t)
    showElem _ (C.Marker v) =
      "|" <> shortName v <> "|"

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
    Type.Ref' r -> showRef env r
    Type.Arrow' i (Type.Effect1' e o) ->
      paren (p >= 2) $ go 2 i <> " ->{" <> go 1 e <> "} " <> go 1 o
    Type.Arrow' i o ->
      paren (p >= 2) $ go 2 i <> " -> " <> go 1 o
    Type.Ann' t k -> paren True $ go 1 t <> " : " <> renderKind k
    Type.Tuple' ts -> paren True $ commas (go 0) ts
    Type.Apps' (Type.Ref' (R.Builtin "Sequence")) [arg] ->
      "[" <> go 0 arg <> "]"
    Type.Apps' f' args -> paren (p >= 3) $ spaces (go 3) (f':args)
    Type.Effects' es -> commas (go 0) es
    Type.Effect' es t -> case es of
      [] -> go p t
      _ -> "{" <> commas (go 0) es <> "} " <> go 3 t
    Type.Effect1' e t -> paren (p >= 3) $ "{" <> go 0 e <> "}" <> go 3 t
    Type.ForallsNamed' vs body -> paren (p >= 1) $
--      if p == 0 then go 0 body
      if not Settings.debugRevealForalls then go 0 body
      else "forall " <> spaces renderVar vs <> " . " <> go 1 body
    Type.Var' v -> renderVar v
    _ -> error $ "pattern match failure in PrintError.renderType " ++ show t
    where go = renderType0 env f

spaces :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
spaces = intercalateMap " "

arrows :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
arrows = intercalateMap " ->"

commas :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
commas = intercalateMap ", "

renderVar :: (IsString a, Var v) => v -> a
renderVar = fromString . Text.unpack .
              (if Settings.demoHideVarNumber
                then Var.name
                else Var.shortName)

renderVar' :: (Var v, Annotated a)
           => Env -> C.Context v a -> v -> AT.AnnotatedText (Maybe b)
renderVar' env ctx v =
  case C.lookupSolved ctx v of
    Nothing -> "unsolved"
    Just t -> renderType' env $ Type.getPolytype t

renderKind :: Kind -> AT.AnnotatedText (Maybe a)
renderKind Kind.Star          = "*"
renderKind (Kind.Arrow k1 k2) = renderKind k1 <> " -> " <> renderKind k2

showRef :: IsString s => Env -> R.Reference -> s
showRef env r = fromString $ fromMaybe (show r) (Map.lookup r (referenceNames env))

-- todo: do something different/better if cid not found
showConstructor :: IsString s => Env -> R.Reference -> Int -> s
showConstructor env r cid = fromString $
  fromMaybe (showRef env r ++ "/" ++ show cid)
            (Map.lookup (r,cid) (constructorNames env))

styleInOverallType :: (Var v, Annotated a, Eq a)
                   => Env
                   -> C.Type v a
                   -> C.Type v a
                   -> Color.Style
                   -> StyledText
styleInOverallType e overallType leafType c =
  renderType e f overallType
    where f loc s = if loc == ABT.annotation leafType then Color.style c s else s

_posToEnglish :: IsString s => L.Pos -> s
_posToEnglish (L.Pos l c) = fromString $ "Line " ++ show l ++ ", Column " ++ show c

rangeForToken :: L.Token a -> Range
rangeForToken t = Range (L.start t) (L.end t)

rangeToEnglish :: IsString s => Range -> s
rangeToEnglish (Range (L.Pos l c) (L.Pos l' c')) = fromString $
  let showColumn = True in
  if showColumn
  then if l == l'
    then if c == c'
        then "line " ++ show l ++ ", column " ++ show c
        else "line " ++ show l ++ ", columns " ++ show c ++ "-" ++ show c'
    else "line " ++ show l ++ ", column " ++ show c ++ " through " ++
        "line " ++ show l' ++ ", column " ++ show c'
  else if l == l'
    then "line " ++ show l
    else "lines " ++ show l ++ "—" ++ show l'

annotatedToEnglish :: (Annotated a, IsString s) => a -> s
annotatedToEnglish a = case ann a of
  Intrinsic     -> "an intrinsic"
  Ann start end -> rangeToEnglish $ Range start end


rangeForAnnotated :: Annotated a => a -> Maybe Range
rangeForAnnotated a = case ann a of
  Intrinsic     -> Nothing
  Ann start end -> Just $ Range start end

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
    <> (case unexpected of
         Just (P.Tokens ts) ->
          traceShow ts $
          AT.sectionToDoc $ showSource s
                    ((\t -> (rangeForToken t, Color.ErrorSite)) <$> toList ts)
         _ -> mempty
       )
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
annotatedAsErrorSite = annotatedAsStyle Color.ErrorSite

annotatedAsStyle :: (Ord s, Annotated a) => s -> String -> a -> AT.Section s
annotatedAsStyle style s ann =
  showSourceMaybes s [(, style) <$> rangeForAnnotated ann]

tokenAsErrorSite :: String -> L.Token a -> AT.Section Color.Style
tokenAsErrorSite src tok = showSource1 src (rangeForToken tok, Color.ErrorSite)

showSourceMaybes :: Ord a => String -> [Maybe (Range, a)] -> AT.Section a
showSourceMaybes src annotations = showSource src $ catMaybes annotations

showSource :: Ord a => String -> [(Range, a)] -> AT.Section a
showSource src annotations = AT.Blockquote $ AT.markup (fromString src) (Set.fromList annotations)

showSource1 :: Ord a => String -> (Range, a) -> AT.Section a
showSource1 src annotation = showSource src [annotation]

findTerm :: Seq (C.PathElement v loc) -> Maybe loc
findTerm = go
  where go (C.InSynthesize t :<| _)      = Just $ ABT.annotation t
        go (C.InCheck t _ :<| _)         = Just $ ABT.annotation t
        go (C.InSynthesizeApp _ t _ :<| _) = Just $ ABT.annotation t
        go (_ :<| t)                     = go t
        go Empty                         = Nothing

prettyTypecheckError :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
                     => Env
                     -> String
                     -> C.Note v loc -> AT.AnnotatedDocument Color.Style
prettyTypecheckError env input n =
  renderTypeError env (typeErrorFromNote n) input

parseErrorToAnsiString :: Var v => String -> Parser.Err v -> String
parseErrorToAnsiString src = show . Color.renderDocANSI 3 . prettyParseError src
