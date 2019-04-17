{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}


module Unison.PrintError where

-- import           Unison.Parser              (showLineCol)
-- import           Unison.Util.Monoid         (whenM)
import           Debug.Trace
import           Control.Lens                 ((%~))
import           Control.Lens.Tuple           (_1, _2, _3)
import           Control.Monad                (join)
import qualified Data.Char                    as Char
import           Data.Foldable
import           Data.List                    (intersperse, sortOn)
import qualified Data.List.NonEmpty           as Nel
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           Data.Sequence                (Seq (..))
import qualified Data.Set                     as Set
import           Data.String                  (IsString, fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Void                    (Void)
import qualified Text.Megaparsec              as P
import qualified Unison.ABT                   as ABT
import qualified Unison.DataDeclaration       as DD
import Unison.DataDeclaration (pattern TupleType')
import qualified Unison.HashQualified         as HQ
import           Unison.Kind                  (Kind)
import qualified Unison.Kind                  as Kind
import qualified Unison.Lexer                 as L
import           Unison.Parser                (Ann (..), Annotated, ann)
import qualified Unison.Parser                as Parser
import qualified Unison.Reference             as R
import           Unison.Referent              (Referent)
import qualified Unison.Referent              as Referent
import           Unison.Result                (Note (..))
import qualified Unison.Settings              as Settings
import qualified Unison.Term                  as Term
import qualified Unison.Type                  as Type
import qualified Unison.Typechecker.Context   as C
import           Unison.Typechecker.TypeError
import qualified Unison.TypeVar               as TypeVar
import qualified Unison.UnisonFile            as UF
import           Unison.Util.AnnotatedText    (AnnotatedText)
import qualified Unison.Util.AnnotatedText    as AT
import           Unison.Util.ColorText        (Color)
import qualified Unison.Util.ColorText        as Color
import           Unison.Util.Monoid           (intercalateMap)
import           Unison.Util.Range            (Range (..))
import           Unison.Var                   (Var)
import qualified Unison.Var                   as Var
import qualified Unison.PrettyPrintEnv as PPE

type Env = PPE.PrettyPrintEnv

pattern Type1 = Color.HiBlue
pattern Type2 = Color.Green
pattern ErrorSite = Color.HiRed
pattern TypeKeyword = Color.Yellow
pattern AbilityKeyword = Color.Green
pattern Identifier = Color.Bold

fromOverHere'
  :: Ord a
  => String
  -> [Maybe (Range, a)]
  -> [Maybe (Range, a)]
  -> AnnotatedText a
fromOverHere' s spots0 removing =
  fromOverHere s (catMaybes spots0) (catMaybes removing)

fromOverHere
  :: Ord a => String -> [(Range, a)] -> [(Range, a)] -> AnnotatedText a
fromOverHere src spots0 removing =
  let spots = toList $ Set.fromList spots0 Set.\\ Set.fromList removing
  in  case length spots of
        0 -> mempty
        1 -> "\n  from right here:\n\n" <> showSource src spots
        _ -> "\n  from these spots, respectively:\n\n" <> showSource src spots

showTypeWithProvenance
  :: (Var v, Annotated a, Ord style)
  => Env
  -> String
  -> style
  -> Type.AnnotatedType v a
  -> AnnotatedText style
showTypeWithProvenance env src color typ =
  style color (renderType' env typ)
    <> ".\n"
    <> fromOverHere' src [styleAnnotated color typ] []

styleAnnotated :: Annotated a => sty -> a -> Maybe (Range, sty)
styleAnnotated sty a = (, sty) <$> rangeForAnnotated a

style :: s -> String -> AnnotatedText s
style sty str = AT.annotate sty (fromString str)

describeStyle :: Color -> AnnotatedText Color
describeStyle ErrorSite = "in " <> style ErrorSite "red"
describeStyle Type1     = "in " <> style Type1 "blue"
describeStyle Type2     = "in " <> style Type2 "green"
describeStyle _         = ""

prettyTypecheckedFile'
  :: forall v loc
   . (Var v, Annotated loc, Ord loc, Show loc)
  => UF.TypecheckedUnisonFile v loc
  -> Env
  -> ([(v, AnnotatedText Color)], -- types
      [(v, AnnotatedText Color)]) -- terms
prettyTypecheckedFile' file env = (sortOn fst types, sortOn fst terms)
  where
  dot = "  "
  terms = renderTerm dot <$> join (UF.topLevelComponents file)
  types = (renderDecl (dot <> style TypeKeyword "type ") <$> Map.toList (UF.dataDeclarations' file))
       <> (renderEffect dot <$> Map.toList (UF.effectDeclarations' file))

  renderVar :: Var v => v -> AnnotatedText Color
  renderVar v = style Identifier . fromString . Text.unpack $ Var.name v

  renderTerm :: AnnotatedText Color -> (v, Term.AnnotatedTerm v loc, Type.AnnotatedType v loc) -> (v, AnnotatedText Color)
  renderTerm s (v, _, typ) =
    (v, mconcat [s, renderVar v, " : ", renderType' env typ])
  renderDecl :: AnnotatedText Color -> (v, (r, DD.DataDeclaration' v loc)) -> (v, AnnotatedText Color)
  renderDecl s (v, (_, decl)) = (v, mconcat
    [s, renderVar v, " ", intercalateMap " " renderVar $ DD.bound decl])
  renderEffect :: AnnotatedText Color -> (v, (r, DD.EffectDeclaration' v loc)) -> (v, AnnotatedText Color)
  renderEffect s (v, (r, decl)) = renderDecl (s <> style AbilityKeyword "ability ") (v, (r, DD.toDataDecl decl))

prettyTypecheckedFile
  :: forall v loc
   . (Var v, Annotated loc, Ord loc, Show loc)
  => UF.TypecheckedUnisonFile v loc -> Env -> AnnotatedText Color
prettyTypecheckedFile file env = let
  (types, terms) = prettyTypecheckedFile' file env
  sep n = if not (null n) then "\n" else ""
  in intercalateMap "\n" snd types <> sep types <>
     intercalateMap "\n" snd terms <> sep terms

-- Render an informational typechecking note
renderTypeInfo
  :: forall v loc sty
   . (Var v, Annotated loc, Ord loc, Show loc)
  => TypeInfo v loc
  -> Env
  -> AnnotatedText sty
renderTypeInfo i env = case i of
  TopLevelComponent {..} ->
    let defs =
          filter (\(v, _, _) -> Text.take 1 (Var.name v) /= "_") definitions
    in  case defs of
          [def] ->
            "🌟 I found and typechecked a definition:\n"
              <> mconcat (renderOne def)
          [] -> mempty
          _ ->
            "🎁 These mutually dependent definitions typechecked:\n"
              <> intercalateMap "\n" (foldMap ("\t" <>) . renderOne) defs
 where
  renderOne
    :: IsString s
    => (v, Type.AnnotatedType v loc, RedundantTypeAnnotation)
    -> [s]
  renderOne (v, typ, _) =
    [fromString . Text.unpack $ Var.name v, " : ", renderType' env typ]


-- Render a type error
renderTypeError
  :: forall v loc
   . (Var v, Annotated loc, Ord loc, Show loc)
  => TypeError v loc
  -> Env
  -> String
  -> AnnotatedText Color
renderTypeError e env src = case e of
  BooleanMismatch {..} -> mconcat
    [ preamble
    , " "
    , style Type1 "Boolean"
    , ", but this one is "
    , style Type2 (renderType' env foundType)
    , ":\n\n"
    , showSourceMaybes src [siteS]
    , fromOverHere' src [typeS] [siteS]
    , debugNoteLoc $ mconcat
      [ "loc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n"
      ]
    , debugSummary note
    ]
   where
    siteS    = styleAnnotated Type2 mismatchSite
    typeS    = styleAnnotated Type2 foundType
    preamble = case getBooleanMismatch of
      CondMismatch ->
        "The condition for an "
          <> style ErrorSite "if"
          <> "-expression has to be"
      AndMismatch ->
        "The arguments to " <> style ErrorSite "and" <> " have to be"
      OrMismatch ->
        "The arguments to " <> style ErrorSite "or" <> " have to be"
      GuardMismatch ->
        "The guard expression for a "
          <> style ErrorSite "case"
          <> " has to be"

  ExistentialMismatch {..} -> mconcat
    [ preamble
    , " "
    , "Here, one is "
    , style Type1 (renderType' env expectedType)
    , " and another is "
    , style Type2 (renderType' env foundType)
    , ":\n\n"
    , showSourceMaybes src [mismatchSiteS, expectedLocS]
    , fromOverHere' src
                    [expectedTypeS, mismatchedTypeS]
                    [mismatchSiteS, expectedLocS]
    , debugNoteLoc $ mconcat
      [ "\nloc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n  expectedType: "
      , annotatedToEnglish expectedType
      , "\n   expectedLoc: "
      , annotatedToEnglish expectedLoc
      , "\n"
      ]
    , debugSummary note
    ]
   where
    mismatchedTypeS = styleAnnotated Type2 foundType
    mismatchSiteS   = styleAnnotated Type2 mismatchSite
    expectedTypeS   = styleAnnotated Type1 expectedType
    expectedLocS    = styleAnnotated Type1 expectedLoc
    preamble        = case getExistentialMismatch of
      IfBody -> mconcat
        [ "The "
        , style ErrorSite "else"
        , " clause of an "
        , style ErrorSite "if"
        , " expression needs to have the same type as the "
        , style ErrorSite "then"
        , " clause."
        ]
      VectorBody -> "The elements of a vector all need to have the same type."
      CaseBody   -> mconcat
        [ "Each case of a "
        , style ErrorSite "case"
        , "/"
        , style ErrorSite "of"
        , " expression "
        , "need to have the same type."
        ]
  NotFunctionApplication {..} -> mconcat
    [ "This looks like a function call, but with a "
    , style Type1 (renderType' env ft)
    , " where the function should be.  Are you missing an operator?\n\n"
    , annotatedAsStyle Type1 src f
    , debugSummary note
    ]
  FunctionApplication {..}
    -> let
         fte         = Type.cleanup (Type.removePureEffects ft)
         fteFreeVars = Set.map TypeVar.underlying $ ABT.freeVars fte
         showVar (v, _t) = Set.member v fteFreeVars
         solvedVars' = filter showVar solvedVars
       in
         mconcat
           [ "The "
           , ordinal argNum
           , " argument to the function "
           , style ErrorSite (renderTerm env f)
           , " is "
           , style Type2 (renderType' env foundType)
           , ", but I was expecting "
           , style Type1 (renderType' env expectedType)
           , ":\n\n"
           , showSourceMaybes src
             [ (, Type1) <$> rangeForAnnotated expectedType
             , (, Type2) <$> rangeForAnnotated foundType
             , (, Type2) <$> rangeForAnnotated arg
             , (, ErrorSite) <$> rangeForAnnotated f
             ]
         -- todo: factor this out and use in ExistentialMismatch and any other
         --       "recursive subtypes" situations
           , case leafs of
             Nothing                        -> mempty
             Just (foundLeaf, expectedLeaf) -> mconcat
               [ "\n"
               , "More specifically, I found "
               , style Type2 (renderType' env foundLeaf)
               , " where I was expecting "
               , style Type1 (renderType' env expectedLeaf)
               , ":\n\n"
               , showSourceMaybes
                 src
                 [ (, Type1) <$> rangeForAnnotated expectedLeaf
                 , (, Type2) <$> rangeForAnnotated foundLeaf
                 ]
               ]
           , case solvedVars' of
             _ : _ ->
               let
                 go :: (v, C.Type v loc) -> AnnotatedText Color
                 go (v, t) = mconcat
                   [ " "
                   , renderVar v
                   , " = "
                   , style ErrorSite (renderType' env t)
                   , ", from here:\n\n"
                   , showSourceMaybes
                     src
                     [(, ErrorSite) <$> rangeForAnnotated t]
                   , "\n"
                   ]
               in
                 mconcat
                   [ "\n"
                   , "because the "
                   , style ErrorSite (renderTerm env f)
                   , " function has type"
                   , "\n\n"
                   , "  "
                   , renderType' env fte
                   , "\n\n"
                   , "where:"
                   , "\n\n"
                   , mconcat (go <$> solvedVars')
                   ]
             [] -> mempty
           , debugNoteLoc
           . mconcat
           $ [ "\nloc debug:"
             , style ErrorSite "\n             f: "
             , annotatedToEnglish f
             , style Type2 "\n     foundType: "
             , annotatedToEnglish foundType
             , style Type1 "\n  expectedType: "
             , annotatedToEnglish expectedType
             -- , "\n   expectedLoc: ", annotatedToEnglish expectedLoc
             ]
           , debugSummary note
           ]
  Mismatch {..} -> mconcat
    [ "I found a value of type "
    , style Type1 (renderType' env foundLeaf)
    , " where I expected to find one of type "
    , style Type2 (renderType' env expectedLeaf)
    , ":\n\n"
    , showSourceMaybes
      src
      [ -- these are overwriting the colored ranges for some reason?
        --   (,Color.ForceShow) <$> rangeForAnnotated mismatchSite
        -- , (,Color.ForceShow) <$> rangeForType foundType
        -- , (,Color.ForceShow) <$> rangeForType expectedType
        -- ,
        (, Type1) <$> rangeForAnnotated mismatchSite
      , (, Type2) <$> rangeForAnnotated expectedLeaf
      ]
    , fromOverHere' src
                    [styleAnnotated Type1 foundLeaf]
                    [styleAnnotated Type1 mismatchSite]
    , debugNoteLoc
    . mconcat
    $ [ "\nloc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n     foundLeaf: "
      , annotatedToEnglish foundLeaf
      , "\n  expectedType: "
      , annotatedToEnglish expectedType
      , "\n  expectedLeaf: "
      , annotatedToEnglish expectedLeaf
      , "\n"
      ]
    , debugSummary note
    ]
  AbilityCheckFailure {..} -> mconcat
    [ "The expression "
    , describeStyle ErrorSite
    , " "
    , case toList requested of
      []  -> error "unpossible"
      [e] -> "needs the {" <> renderType' env e <> "} ability,"
      requested ->
        " needs these abilities: {"
          <> commas (renderType' env) requested
          <> "},"
    , " but "
    , case toList ambient of
      [] -> "this location does not have access to any abilities."
      [e] ->
        "this location only has access to the {"
          <> renderType' env e
          <> "} ability,"
      ambient ->
        "this location only has access to these abilities: "
          <> "{"
          <> commas (renderType' env) ambient
          <> "}"
    , "\n\n"
    , annotatedAsErrorSite src abilityCheckFailureSite
    , debugSummary note
    ]
  UnguardedLetRecCycle vs locs _ -> mconcat
    [ "These definitions depend on each other cyclically but aren't guarded "
    , "by a lambda: " <> intercalateMap ", " renderVar vs
    , "\n"
    , showSourceMaybes src [ (,ErrorSite) <$> rangeForAnnotated loc | loc <- locs ]]

  UnknownType {..} -> mconcat
    [ "I don't know about the type "
    , style ErrorSite (renderVar unknownTypeV)
    , ".  Make sure it's imported and spelled correctly:\n\n"
    , annotatedAsErrorSite src typeSite
    ]
  UnknownTerm {..}
    | Type.isArrow expectedType && Var.typeOf unknownTermV == Var.AskInfo
    -> let Type.Arrow' i o = case expectedType of
             Type.ForallsNamed' _ body -> body
             _                         -> expectedType
       in
         mconcat
           [ "Here's what I know about the expression at "
           , annotatedToEnglish termSite
           , ":\n\n"
           , annotatedAsErrorSite src termSite
           , "\n"
           , "Its type is: "
           , style ErrorSite (renderType' env i)
           , ".\n\n"
           , case o of
             Type.Existential' _ _ ->
               "It can be replaced with a value of any type.\n"
             _ ->
               "A well-typed replacement must conform to: "
                 <> style Type2 (renderType' env o)
                 <> ".\n"
           ]
  UnknownTerm {..} | Var.typeOf unknownTermV == Var.MissingResult -> mconcat
    [ "I found a block that ends with a binding instead of an expression at "
    , annotatedToEnglish termSite
    , ":\n\n"
    , annotatedAsErrorSite src termSite
    , "\n"
    , case expectedType of
      Type.Existential' _ _ ->
        "To complete the block, add an expression after this binding.\n\n"
      _ ->
        "Based on the context, I'm expecting an expression of type "
          <> style Type1 (renderType' env expectedType)
          <> " after this binding. \n\n"
    ]
  UnknownTerm {..} ->
    let (correct, wrongTypes, wrongNames) =
          foldr sep id suggestions ([], [], [])
        sep (C.Suggestion name typ _) r = (_1 %~ ((name, typ) :)) . r
        sep (C.WrongType name typ   ) r = (_2 %~ ((name, typ) :)) . r
        sep (C.WrongName name typ   ) r = (_3 %~ ((name, typ) :)) . r
    in  mconcat
          [ "I'm not sure what "
          , style ErrorSite (show unknownTermV)
          , " means at "
          , annotatedToEnglish termSite
          , "\n\n"
          , annotatedAsErrorSite src termSite
          , case expectedType of
            Type.Existential' _ _ -> "\nThere are no constraints on its type."
            _ ->
              "\nWhatever it is, it has a type that conforms to "
                <> style Type1 (renderType' env expectedType)
                <> ".\n"
                 -- ++ showTypeWithProvenance env src Type1 expectedType
          , case correct of
            [] -> case wrongTypes of
              [] -> case wrongNames of
                []     -> mempty
                wrongs -> formatWrongs wrongNameText wrongs
              wrongs -> formatWrongs wrongTypeText wrongs
            suggs -> mconcat
              [ "I found some terms in scope that have matching names and types. "
              , "Maybe you meant one of these:\n\n"
              , intercalateMap "\n" formatSuggestion suggs
              ]
          ]
  Other note -> mconcat
    [ "Sorry, you hit an error we didn't make a nice message for yet.\n\n"
    , "Here is a summary of the Note:\n"
    , summary note
    ]
 where
  wrongTypeText pl = mconcat
    [ "I found "
    , pl "a term" "some terms"
    , " in scope with "
    , pl "a " ""
    , "matching name"
    , pl "" "s"
    , " but "
    , pl "a " ""
    , "different type"
    , pl "" "s"
    , ". "
    , "If "
    , pl "this" "one of these"
    , " is what you meant, try using the fully qualified name and I might "
    , "be able to give you a more illuminating error message: \n\n"
    ]
  wrongNameText pl = mconcat
    [ "I found "
    , pl "a term" "some terms"
    , " in scope with "
    , pl "a " ""
    , "matching type"
    , pl "" "s"
    , " but "
    , pl "a " ""
    , "different name"
    , pl "" "s"
    , ". "
    , "Maybe you meant "
    , pl "this" "one of these"
    , ":\n\n"
    ]
  formatSuggestion :: (Text, C.Type v loc) -> AnnotatedText Color
  formatSuggestion (name, typ) =
    "  - " <> fromString (Text.unpack name) <> " : " <> renderType' env typ
  formatWrongs txt wrongs =
    let sz = length wrongs
        pl a b = if sz == 1 then a else b
    in  mconcat [txt pl, intercalateMap "\n" formatSuggestion wrongs]
  ordinal :: (IsString s) => Int -> s
  ordinal n = fromString $ show n ++ case last (show n) of
    '1' -> "st"
    '2' -> "nd"
    '3' -> "rd"
    _   -> "th"
  debugNoteLoc a = if Settings.debugNoteLoc then a else mempty
  debugSummary :: C.ErrorNote v loc -> AnnotatedText Color
  debugSummary note =
    if Settings.debugNoteSummary then summary note else mempty
  summary :: C.ErrorNote v loc -> AnnotatedText Color
  summary note = mconcat
    [ "\n"
    , "  simple cause:\n"
    , "    "
    , simpleCause (C.cause note)
    , "\n"
    , case toList (C.path note) of
      [] -> "  path: (empty)\n"
      l  -> "  path:\n" <> mconcat (simplePath <$> l)
    ]
  simplePath :: C.PathElement v loc -> AnnotatedText Color
  simplePath e = "    " <> simplePath' e <> "\n"
  simplePath' :: C.PathElement v loc -> AnnotatedText Color
  simplePath' = \case
    C.InSynthesize e -> "InSynthesize e=" <> renderTerm env e
    C.InSubtype t1 t2 ->
      "InSubtype t1=" <> renderType' env t1 <> ", t2=" <> renderType' env t2
    C.InCheck e t ->
      "InCheck e=" <> renderTerm env e <> "," <> " t=" <> renderType' env t
    C.InInstantiateL v t ->
      "InInstantiateL v=" <> renderVar v <> ", t=" <> renderType' env t
    C.InInstantiateR t v ->
      "InInstantiateR t=" <> renderType' env t <> " v=" <> renderVar v
    C.InSynthesizeApp t e n ->
      "InSynthesizeApp t="
        <> renderType' env t
        <> ", e="
        <> renderTerm env e
        <> ", n="
        <> fromString (show n)
    C.InFunctionCall vs f ft es ->
      "InFunctionCall vs=["
        <> commas renderVar vs
        <> "]"
        <> ", f="
        <> renderTerm env f
        <> ", ft="
        <> renderType' env ft
        <> ", es=["
        <> commas (renderTerm env) es
        <> "]"
    C.InIfCond        -> "InIfCond"
    C.InIfBody loc    -> "InIfBody thenBody=" <> annotatedToEnglish loc
    C.InAndApp        -> "InAndApp"
    C.InOrApp         -> "InOrApp"
    C.InVectorApp loc -> "InVectorApp firstTerm=" <> annotatedToEnglish loc
    C.InMatch     loc -> "InMatch firstBody=" <> annotatedToEnglish loc
    C.InMatchGuard    -> "InMatchGuard"
    C.InMatchBody     -> "InMatchBody"
  simpleCause :: C.Cause v loc -> AnnotatedText Color
  simpleCause = \case
    C.TypeMismatch c ->
      mconcat ["TypeMismatch\n", "  context:\n", renderContext env c]
    C.IllFormedType c ->
      mconcat ["IllFormedType\n", "  context:\n", renderContext env c]
    C.UnguardedLetRecCycle vs _ts ->
      "Unguarded cycle of definitions: " <>
      foldMap renderVar vs
    C.UnknownSymbol loc v -> mconcat
      [ "UnknownSymbol: "
      , annotatedToEnglish loc
      , " " <> renderVar v
      , "\n\n"
      , annotatedAsErrorSite src loc
      ]
    C.UnknownTerm loc v suggestions typ -> mconcat
      [ "UnknownTerm: "
      , annotatedToEnglish loc
      , " "
      , renderVar v
      , "\n\n"
      , annotatedAsErrorSite src loc
      , "Suggestions: "
      , mconcat (renderSuggestion env <$> suggestions)
      , "\n\n"
      , "Type: "
      , renderType' env typ
      ]
    C.CompilerBug c -> "CompilerBug: " <> fromString (show c)
    C.AbilityCheckFailure ambient requested c -> mconcat
      [ "AbilityCheckFailure: "
      , "ambient={"
      , commas (renderType' env) ambient
      , "} requested={"
      , commas (renderType' env) requested
      , "}\n"
      , renderContext env c
      ]
    C.EffectConstructorWrongArgCount e a r cid -> mconcat
      [ "EffectConstructorWrongArgCount:"
      , "  expected="
      , (fromString . show) e
      , ", actual="
      , (fromString . show) a
      , ", reference="
      , showConstructor env r cid
      ]
    C.MalformedEffectBind ctorType ctorResult es -> mconcat
      [ "MalformedEffectBind: "
      , "  ctorType="
      , renderType' env ctorType
      , "  ctorResult="
      , renderType' env ctorResult
      , "  effects="
      , fromString (show es)
      ]
    C.PatternArityMismatch loc typ args -> mconcat
      [ "PatternArityMismatch:\n"
      , "  loc="
      , annotatedToEnglish loc
      , "\n"
      , "  typ="
      , renderType' env typ
      , "\n"
      , "  args="
      , fromString (show args)
      , "\n"
      ]
    C.DuplicateDefinitions vs ->
      let go :: (v, [loc]) -> AnnotatedText a
          go (v, locs) =
            "["
              <> renderVar v
              <> mconcat (intersperse " : " $ annotatedToEnglish <$> locs)
              <> "]"
      in  "DuplicateDefinitions:" <> mconcat (go <$> Nel.toList vs)

renderContext
  :: (Var v, Ord loc) => Env -> C.Context v loc -> AnnotatedText a
renderContext env ctx@(C.Context es) = "  Γ\n    "
  <> intercalateMap "\n    " (showElem ctx . fst) (reverse es)
 where
  shortName :: (Var v, IsString loc) => v -> loc
  shortName = fromString . Text.unpack . Var.name
  showElem
    :: (Var v, Ord loc)
    => C.Context v loc
    -> C.Element v loc
    -> AnnotatedText a
  showElem _ctx (C.Var v) = case v of
    TypeVar.Universal x     -> "@" <> renderVar x
    TypeVar.Existential _ x -> "'" <> renderVar x
  showElem ctx (C.Solved _ v (Type.Monotype t)) =
    "'" <> shortName v <> " = " <> renderType' env (C.apply ctx t)
  showElem ctx (C.Ann v t) =
    shortName v <> " : " <> renderType' env (C.apply ctx t)
  showElem _ (C.Marker v) = "|" <> shortName v <> "|"

renderTerm :: (IsString s, Var v) => Env -> C.Term v loc -> s
renderTerm _ (ABT.Var' v) | Settings.demoHideVarNumber =
  fromString (Text.unpack $ Var.name v)
renderTerm env (Term.Ref' r) =
  fromString (HQ.toString $ PPE.termName env (Referent.Ref r))
renderTerm _ e =
  let s = show e
  in      -- todo: pretty print
      if length s > Settings.renderTermMaxLength
        then fromString (take Settings.renderTermMaxLength s <> "...")
        else fromString s

-- | renders a type with no special styling
renderType' :: (IsString s, Var v) => Env -> Type.AnnotatedType v loc -> s
renderType' env typ = fromString . Color.toPlain $ renderType env (const id) typ

-- | `f` may do some styling based on `loc`.
-- | You can pass `(const id)` if no styling is needed, or call `renderType'`.
renderType
  :: Var v
  => Env
  -> (loc -> AnnotatedText a -> AnnotatedText a)
  -> Type.AnnotatedType v loc
  -> AnnotatedText a
renderType env f t = renderType0 env f (0 :: Int) (Type.cleanup $ Type.removePureEffects t)
 where
  wrap :: (IsString a, Semigroup a) => a -> a -> Bool -> a -> a
  wrap start end test s = if test then start <> s <> end else s
  paren = wrap "(" ")"
  curly = wrap "{" "}"
  renderType0 env f p t = f (ABT.annotation t) $ case t of
    Type.Ref' r -> showTypeRef env r
    Type.Arrow' i (Type.Effect1' e o) ->
      paren (p >= 2) $ go 2 i <> " ->{" <> go 1 e <> "} " <> go 1 o
    Type.Arrow' i o -> paren (p >= 2) $ go 2 i <> " -> " <> go 1 o
    Type.Ann'   t k -> paren True $ go 1 t <> " : " <> renderKind k
    TupleType' ts  -> paren True $ commas (go 0) ts
    Type.Apps' (Type.Ref' (R.Builtin "Sequence")) [arg] ->
      "[" <> go 0 arg <> "]"
    Type.Apps' f' args -> paren (p >= 3) $ spaces (go 3) (f' : args)
    Type.Effects' es   -> curly (p >= 3) $ commas (go 0) es
    Type.Effect' es t  -> case es of
      [] -> go p t
      _  -> "{" <> commas (go 0) es <> "} " <> go 3 t
    Type.Effect1' e t -> paren (p >= 3) $ "{" <> go 0 e <> "}" <> go 3 t
    Type.ForallsNamed' vs body ->
      paren (p >= 1) $
--      if p == 0 then go 0 body
                       if not Settings.debugRevealForalls
        then go 0 body
        else "forall " <> spaces renderVar vs <> " . " <> go 1 body
    Type.Var' v -> renderVar v
    _ -> error $ "pattern match failure in PrintError.renderType " ++ show t
    where go = renderType0 env f

renderSuggestion
  :: (IsString s, Semigroup s, Var v) => Env -> C.Suggestion v loc -> s
renderSuggestion env sug =
  fromString (Text.unpack $ C.suggestionName sug) <> " : " <> renderType'
    env
    (C.suggestionType sug)

spaces :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
spaces = intercalateMap " "

arrows :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
arrows = intercalateMap " ->"

commas :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
commas = intercalateMap ", "

renderVar :: (IsString a, Var v) => v -> a
renderVar = fromString . Text.unpack . Var.name

renderVar' :: (Var v, Annotated a) => Env -> C.Context v a -> v -> String
renderVar' env ctx v = case C.lookupSolved ctx v of
  Nothing -> "unsolved"
  Just t  -> renderType' env $ Type.getPolytype t

renderKind :: Kind -> AnnotatedText a
renderKind Kind.Star          = "*"
renderKind (Kind.Arrow k1 k2) = renderKind k1 <> " -> " <> renderKind k2

showTermRef :: IsString s => Env -> Referent -> s
showTermRef env r = fromString . HQ.toString $ PPE.termName env r

showTypeRef :: IsString s => Env -> R.Reference -> s
showTypeRef env r = fromString . HQ.toString $ PPE.typeName env r

-- todo: do something different/better if cid not found
showConstructor :: IsString s => Env -> R.Reference -> Int -> s
showConstructor env r cid = fromString . HQ.toString $
  PPE.patternName env r cid

styleInOverallType
  :: (Var v, Annotated a, Eq a)
  => Env
  -> C.Type v a
  -> C.Type v a
  -> Color
  -> AnnotatedText Color
styleInOverallType e overallType leafType c = renderType e f overallType
  where f loc s = if loc == ABT.annotation leafType then Color.style c s else s

_posToEnglish :: IsString s => L.Pos -> s
_posToEnglish (L.Pos l c) =
  fromString $ "Line " ++ show l ++ ", Column " ++ show c

rangeForToken :: L.Token a -> Range
rangeForToken t = Range (L.start t) (L.end t)

rangeToEnglish :: IsString s => Range -> s
rangeToEnglish (Range (L.Pos l c) (L.Pos l' c')) =
  fromString
    $ let showColumn = True
      in
        if showColumn
          then if l == l'
            then if c == c'
              then "line " ++ show l ++ ", column " ++ show c
              else "line " ++ show l ++ ", columns " ++ show c ++ "-" ++ show c'
            else
              "line "
              ++ show l
              ++ ", column "
              ++ show c
              ++ " through "
              ++ "line "
              ++ show l'
              ++ ", column "
              ++ show c'
          else if l == l'
            then "line " ++ show l
            else "lines " ++ show l ++ "—" ++ show l'

annotatedToEnglish :: (Annotated a, IsString s) => a -> s
annotatedToEnglish a = case ann a of
  Intrinsic     -> "an intrinsic"
  External      -> "an external"
  Ann start end -> rangeToEnglish $ Range start end


rangeForAnnotated :: Annotated a => a -> Maybe Range
rangeForAnnotated a = case ann a of
  Intrinsic     -> Nothing
  External      -> Nothing
  Ann start end -> Just $ Range start end

showLexerOutput :: Bool
showLexerOutput = False

renderNoteAsANSI :: (Var v, Annotated a, Show a, Ord a)
                 => Env -> String -> Note v a -> String
renderNoteAsANSI e s n = Color.toANSI $ printNoteWithSource e s n

renderParseErrorAsANSI :: Var v => String -> Parser.Err v -> String
renderParseErrorAsANSI src = Color.toANSI . prettyParseError src

printNoteWithSource
  :: (Var v, Annotated a, Show a, Ord a)
  => Env
  -> String
  -> Note v a
  -> AnnotatedText Color
printNoteWithSource env  _s (TypeInfo  n) = prettyTypeInfo n env
printNoteWithSource _env s  (Parsing   e) = prettyParseError s e
printNoteWithSource env  s  (TypeError e) = prettyTypecheckError e env s
printNoteWithSource _env s (InvalidPath path term) =
  fromString ("Invalid Path: " ++ show path ++ "\n")
    <> annotatedAsErrorSite s term
printNoteWithSource _env s (UnknownSymbol v a) =
  fromString ("Unknown symbol `" ++ Text.unpack (Var.name v) ++ "`\n\n")
    <> annotatedAsErrorSite s a
printNoteWithSource _env _s (CompilerBug c) =
  fromString $ "Compiler bug: " <> show c

_printPosRange :: String -> L.Pos -> L.Pos -> String
_printPosRange s (L.Pos startLine startCol) _end =
  -- todo: multi-line ranges
  -- todo: ranges
  _printArrowsAtPos s startLine startCol

_printArrowsAtPos :: String -> Int -> Int -> String
_printArrowsAtPos s line column =
  let lineCaret s i = s ++ if i == line then "\n" ++ columnCaret else ""
      columnCaret = replicate (column - 1) '-' ++ "^"
      source      = unlines (uncurry lineCaret <$> lines s `zip` [1 ..])
  in  source

prettyParseError
  :: forall v
   . Var v
  => String
  -> Parser.Err v
  -> AnnotatedText Color
prettyParseError s = \case
  P.TrivialError sp unexpected expected
    -> fromString
        (P.parseErrorPretty @_ @Void (P.TrivialError sp unexpected expected))
      <> (case unexpected of
           Just (P.Tokens ts) -> traceShow ts $ showSource
             s
             ((\t -> (rangeForToken t, ErrorSite)) <$> toList ts)
           _ -> mempty
         )
      <> lexerOutput

  P.FancyError sp fancyErrors ->
    mconcat (go' <$> Set.toList fancyErrors) <> dumpSourcePos sp <> lexerOutput
 where
  dumpSourcePos :: Nel.NonEmpty P.SourcePos -> AnnotatedText a
  dumpSourcePos sp =
    (mconcat . toList) (fromString . (\s -> "  " ++ show s ++ "\n") <$> sp)
  go' :: P.ErrorFancy (Parser.Error v) -> AnnotatedText Color
  go' (P.ErrorFail s) =
    "The parser failed with this message:\n" <> fromString s
  go' (P.ErrorIndentation ordering indent1 indent2) = mconcat
    [ "The parser was confused by the indentation.\n"
    , "It was expecting the reference level ("
    , fromString (show indent1)
    , ")\nto be "
    , fromString (show ordering)
    , " than/to the actual level ("
    , fromString (show indent2)
    , ").\n"
    ]
  go' (P.ErrorCustom e) = go e
  go :: Parser.Error v -> AnnotatedText Color
  go (Parser.ExpectedBlockOpen blockName tok@(L.payload -> L.Close)) = mconcat
    [ "I was expecting an indented block following the " <>
      "`" <> fromString blockName <> "` keyword\n"
    , "but instead found an outdent:\n\n"
    , tokenAsErrorSite s tok ] -- todo: @aryairani why is this displaying weirdly?
  go (Parser.ExpectedBlockOpen blockName tok) = mconcat
    [ "I was expecting an indented block following the " <>
      "`" <> fromString blockName <> "` keyword\n"
    , "but instead found this token:\n"
    , tokenAsErrorSite s tok ]
  go (Parser.SignatureNeedsAccompanyingBody tok) = mconcat
    [ "You provided a type signature, but I didn't find an accompanying\n"
    , "binding after it.  Could it be a spelling mismatch?\n"
    , tokenAsErrorSite s tok
    ]
   -- we would include the last binding term if we didn't have to have an Ord
   -- instance for it
  go (Parser.BlockMustEndWithExpression blockAnn lastBindingAnn) = mconcat
    [ "The last line of the block starting at "
    , fromString . fmap Char.toLower . annotatedToEnglish $ blockAnn
    , "\n"
    , "has to be an expression, not a binding/import/etc:"
    , annotatedAsErrorSite s lastBindingAnn
    ]
  go (Parser.EmptyBlock tok) = mconcat
    [ "I expected a block after this ("
    , describeStyle ErrorSite
    , "), "
    , "but there wasn't one.  Maybe check your indentation:\n"
    , tokenAsErrorSite s tok
    ]
  go (Parser.EmptyWatch) =
    "I expected a non-empty watch expression and not just \">\""
  go (Parser.UnknownEffectConstructor tok) = unknownConstructor "effect" tok
  go (Parser.UnknownDataConstructor   tok) = unknownConstructor "data" tok
  unknownConstructor
    :: String -> L.Token String -> AnnotatedText Color
  unknownConstructor ctorType tok = mconcat
    [ "I don't know about any "
    , fromString ctorType
    , " constructor named "
    , style ErrorSite (show (L.payload tok))
    , ".\n"
    , "Maybe make sure it's correctly spelled and that you've imported it:\n"
    , tokenAsErrorSite s tok
    ]
  lexerOutput :: AnnotatedText a
  lexerOutput = if showLexerOutput
    then "\nLexer output:\n" <> fromString (L.debugLex' s)
    else mempty

annotatedAsErrorSite
  :: Annotated a => String -> a -> AnnotatedText Color
annotatedAsErrorSite = annotatedAsStyle ErrorSite

annotatedAsStyle
  :: (Ord s, Annotated a) => s -> String -> a -> AnnotatedText s
annotatedAsStyle style s ann =
  showSourceMaybes s [(, style) <$> rangeForAnnotated ann]

tokenAsErrorSite :: String -> L.Token a -> AnnotatedText Color
tokenAsErrorSite src tok = showSource1 src (rangeForToken tok, ErrorSite)

showSourceMaybes
  :: Ord a => String -> [Maybe (Range, a)] -> AnnotatedText a
showSourceMaybes src annotations = showSource src $ catMaybes annotations

showSource :: Ord a => String -> [(Range, a)] -> AnnotatedText a
showSource src annotations =
  AT.condensedExcerptToText 6 $
    AT.markup (fromString src) (Map.fromList annotations)

showSource1 :: Ord a => String -> (Range, a) -> AnnotatedText a
showSource1 src annotation = showSource src [annotation]

findTerm :: Seq (C.PathElement v loc) -> Maybe loc
findTerm = go
 where
  go (C.InSynthesize t        :<| _) = Just $ ABT.annotation t
  go (C.InCheck t _           :<| _) = Just $ ABT.annotation t
  go (C.InSynthesizeApp _ t _ :<| _) = Just $ ABT.annotation t
  go (_                       :<| t) = go t
  go Empty                           = Nothing

prettyTypecheckError
  :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
  => C.ErrorNote v loc
  -> Env
  -> String
  -> AnnotatedText Color
prettyTypecheckError = renderTypeError . typeErrorFromNote

prettyTypeInfo
  :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
  => C.InfoNote v loc
  -> Env
  -> AnnotatedText Color
prettyTypeInfo n e =
  maybe "" (`renderTypeInfo` e) (typeInfoFromNote n)
