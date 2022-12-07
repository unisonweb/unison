module Unison.Syntax.DeclPrinter (prettyDecl, prettyDeclHeader, prettyDeclOrBuiltinHeader) where

import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration
  ( DataDeclaration,
    EffectDeclaration,
    toDataDecl,
  )
import qualified Unison.DataDeclaration as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import qualified Unison.HashQualified as HQ
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Name (Name)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.Reference (Reference (DerivedId))
import qualified Unison.Referent as Referent
import qualified Unison.Result as Result
import qualified Unison.Syntax.HashQualified as HQ (toString, toVar, unsafeFromString)
import Unison.Syntax.NamePrinter (styleHashQualified'')
import Unison.Syntax.TypePrinter (runPretty)
import qualified Unison.Syntax.TypePrinter as TypePrinter
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup (TypeLookup))
import qualified Unison.Typechecker.TypeLookup as TypeLookup
import Unison.Util.Pretty (Pretty)
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)
import qualified Unison.Var as Var

type SyntaxText = S.SyntaxText' Reference

prettyDecl ::
  (Var v, Monad m) =>
  PrettyPrintEnvDecl m ->
  Reference ->
  HQ.HashQualified Name ->
  DD.Decl v a ->
  m (Pretty SyntaxText)
prettyDecl pped r hq d = case d of
  Left e -> PPE.usePPE (suffixifiedPPE pped) $ prettyEffectDecl r hq e
  Right dd -> prettyDataDecl pped r hq dd

prettyEffectDecl ::
  (Var v, PPE.PrettyPrint m) =>
  Reference ->
  HQ.HashQualified Name ->
  EffectDeclaration v a ->
  m (Pretty SyntaxText)
prettyEffectDecl r name = prettyGADT CT.Effect r name . toDataDecl

prettyGADT ::
  (Var v, PPE.PrettyPrint m) =>
  CT.ConstructorType ->
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  m (Pretty SyntaxText)
prettyGADT ctorType r name dd = do
  ctors <- traverse constructor (zip [0 ..] (DD.constructors' dd))
  pure . P.hang header . P.lines $ ctors
  where
    constructor (n, (_, _, t)) = do
      prettyPat <- prettyPattern ctorType name (ConstructorReference r n)
      prettyTyp <- TypePrinter.prettySyntax t
      pure
        ( prettyPat
            <> fmt S.TypeAscriptionColon " :"
              `P.hang` prettyTyp
        )
    header = prettyEffectHeader name (DD.EffectDeclaration dd) <> fmt S.ControlKeyword " where"

prettyPattern ::
  (PPE.PrettyPrint m) =>
  CT.ConstructorType ->
  HQ.HashQualified Name ->
  ConstructorReference ->
  m (Pretty SyntaxText)
prettyPattern ctorType namespace ref = do
  styleHashQualified''
    (fmt (S.TermReference conRef))
    . strip
    <$> PPE.termName conRef
  where
    strip =
      case HQ.toName namespace of
        Nothing -> id
        Just name -> HQ.stripNamespace name
    conRef = Referent.Con ref ctorType

prettyDataDecl ::
  forall m v a.
  (Var v, Monad m) =>
  PrettyPrintEnvDecl m ->
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  m (Pretty SyntaxText)
prettyDataDecl (PrettyPrintEnvDecl unsuffixifiedPPE suffixifiedPPE) r name dd = do
  ctors <- traverse constructor (zip [0 ..] (DD.constructors' dd))
  pure $ (header <>) . P.sep (fmt S.DelimiterChar (" | " `P.orElse` "\n  | ")) $ ctors
  where
    constructor :: (ConstructorId, (a, b, Type.Type v a)) -> m (Pretty SyntaxText)
    constructor (n, (_, _, Type.ForallsNamed' _ t)) = constructor' n t
    constructor (n, (_, _, t)) = constructor' n t
    constructor' :: (ConstructorId -> Type.Type v a -> m (Pretty SyntaxText))
    constructor' n t = case Type.unArrows t of
      Nothing -> PPE.usePPE suffixifiedPPE (prettyPattern CT.Data name (ConstructorReference r n))
      Just ts ->
        PPE.usePPE unsuffixifiedPPE (fieldNames r name dd) >>= \case
          Nothing -> do
            prettyPat <- PPE.usePPE suffixifiedPPE (prettyPattern CT.Data name (ConstructorReference r n))
            prettyT <- PPE.usePPE suffixifiedPPE (runPretty (traverse (TypePrinter.prettyRaw Map.empty 10) (init ts)))
            pure
              ( P.group . P.hang' prettyPat "      " $
                  P.spaced prettyT
              )
          Just fs -> do
            prettyFields <- for (zip fs (init ts)) field
            pure . P.group $
              fmt S.DelimiterChar "{ "
                <> P.sep
                  (fmt S.DelimiterChar "," <> " " `P.orElse` "\n      ")
                  prettyFields
                <> fmt S.DelimiterChar " }"
    field (fname, typ) = do
      prettyT <- PPE.usePPE suffixifiedPPE $ runPretty (TypePrinter.prettyRaw Map.empty (-1) typ)
      pure . P.group $
        styleHashQualified'' (fmt (S.TypeReference r)) fname
          <> fmt S.TypeAscriptionColon " :" `P.hang` prettyT
    header = prettyDataHeader name dd <> fmt S.DelimiterChar (" = " `P.orElse` "\n  = ")

-- Comes up with field names for a data declaration which has the form of a
-- record, like `type Pt = { x : Int, y : Int }`. Works by generating the
-- record accessor terms for the data type, hashing these terms, and then
-- checking the `PrettyPrintEnv` for the names of those hashes. If the names for
-- these hashes are:
--
--   `Pt.x`, `Pt.x.set`, `Pt.x.modify`, `Pt.y`, `Pt.y.set`, `Pt.y.modify`
--
-- then this matches the naming convention generated by the parser, and we
-- return `x` and `y` as the field names.
--
-- This function bails with `Nothing` if the names aren't an exact match for
-- the expected record naming convention.
fieldNames ::
  forall v a m.
  (Var v, PPE.PrettyPrint m) =>
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  m (Maybe [HQ.HashQualified Name])
fieldNames r name dd = runMaybeT do
  typ <- case DD.constructors dd of
    [(_, typ)] -> pure typ
    _ -> empty
  let vars :: [v]
      vars = [Var.freshenId (fromIntegral n) (Var.named "_") | n <- [0 .. Type.arity typ - 1]]
  let accessors :: [(v, Term.Term v ())]
      accessors = DD.generateRecordAccessors (map (,()) vars) (HQ.toVar name) r
  let typeLookup :: TypeLookup v ()
      typeLookup =
        TypeLookup
          { TypeLookup.typeOfTerms = mempty,
            TypeLookup.dataDecls = Map.singleton r (void dd),
            TypeLookup.effectDecls = mempty
          }
  let typecheckingEnv :: Typechecker.Env v ()
      typecheckingEnv =
        Typechecker.Env
          { Typechecker._ambientAbilities = mempty,
            Typechecker._typeLookup = typeLookup,
            Typechecker._termsByShortname = mempty
          }
  accessorsWithTypes :: [(v, Term.Term v (), Type.Type v ())] <-
    for accessors \(v, trm) ->
      case Result.result (Typechecker.synthesize typecheckingEnv trm) of
        Nothing -> empty
        Just typ -> pure (v, trm, typ)
  let hashes = Hashing.hashTermComponents (Map.fromList . fmap (\(v, trm, typ) -> (v, (trm, typ))) $ accessorsWithTypes)
  names <- lift $ for (Map.elems hashes) \(refId, _trm, _typ) -> do
    name <- HQ.toString <$> (PPE.termName . Referent.Ref $ DerivedId refId)
    pure (refId, name)
  let fieldNames =
        Map.fromList
          [ (r, f) | (r, n) <- names, typename <- pure (HQ.toString name), typename `isPrefixOf` n, rest <- pure $ drop (length typename + 1) n, (f, rest) <- pure $ span (/= '.') rest, rest `elem` ["", ".set", ".modify"]
          ]

  if Map.size fieldNames == length names
    then
      pure
        [ HQ.unsafeFromString name
          | v <- vars,
            Just (ref, _, _) <- [Map.lookup (Var.namespaced [HQ.toVar name, v]) hashes],
            Just name <- [Map.lookup ref fieldNames]
        ]
    else empty

prettyModifier :: DD.Modifier -> Pretty SyntaxText
prettyModifier DD.Structural = fmt S.DataTypeModifier "structural"
prettyModifier (DD.Unique _uid) =
  fmt S.DataTypeModifier "unique" -- <> ("[" <> P.text uid <> "] ")

prettyDataHeader ::
  Var v => HQ.HashQualified Name -> DD.DataDeclaration v a -> Pretty SyntaxText
prettyDataHeader name dd =
  P.sepNonEmpty
    " "
    [ prettyModifier (DD.modifier dd),
      fmt S.DataTypeKeyword "type",
      styleHashQualified'' (fmt $ S.HashQualifier name) name,
      P.sep " " (fmt S.DataTypeParams . P.text . Var.name <$> DD.bound dd)
    ]

prettyEffectHeader ::
  Var v =>
  HQ.HashQualified Name ->
  DD.EffectDeclaration v a ->
  Pretty SyntaxText
prettyEffectHeader name ed =
  P.sepNonEmpty
    " "
    [ prettyModifier (DD.modifier (DD.toDataDecl ed)),
      fmt S.DataTypeKeyword "ability",
      styleHashQualified'' (fmt $ S.HashQualifier name) name,
      P.sep
        " "
        (fmt S.DataTypeParams . P.text . Var.name <$> DD.bound (DD.toDataDecl ed))
    ]

prettyDeclHeader ::
  Var v =>
  HQ.HashQualified Name ->
  Either (DD.EffectDeclaration v a) (DD.DataDeclaration v a) ->
  Pretty SyntaxText
prettyDeclHeader name (Left e) = prettyEffectHeader name e
prettyDeclHeader name (Right d) = prettyDataHeader name d

prettyDeclOrBuiltinHeader ::
  Var v =>
  HQ.HashQualified Name ->
  DD.DeclOrBuiltin v a ->
  Pretty SyntaxText
prettyDeclOrBuiltinHeader name (DD.Builtin ctype) = case ctype of
  CT.Data -> fmt S.DataTypeKeyword "builtin type " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
  CT.Effect -> fmt S.DataTypeKeyword "builtin ability " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
prettyDeclOrBuiltinHeader name (DD.Decl e) = prettyDeclHeader name e

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = P.withSyntax
