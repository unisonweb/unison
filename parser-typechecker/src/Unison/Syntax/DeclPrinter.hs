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
import qualified Unison.HashQualified as HQ
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
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
  (Var v) =>
  PrettyPrintEnvDecl ->
  Reference ->
  HQ.HashQualified Name ->
  DD.Decl v a ->
  Pretty SyntaxText
prettyDecl ppe r hq d = case d of
  Left e -> prettyEffectDecl (suffixifiedPPE ppe) r hq e
  Right dd -> prettyDataDecl ppe r hq dd

prettyEffectDecl ::
  (Var v) =>
  PrettyPrintEnv ->
  Reference ->
  HQ.HashQualified Name ->
  EffectDeclaration v a ->
  Pretty SyntaxText
prettyEffectDecl ppe r name = prettyGADT ppe CT.Effect r name . toDataDecl

prettyGADT ::
  (Var v) =>
  PrettyPrintEnv ->
  CT.ConstructorType ->
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Pretty SyntaxText
prettyGADT env ctorType r name dd =
  P.hang header . P.lines $
    constructor
      <$> zip
        [0 ..]
        (DD.constructors' dd)
  where
    constructor (n, (_, _, t)) =
      prettyPattern env ctorType name (ConstructorReference r n)
        <> fmt S.TypeAscriptionColon " :"
          `P.hang` TypePrinter.prettySyntax env t
    header = prettyEffectHeader name (DD.EffectDeclaration dd) <> fmt S.ControlKeyword " where"

prettyPattern ::
  PrettyPrintEnv ->
  CT.ConstructorType ->
  HQ.HashQualified Name ->
  ConstructorReference ->
  Pretty SyntaxText
prettyPattern env ctorType namespace ref =
  styleHashQualified''
    (fmt (S.TermReference conRef))
    ( let strip =
            case HQ.toName namespace of
              Nothing -> id
              Just name -> HQ.stripNamespace name
       in strip (PPE.termName env conRef)
    )
  where
    conRef = Referent.Con ref ctorType

prettyDataDecl ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Pretty SyntaxText
prettyDataDecl (PrettyPrintEnvDecl unsuffixifiedPPE suffixifiedPPE) r name dd =
  (header <>) . P.sep (fmt S.DelimiterChar (" | " `P.orElse` "\n  | ")) $
    constructor
      <$> zip
        [0 ..]
        (DD.constructors' dd)
  where
    constructor (n, (_, _, Type.ForallsNamed' _ t)) = constructor' n t
    constructor (n, (_, _, t)) = constructor' n t
    constructor' n t = case Type.unArrows t of
      Nothing -> prettyPattern suffixifiedPPE CT.Data name (ConstructorReference r n)
      Just ts -> case fieldNames unsuffixifiedPPE r name dd of
        Nothing ->
          P.group . P.hang' (prettyPattern suffixifiedPPE CT.Data name (ConstructorReference r n)) "      " $
            P.spaced (runPretty suffixifiedPPE (traverse (TypePrinter.prettyRaw Map.empty 10) (init ts)))
        Just fs ->
          P.group $
            fmt S.DelimiterChar "{ "
              <> P.sep
                (fmt S.DelimiterChar "," <> " " `P.orElse` "\n      ")
                (field <$> zip fs (init ts))
              <> fmt S.DelimiterChar " }"
    field (fname, typ) =
      P.group $
        styleHashQualified'' (fmt (S.TypeReference r)) fname
          <> fmt S.TypeAscriptionColon " :" `P.hang` runPretty suffixifiedPPE (TypePrinter.prettyRaw Map.empty (-1) typ)
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
  forall v a.
  (Var v) =>
  PrettyPrintEnv ->
  Reference ->
  HQ.HashQualified Name ->
  DataDeclaration v a ->
  Maybe [HQ.HashQualified Name]
fieldNames env r name dd = do
  typ <- case DD.constructors dd of
    [(_, typ)] -> Just typ
    _ -> Nothing
  let vars :: [v]
      vars = [Var.freshenId (fromIntegral n) (Var.named "_") | n <- [0 .. Type.arity typ - 1]]
  let accessors :: [(v, (), Term.Term v ())]
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
    for accessors \(v, _a, trm) ->
      case Result.result (Typechecker.synthesize typecheckingEnv trm) of
        Nothing -> Nothing
        Just typ -> Just (v, trm, typ)
  let hashes = Hashing.hashTermComponents (Map.fromList . fmap (\(v, trm, typ) -> (v, (trm, typ, ()))) $ accessorsWithTypes)
  let names =
        [ (r, HQ.toString . PPE.termName env . Referent.Ref $ DerivedId r)
          | r <- (\(refId, _trm, _typ, _ann) -> refId) <$> Map.elems hashes
        ]
  let fieldNames =
        Map.fromList
          [ (r, f) | (r, n) <- names, typename <- pure (HQ.toString name), typename `isPrefixOf` n, rest <- pure $ drop (length typename + 1) n, (f, rest) <- pure $ span (/= '.') rest, rest `elem` ["", ".set", ".modify"]
          ]

  if Map.size fieldNames == length names
    then
      Just
        [ HQ.unsafeFromString name
          | v <- vars,
            Just (ref, _, _, _) <- [Map.lookup (Var.namespaced [HQ.toVar name, v]) hashes],
            Just name <- [Map.lookup ref fieldNames]
        ]
    else Nothing

prettyModifier :: DD.Modifier -> Pretty SyntaxText
prettyModifier DD.Structural = fmt S.DataTypeModifier "structural"
prettyModifier (DD.Unique _uid) =
  fmt S.DataTypeModifier "unique" -- <> ("[" <> P.text uid <> "] ")

prettyDataHeader ::
  (Var v) => HQ.HashQualified Name -> DD.DataDeclaration v a -> Pretty SyntaxText
prettyDataHeader name dd =
  P.sepNonEmpty
    " "
    [ prettyModifier (DD.modifier dd),
      fmt S.DataTypeKeyword "type",
      styleHashQualified'' (fmt $ S.HashQualifier name) name,
      P.sep " " (fmt S.DataTypeParams . P.text . Var.name <$> DD.bound dd)
    ]

prettyEffectHeader ::
  (Var v) =>
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
  (Var v) =>
  HQ.HashQualified Name ->
  Either (DD.EffectDeclaration v a) (DD.DataDeclaration v a) ->
  Pretty SyntaxText
prettyDeclHeader name (Left e) = prettyEffectHeader name e
prettyDeclHeader name (Right d) = prettyDataHeader name d

prettyDeclOrBuiltinHeader ::
  (Var v) =>
  HQ.HashQualified Name ->
  DD.DeclOrBuiltin v a ->
  Pretty SyntaxText
prettyDeclOrBuiltinHeader name (DD.Builtin ctype) = case ctype of
  CT.Data -> fmt S.DataTypeKeyword "builtin type " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
  CT.Effect -> fmt S.DataTypeKeyword "builtin ability " <> styleHashQualified'' (fmt $ S.HashQualifier name) name
prettyDeclOrBuiltinHeader name (DD.Decl e) = prettyDeclHeader name e

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = P.withSyntax
