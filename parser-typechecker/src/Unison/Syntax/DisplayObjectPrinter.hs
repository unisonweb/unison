module Unison.Syntax.DisplayObjectPrinter
  ( prettyTerm,
    prettyType,
    prettyTermDisplayObjects,
    prettyTypeDisplayObjects,
  )
where

import Control.Lens hiding (at)
import Data.List qualified as List
import Data.Map qualified as Map
import U.Codebase.Reference qualified as Reference
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference, TermReferenceId)
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.DisplayObject (DisplayObject (BuiltinObject, MissingObject, UserObject))
import Unison.Syntax.NamePrinter (SyntaxText, prettyHashQualified)
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Pretty qualified as P
import Unison.WatchKind qualified as WK

prettyTypeDisplayObjects ::
  PPED.PrettyPrintEnvDecl ->
  (Map Reference (DisplayObject () (DD.Decl Symbol Ann))) ->
  [P.Pretty SyntaxText]
prettyTypeDisplayObjects pped types =
  types
    & Map.toList
    & map (\(ref, dt) -> (PPE.typeName unsuffixifiedPPE ref, ref, dt))
    & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
    & map (prettyType pped)
  where
    unsuffixifiedPPE = PPED.unsuffixifiedPPE pped

prettyTermDisplayObjects ::
  PPED.PrettyPrintEnvDecl ->
  Bool ->
  (TermReferenceId -> Bool) ->
  (Map Reference.TermReference (DisplayObject (Type Symbol Ann) (Term Symbol Ann))) ->
  [P.Pretty SyntaxText]
prettyTermDisplayObjects pped isSourceFile isTest terms =
  terms
    & Map.toList
    & map (\(ref, dt) -> (PPE.termName unsuffixifiedPPE (Referent.Ref ref), ref, dt))
    & List.sortBy (\(n0, _, _) (n1, _, _) -> Name.compareAlphabetical n0 n1)
    & map (\t -> prettyTerm pped isSourceFile (fromMaybe False . fmap isTest . Reference.toId $ (t ^. _2)) t)
  where
    unsuffixifiedPPE = PPED.unsuffixifiedPPE pped

prettyTerm ::
  PPED.PrettyPrintEnvDecl ->
  Bool {- whether we're printing to a source-file or not. -} ->
  Bool {- Whether the term is a test -} ->
  (HQ.HashQualified Name, Reference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)) ->
  P.Pretty SyntaxText
prettyTerm pped isSourceFile isTest (n, r, dt) =
  case dt of
    MissingObject r -> missingDefinitionMsg n r
    BuiltinObject typ ->
      commentBuiltin $
        P.hang
          ("builtin " <> prettyHashQualified n <> " :")
          (TypePrinter.prettySyntax (ppeBody n r) typ)
    UserObject tm ->
      if isTest
        then WK.TestWatch <> "> " <> TermPrinter.prettyBindingWithoutTypeSignature (ppeBody n r) n tm
        else TermPrinter.prettyBinding (ppeBody n r) n tm
  where
    commentBuiltin txt =
      if isSourceFile
        then P.indent "-- " txt
        else txt
    ppeBody n r = PPE.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPE pped r

prettyType :: PPED.PrettyPrintEnvDecl -> (HQ.HashQualified Name, Reference, DisplayObject () (DD.Decl Symbol Ann)) -> P.Pretty SyntaxText
prettyType pped (n, r, dt) =
  case dt of
    MissingObject r -> missingDefinitionMsg n r
    BuiltinObject _ -> builtin n
    UserObject decl -> DeclPrinter.prettyDecl (PPED.biasTo (maybeToList $ HQ.toName n) $ PPE.declarationPPEDecl pped r) r n decl
  where
    builtin n = P.wrap $ "--" <> prettyHashQualified n <> " is built-in."

missingDefinitionMsg :: HQ.HashQualified Name -> ShortHash -> P.Pretty SyntaxText
missingDefinitionMsg n r =
  P.wrap
    ( "-- The name "
        <> prettyHashQualified n
        <> " is assigned to the "
        <> "reference "
        <> fromString (show r ++ ",")
        <> "which is missing from the codebase."
    )
    <> P.newline
    <> tip "You might need to repair the codebase manually."
  where
    tip :: P.Pretty SyntaxText -> P.Pretty SyntaxText
    tip s = P.column2 [("Tip:", P.wrap s)]
