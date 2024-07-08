module Unison.LSP.FileAnalysis.UnusedBindings where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Types (Diagnostic)
import Language.LSP.Protocol.Types qualified as Lsp
import U.Core.ABT (ABT (..))
import U.Core.ABT qualified as ABT
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.Diagnostics qualified as Diagnostic
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol (..))
import Unison.Term (Term)
import Unison.Var qualified as Var

analyseTerm :: Lsp.Uri -> Term Symbol Ann -> [Diagnostic]
analyseTerm fileUri tm =
  let (unusedVars, _) = ABT.cata alg tm
      vars =
        Map.toList unusedVars & mapMaybe \(v, ann) -> do
          (,ann) <$> getRelevantVarName v
      diagnostics =
        vars & mapMaybe \(varName, ann) -> do
          lspRange <- Cv.annToRange ann
          pure $ Diagnostic.mkDiagnostic fileUri lspRange Diagnostic.DiagnosticSeverity_Warning ("Unused binding " <> varName <> ". Use the binding, or prefix it with an _ to dismiss this warning.") []
   in diagnostics
  where
    getRelevantVarName :: Symbol -> Maybe Text
    getRelevantVarName = \case
      -- Sometimes 'do' gets a binding of '()', which we don't care about
      Symbol _ (Var.User "()") -> Nothing
      Symbol _ (Var.User "") -> Nothing
      -- We only care about user bindings which don't start with an underscore
      Symbol _ (Var.User n) -> do
        guard (not (Text.isPrefixOf "_" n))
        Just n
      _ -> Nothing
    alg :: (Foldable f, Ord v) => Ann -> ABT f v (Map v Ann, Set v) -> (Map v Ann, Set v)
    alg ann abt = case abt of
      Var v -> (mempty, Set.singleton v)
      Cycle x -> x
      Abs v (unusedBindings, usedVars) ->
        if v `Set.member` usedVars
          then (unusedBindings, Set.delete v usedVars)
          else (Map.insert v ann unusedBindings, usedVars)
      Tm fx -> Foldable.fold fx
