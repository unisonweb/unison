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
import Unison.Lexer.Pos qualified as Pos
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol (..))
import Unison.Term (Term)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Range qualified as Range
import Unison.Var qualified as Var

analyseTerm :: Lsp.Uri -> Ann -> Term Symbol Ann -> [Diagnostic]
analyseTerm fileUri topLevelTermAnn tm =
  let (unusedVars, _) = ABT.cata alg tm
      -- Unfortunately we don't capture the annotation of the actual binding when parsing :'(, for now the least
      -- annoying thing to do is just highlight the top of the binding.
      mayRange =
        Cv.annToURange topLevelTermAnn
          <&> (\(Range.Range start@(Pos.Pos line _col) _end) -> Range.Range start (Pos.Pos line 9999))
          <&> Cv.uToLspRange
      vars =
        Map.toList unusedVars & mapMaybe \(v, _ann) -> do
          getRelevantVarName v
   in case mayRange of
        Nothing -> []
        Just lspRange ->
          let bindings = Text.intercalate ", " (tShow <$> vars)
           in Monoid.whenM (not $ null vars) [Diagnostic.mkDiagnostic fileUri lspRange Diagnostic.DiagnosticSeverity_Warning ("Unused binding(s) " <> bindings <> " inside this term.\nUse the binding(s), or prefix them with an _ to dismiss this warning.") []]
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
