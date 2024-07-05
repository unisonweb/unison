module Unison.LSP.FileAnalysis.UnusedBindings where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LSP.Protocol.Types (Diagnostic)
import Language.LSP.Protocol.Types qualified as Lsp
import U.Core.ABT (ABT (..))
import U.Core.ABT qualified as ABT
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.Diagnostics qualified as Diagnostic
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.Term (Term)

analyseTerm :: Lsp.Uri -> Term Symbol Ann -> [Diagnostic]
analyseTerm fileUri tm =
  let (unusedVars, _) = ABT.cata alg tm
   in Map.toList unusedVars & mapMaybe \(v, ann) -> do
        range <- Cv.annToRange ann
        pure $ Diagnostic.mkDiagnostic fileUri range Diagnostic.DiagnosticSeverity_Warning ("Unused binding " <> tShow v) []
  where
    alg :: (Foldable f, Ord v) => Ann -> ABT f v (Map v Ann, Set v) -> (Map v Ann, Set v)
    alg ann abt = case abt of
      Var v -> (mempty, Set.singleton v)
      Cycle x -> x
      Abs v (unusedBindings, usedVars) ->
        if v `Set.member` usedVars
          then (mempty, Set.delete v usedVars)
          else (Map.insert v ann unusedBindings, usedVars)
      Tm fx -> Foldable.fold fx
