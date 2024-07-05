module Unison.LSP.FileAnalysis.UnusedBindings (analyseTerm) where

import Control.Lens
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
import Unison.Term qualified as Term

analyseTerm :: Lsp.Uri -> Term Symbol Ann -> [Diagnostic]
analyseTerm fileUri tm =
  let (unusedVars, _) = ABT.para alg tm
   in unusedVars & mapMaybe \(v, ann) -> do
        range <- Cv.annToRange ann
        pure $ Diagnostic.mkDiagnostic fileUri range Diagnostic.DiagnosticSeverity_Warning ("Unused binding " <> tShow v) []
  where
    alg :: (Ord v) => Ann -> ABT (Term.F v a a) v (Term v Ann, ([(v, Ann)], Set v)) -> ([(v, Ann)], Set v)
    alg ann abt = case abt of
      Var v -> (mempty, Set.singleton v)
      Cycle (_t, x) -> x
      Abs v (_, (unusedBindings, usedVars)) ->
        if v `Set.member` usedVars
          then (unusedBindings, Set.delete v usedVars)
          else ((v, ann) : unusedBindings, usedVars)
      Tm fx -> case fx of
        Term.Let _isTop (lhsTerm, lx) (_rhsTerm, rx) ->
          set (_1 . _head . _2) (ABT.annotation lhsTerm) lx <> rx
        _ -> foldOf (folded . _2) fx
