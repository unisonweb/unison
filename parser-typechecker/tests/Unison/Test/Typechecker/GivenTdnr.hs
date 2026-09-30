module Unison.Test.Typechecker.GivenTdnr (test) where

import Control.Monad.State (runStateT)
import Control.Monad.Writer (tell)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..), SiteId (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Result (pattern Result)
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as TC
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      n = Type.nat a :: Type.Type Symbol Ann
      name = Name.unsafeParseText "value"
      ref = Reference.Builtin "value"
      v = Var.named "result"
      env = TC.Env [] (TL.TypeLookup (Map.singleton ref n) Map.empty Map.empty) (Map.singleton name [Right (TC.NamedReference name n (Context.ReplacementRef (Referent.Ref ref)))]) Map.empty Map.empty Map.empty
      source = Term.ann a (Term.resolve a a "value") n
      nodes node = node : foldMap nodes (ABT.out node)
      hasBlank term = any (\case Term.Blank' {} -> True; _ -> False) (nodes term)
      -- Deliberately vary every elaboration note across passes. None of the
      -- initial pass's decisions may survive name substitution and rechecking.
      check e term = do
        typ <- TC.synthesize PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled e term
        let site = if hasBlank term then SiteId 1 else SiteId 2
        tell
          ( mempty
              { TC.infos =
                  Seq.fromList
                    [ Context.ConstraintGoal site 0 a (TV.liftType n) [],
                      Context.DictionaryParameter site a v,
                      Context.GivenBinding site a v (Given.Lexical 0) (TV.liftType n),
                      Context.TopLevelComponent [(v, n, False)]
                    ]
              }
          )
        pure typ
      run e term = runStateT (TC.synthesizeAndResolveWith check PPE.empty e) term
      inspect term action = case run env term of
        Result notes (Just (_, resolved)) -> action (toList (TC.infos notes)) resolved
        Result notes Nothing -> crash (show notes)
      count predicate notes = length (filter predicate notes)
      expectSinglePass notes = do
        expectEqual [site | Context.ConstraintGoal site _ _ _ _ <- notes] [SiteId 2]
        expectEqual [site | Context.DictionaryParameter site _ _ <- notes] [SiteId 2]
        expectEqual [site | Context.GivenBinding site _ _ _ _ <- notes] [SiteId 2]
        expectEqual (count (\case Context.TopLevelComponent {} -> True; _ -> False) notes) 1
   in scope "given-tdnr" $
        tests
          [ scope "recheck-replaces-provisional-decisions" $
              inspect source \notes resolved -> do
                expectSinglePass notes
                expectEqual (count (\case Context.Decision {} -> True; _ -> False) notes) 1
                expect (not (hasBlank resolved)),
            scope "single-pass-does-not-duplicate-decisions" $
              inspect (Term.ann a (Term.ref a ref) n) \notes _ -> expectSinglePass notes,
            scope "unresolved-name-cannot-return-provisional-result" $
              case run (env {TC.termsByShortname = Map.empty}) source of
                Result notes Nothing -> do
                  expect (not (null (TC.errors notes)))
                  expectEqual (count (\case Context.ConstraintGoal {} -> True; _ -> False) (toList (TC.infos notes))) 0
                Result _ (Just _) -> crash "unresolved name was accepted"
          ]
